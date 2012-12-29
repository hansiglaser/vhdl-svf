-------------------------------------------------------------------------------
-- SVF (Serial Vector Format) interpreter to control a JTAG TAP
--
-- Author: Johann Glaser
-- Date:   2012-12-29
--
-------------------------------------------------------------------------------
--
-- The SVF format specifies JTAG bus operations.
--
-- Specification:
--  - http://en.wikipedia.org/wiki/Serial_Vector_Format
--  - http://www.asset-intertech.com/Media/en-US/Documents/Boundary-Scan-Test/Serial_Vector_Format-SVF.pdf
--
-------------------------------------------------------------------------------
--
-- This package provides the procedure ProcessSVF() which parses an SVF
-- file and performs the specified JTAG bus operations. Usage scenarios
-- are testing chip designs with integrated JTAG TAPs.
--
-- SVF Properties:
--  - SVF is an ASCII text file.
--  - The maximum number of characters allowed on a line is 256.
--  - Although one SVF statement can span more than one line.
--  - Each statement consists of a command and associated parameters.
--  - Each SVF statement is terminated by a semicolon.
--  - SVF is not case sensitive.
--  - Comments can be inserted into a SVF file after an exclamation point '!'
--    or a pair of slashes '//'. Either '//' or '!' will comment out the
--    remainder of the line.
--
-- Scan Data:
--  - Scan data within a statement is expressed as hexadecimal and is always
--    enclosed in paranthesis (e.g. "SIR 8 TDI (41);")
--  - This scan data cannot specify more bits than given by the length
--    parameter.
--  - Although, if the MSBs are '0', that is ok.
--  - LSB is first scanned in (TDI, SMASK) and scanned out (TDO, MASK).
--
-- Scan Operation:
--  - A scan operation is initiated by the commands SIR and SDR.
--  - Additionally, headers and trailers as given by the commands
--    HIR, HDR, TIR, TDR are scanned.
--  - The optional command parameters "TDI", "MASK" and "SMASK" are sticky,
--    i.e., they are remembered from the previous command until changed or
--    invalidated).
--  - These are remembered separately for the SIR, SDR, HIR, HDR, TIR
--    and TDR commands.
--
-- Implementation Details:
--  - If the bits which were read beak do not match the specified
--    values, an error is reported, which severity is "error". The
--    execution will continue in this case, if the simulator allows
--    this severity level.
--  - If the SVF file contains syntax errors, an error is reported
--    which severity is "failure".
--  - Length parameters are parsed to variables of VHDL "integer" type.
--    Therefore the maximum length is 2^31-1 instead of 2^32-1.
--
-- TODO:
--  - improve lexer: use a file of character instead of text, should simplfy
--    the parsing of newlines.
--  - even better improvement: use a string instead of a file, add file
--    as input as helper function
--  - dynamically allocate strings with infinite length in GetToken()
--  - implement commands PIO, PIOMAP
--  - implement SMASK parameter (but first understand what it is meant for)
--
-------------------------------------------------------------------------------
--
-- To simulate using ModelSim or QuestaSim, execute the script make.sh
--   $ ./make.sh
-- which compiles this package and the testbench in tb_svf.vhd and finally
-- creates a Makefile. Then start the simulation with
--   $ ./sim.sh
--
-------------------------------------------------------------------------------
--
-- Usage:
-- ------
--
-- First, include this package in your simulation project or Makefiles.
-- In your testbench, add the lines at the top
--   library work;
--   use work.svf.all;
-- and in the stimulus process, call the procedure ProcessSVF().
--   ProcessSVF("mysvf.svf",TMS,TDI,TCK,TDO);
-- where "mysvf.svf" is your SVF file, and TMS, TDI, TCK and TDO are the
-- signals which are connected to your DUT's JTAG ports.
--
-- See tb_svf.vhd for an example.
--
-------------------------------------------------------------------------------
library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use std.textio.all;
use ieee.std_logic_textio.all;
use ieee.math_real.all;

package svf is

  -----------------------------------------------------------------------------
  -- Read a SVF file and control the JTAG signals accordingly
  --
  procedure ProcessSVF (
    constant Filename : string;
    signal   TMS      : out std_logic;
    signal   TDI      : out std_logic;
    signal   TCK      : out std_logic;
    signal   TDO      : in  std_logic 
  );

  -- default TCK frequency
  constant DefaultFrequency    : real    := 1000000.0;  -- 1 MHz
  -- maximum number of transitions specified by command "STATE"
  constant MaxStateTransitions : integer := 8;

end svf;

package body svf is

  -----------------------------------------------------------------------------
  -- Lexer
  -----------------------------------------------------------------------------

  type PString is access string;

  -----------------------------------------------------------------------------
  -- Record to keep the lexer state
  type TLexerState is record
    Filename : PString;
    -- VHDL does not allow to put a file or text entry here :-(
    L        : line;
    Row      : integer;    -- row (line number)
    Col      : integer;    -- column
    Put      : boolean;    -- set to true if the last char was put back using PutChar()
    LastCh   : character;
    Complete : boolean;    -- true if all chars were consumed
  end record;

  -----------------------------------------------------------------------------
  -- Create an empty TLexerState record
  --
  -- Reads the first line of the file
  impure function StartLexer (
    Filename : string;
    file F   : text
  ) return TLexerState is
    variable State : TLexerState;
  begin  -- StartLexer
    State.Filename := new string'(Filename);
    readline(F,State.L);
    State.Row      := 0;
    State.Col      := 0;
    State.Put      := false;
    State.LastCh   := character'val(0);
    State.Complete := false;
    return State;
  end StartLexer;

  -----------------------------------------------------------------------------
  -- Read one character from the file
  --
  -- If PutChar() was used before, this is returned instead of reading from the
  -- file.
  -- If last character is read and it is called again, State.Complete will be
  -- set to true.
  procedure GetChar (
    file F : text;
    State  : inout TLexerState;
    Ch     : out   character
  ) is
  begin  -- GetChar
    if State.Complete then
      return;
    end if;
    -- return last character if PutChar() was used
    if State.Put then
      State.Put := false;
      Ch := State.LastCh;
      return;
    end if;
    -- If the Line string is already consumed (i.e. length=0) we read the next
    -- line.
    if State.L'length = 0 then
      if EndFile(F) then
        State.Complete := true;
        return;
      end if;
      State.Col := 0;
      State.Row := State.Row + 1;
      readline(F,State.L);
      -- simulate as if we had read a ^J to split identifiers
      State.LastCh := character'val(10);
      Ch := State.LastCh;
      return;
    end if;
    -- normal operation to return a single character
    Read(State.L,State.LastCh);
    Ch := State.LastCh;
    State.Col := State.Col + 1;
  end GetChar;

  -----------------------------------------------------------------------------
  -- Take back one character
  --
  -- If the tokenizer has requested one character too much, it can return it
  -- back to the lexer.
  --
  -- Important: Only 1 character can be taken back!
  procedure PutChar (
    State : inout TLexerState
  ) is
  begin
    assert State.Put = false report "You can't use PutChar twice" severity failure;
    State.Put      := true;
    State.Complete := false;
  end PutChar;

  -----------------------------------------------------------------------------
  -- Skip to the end of line
  procedure Skip2EOL (
    file F : text;
    State  : inout TLexerState
  ) is
  begin  -- Skip2EOL
    if EndFile(F) then
      State.Complete := true;
      return;
    end if;
    if State.Complete then
      return;
    end if;
    State.Col := 0;
    State.Row := State.Row + 1;
    readline(F,State.L);
  end Skip2EOL;

  -----------------------------------------------------------------------------
  -- Consume all whitespace characters
  procedure EatWhitespace (
    file F : text;
    State  : inout TLexerState
  ) is
    variable Ch : character;
  begin  -- EatWhitespace
    while true loop
      GetChar(F,State,Ch);
      if State.Complete then
        return;
      end if;
      if Ch = '!' then
        Skip2EOL(F,State);
      elsif Ch = '/' then
        GetChar(F,State,Ch);
        if Ch = '/' then
          Skip2EOL(F,State);
        else
          PutChar(State);
          assert false report "'/' character followed by something else than a '/' character" severity warning;
          return;
        end if;
      elsif not (Ch <= ' ') then
        PutChar(State);
        return;
      end if;
    end loop;
  end EatWhitespace;

  -----------------------------------------------------------------------------
  -- Small test routine for the lexer
  procedure TestLexer (
    constant Filename : string
  ) is
    file F : text open read_mode is Filename;
    variable State : TLexerState;
    variable Ch    : character;
  begin
    State := StartLexer(Filename,F);
    while not State.Complete loop
      GetChar(F,State,Ch);
      assert false report
          "Col = " & integer'image(State.Col) &
        ", Row = " & integer'image(State.Row) &
        ", Ch = '" & Ch & "'"
        severity note;
    end loop;
  end TestLexer;

  -----------------------------------------------------------------------------
  -- Tokenizer
  -----------------------------------------------------------------------------

  -----------------------------------------------------------------------------
  -- Token information
  type TTokenType is (ttNone,ttEOF,ttIdentifier,ttParOpen,ttParClose,ttSemicolon);

  type TToken is record
    TokenType : TTokenType;
    St        : PString;
    Filename  : PString;
    Row       : integer;                        -- row (line number)
    Col       : integer;                        -- column
  end record;

  -----------------------------------------------------------------------------
  -- Returns a printable representation of the token
  -- 
  -- VHDL unfortunately does not allow to pass a TToken as a single parameter
  -- to a function, therefore we have to pass the TokenType and St separately.
  function Token2String (
    constant TokenType : in TTokenType;
    constant St        : string
  ) return string is
  begin  -- Token2String
    case TokenType is
      when ttNone       => return "NULL";
      when ttEOF        => return "EOF";
      when ttIdentifier => return "Identifier '" & St & "'";
      when ttParOpen    => return "(";
      when ttParClose   => return ")";
      when ttSemicolon  => return ";";
    end case;
    return "ERROR";
  end Token2String;

  -----------------------------------------------------------------------------
  -- Report information on a token
  procedure PrintToken (
    Token : inout TToken
  ) is
  begin
     report
        "Col = " & integer'image(Token.Col) &
      ", Row = " & integer'image(Token.Row) &
      ", Token = " &
      Token2String(Token.TokenType,Token.St.ALL);
  end PrintToken;

  -----------------------------------------------------------------------------
  -- Read one token from the file
  --
  procedure GetToken (
    file F : text;
    State  : inout TLexerState;
    Token  : inout TToken
  ) is
    constant MaxLen  : integer := 10000;
    variable Ch      : character;
    variable St      : string(1 to MaxLen);
    variable StPos   : integer;
  begin
    St    := (others => character'val(0));
    StPos := 1;
    EatWhitespace(F,State);
    Token.St        := new string'("");
    Token.Filename  := State.Filename;
    Token.Row       := State.Row;
    Token.Col       := State.Col-1;
    -- return ttEOF
    Token.TokenType := ttEOF;
    if State.Complete then
      return;
    end if;
    -- ok some normal token
    Token.TokenType := ttNone;
    GetChar(F,State,Ch);
    if Ch = '(' then
      Token.TokenType := ttParOpen;
      return;
    elsif Ch = ')' then
      Token.TokenType := ttParClose;
      return;
    elsif Ch = ';' then
      Token.TokenType := ttSemicolon;
      return;
    else
      Token.TokenType := ttIdentifier;
      -- identifier, read until its end
      while true loop
        St(StPos) := Ch;
        StPos := StPos + 1;
        GetChar(F,State,Ch);
        if State.Complete or
           not (((Ch >= '0') and (Ch <= '9')) or
                ((Ch >= 'A') and (Ch <= 'Z')) or
                ((Ch >= 'a') and (Ch <= 'z')) or
                (Ch = '.') or (Ch = '-') or (Ch = '+')) then
          PutChar(State);
          Token.St := new string'(St(1 to StPos-1));
          return;
        end if;
      end loop;
      -- EOF reached, but this should never happen
      Token.TokenType := ttEOF;
    end if;
  end GetToken;

  -----------------------------------------------------------------------------
  -- Print a parser warning including filename, row and column
  procedure ParserWarning (
    Token     : inout TToken;
    Msg       : in string
  ) is
  begin
    assert false
      report "WARNING: " & Token.Filename.ALL & " (" &
        integer'image(Token.Row+1) & ":" & 
        integer'image(Token.Col+1) & "): " &
        Msg
      severity warning;
  end ParserWarning;

  -----------------------------------------------------------------------------
  -- Print a parser error including filename, row and column
  procedure ParserError (
    Token     : inout TToken;
    Msg       : in string
  ) is
  begin
    assert false
      report "ERROR: " & Token.Filename.ALL & " (" &
        integer'image(Token.Row+1) & ":" & 
        integer'image(Token.Col+1) & "): " &
        Msg
      severity failure;
  end ParserError;

  -----------------------------------------------------------------------------
  -- Print a match error including filename, row and column
  procedure MatchError (
    Token     : inout TToken;
    Msg       : in string
  ) is
  begin
    assert false
      report "MATCH ERROR: " & Token.Filename.ALL & " (" &
        integer'image(Token.Row+1) & ":" & 
        integer'image(Token.Col+1) & "): " &
        Msg
      severity error;
  end MatchError;

  -----------------------------------------------------------------------------
  -- Read a token and check its type
  procedure RequireToken(
    file F    : text;
    State     : inout TLexerState;
    Token     : inout TToken;
    TokenType : in TTokenType
  ) is
  begin -- RequireToken
    GetToken(F,State,Token);
    if Token.TokenType /= TokenType then
      ParserError(Token,
        "Required token type is " &
        Token2String(TokenType,"") &
        " but " &
        Token2String(Token.TokenType,Token.St.ALL) &
        " found.");
    end if;
  end RequireToken;

  -----------------------------------------------------------------------------
  -- Parse a token as an integer
  procedure TryParseToken (
    Token     : inout TToken;
    IsOk      : out   boolean;
    Int       : inout integer           -- using "inout" so that the value is not overwritten on an error
  ) is
    variable Ch : character;
  begin -- TryParseToken
    IsOk := false;
    -- check valid characters
    for Pos in Token.St.all'length downto 1 loop
      Ch := Token.St.all(Pos);
      if not ((Ch >= '0') and (Ch <= '9')) then
        return;
      end if;
    end loop;
    Int := integer'value(Token.St.ALL);
    IsOk := true;
  end TryParseToken;

  -----------------------------------------------------------------------------
  -- Parse a token as an integer, error on invalid integer
  procedure ParseToken (
    Token     : inout TToken;
    Int       : inout integer
  ) is
    variable IsOk : boolean;
  begin -- ParseToken
    TryParseToken(Token,IsOk,Int);
    if not IsOk then
      ParserError(Token,
        Token2String(Token.TokenType,Token.St.ALL) &
        " is an invalid real.");
    end if;
  end ParseToken;

  -----------------------------------------------------------------------------
  -- Read a token and convert to an integer, error on invalid integer
  procedure RequireToken(
    file F    : text;
    State     : inout TLexerState;
    Token     : inout TToken;
    Int       : inout integer
  ) is
  begin -- RequireToken
    RequireToken(F,State,Token,ttIdentifier);
    ParseToken(Token,Int);
  end RequireToken;

  -----------------------------------------------------------------------------
  -- Parse a token as a real
  --
  -- Format: "digits [ . digits ] [ E [+|-] digits ]"
  procedure TryParseToken (
    Token     : inout TToken;
    IsOk      : out   boolean;
    R         : inout real              -- using "inout" so that the value is not overwritten on an error
  ) is
    type TRealState is (rsInt,rsFrac,rsExp);
    variable State : TRealState;
    variable Result : real;
    variable NumFrac : integer;
    variable Exp : integer;
    variable ExpNeg : boolean;
    variable Ch : character;
  begin -- TryParseToken
    IsOk := false;
    State := rsInt;
    Result := 0.0;
    NumFrac := 0;                       -- count digits after '.'
    Exp := 0;
    ExpNeg := false;
    -- check valid characters
    for Pos in 1 to Token.St.all'length loop
      Ch := Token.St.all(Pos);
      case State is
        when rsInt =>
          if (Ch >= '0') and (Ch <= '9') then
            Result := Result * 10.0;
            Result := Result + real(character'pos(Ch) - character'pos('0'));
          elsif Ch = '.' then
            State := rsFrac;
          elsif Ch = 'E' then
            State := rsExp;
          else
            -- error
            return;
          end if;
        when rsFrac =>
          if (Ch >= '0') and (Ch <= '9') then
            Result := Result * 10.0;
            Result := Result + real(character'pos(Ch) - character'pos('0'));
            NumFrac := NumFrac + 1;
          elsif Ch = 'E' then
            State := rsExp;
          else
            -- error
            return;
          end if;
        when rsExp =>
          if (Ch >= '0') and (Ch <= '9') then
            Exp := Exp * 10 + (character'pos(Ch) - character'pos('0'));
          elsif Ch = '+' then
            ExpNeg := false;
          elsif Ch = '-' then
            ExpNeg := true;
          else
            -- error
            return;
          end if;
        when others =>
          return;
      end case;
    end loop;
    if ExpNeg then
      Exp := - Exp;
    end if;
    Result := Result * (0.1 ** NumFrac) * (10.0 ** Exp);
    R := Result; -- real'value(Token.St.ALL);
    IsOk := true;
  end TryParseToken;

  -----------------------------------------------------------------------------
  -- Parse a token as a real, error on invalid real
  procedure ParseToken (
    Token     : inout TToken;
    R         : inout real
  ) is
    variable IsOk : boolean;
  begin -- ParseToken
    TryParseToken(Token,IsOk,R);
    if not IsOk then
      ParserError(Token,
        Token2String(Token.TokenType,Token.St.ALL) &
        " is an invalid real.");
    end if;
  end ParseToken;

  -----------------------------------------------------------------------------
  -- Read a token and convert to a real, error on invalid real
  procedure RequireToken(
    file F    : text;
    State     : inout TLexerState;
    Token     : inout TToken;
    R         : inout real
  ) is
  begin -- RequireToken
    RequireToken(F,State,Token,ttIdentifier);
    ParseToken(Token,R);
  end RequireToken;

  -----------------------------------------------------------------------------
  -- Test the tokenizer and report all tokens
  procedure TestTokenizer (
    constant Filename : in string
  ) is
    file F : text open read_mode is Filename;
    variable State : TLexerState;
    variable Token : TToken;
  begin  -- TestTokenizer
    State := StartLexer(Filename,F);
    while not State.Complete loop
      GetToken(F,State,Token);
      assert false report
          "Col = " & integer'image(Token.Col) &
        ", Row = " & integer'image(Token.Row) &
        ", Token = " &
        Token2String(Token.TokenType,Token.St.ALL)
        severity note;
    end loop;
  end TestTokenizer;

  -----------------------------------------------------------------------------
  -- SVF Parser
  -----------------------------------------------------------------------------

  -- Modes setup by the TRST command
  type TTRST_Mode is (tmOn,tmOff,tmZ,tmAbsent);
  -- JTAG states
  type TJTAGState is (jsUnknown,jsReset,jsIdle,jsDRSelect,jsDRCapture,jsDRShift,jsDRExit1,jsDRPause,jsDRExit2,jsDRUpdate,
                                               jsIRSelect,jsIRCapture,jsIRShift,jsIRExit1,jsIRPause,jsIRExit2,jsIRUpdate);
  type TJTAGStateArr is array (natural range <>) of TJTAGState;
  type PStdLogicVector is access std_logic_vector;

  function JTAGStateName (
    constant JTAGState : TJTAGState)
    return string is
  begin  -- JTAGStateName
    case JTAGState is
      when jsUnknown   => return "UNKNOWN";
      when jsReset     => return "RESET";
      when jsIdle      => return "IDLE";
      when jsDRSelect  => return "DRSELECT";
      when jsDRCapture => return "DRCAPTURE";
      when jsDRShift   => return "DRSHIFT";
      when jsDRExit1   => return "DREXIT1";
      when jsDRPause   => return "DRPAUSE";
      when jsDRExit2   => return "DREXIT2";
      when jsDRUpdate  => return "DRUPDATE";
      when jsIRSelect  => return "IRSELECT";
      when jsIRCapture => return "IRCAPTURE";
      when jsIRShift   => return "IRSHIFT";
      when jsIRExit1   => return "IREXIT1";
      when jsIRPause   => return "IRPAUSE";
      when jsIRExit2   => return "IREXIT2";
      when jsIRUpdate  => return "IRUPDATE";
      when others      => return "INVALID";
    end case;
  end JTAGStateName;

  -----------------------------------------------------------------------------
  -- Check if a state is a stable state
  function IsStableState (
    constant State : TJTAGState
  ) return boolean is
  begin  -- IsStableState
    if State = jsIRPause or
       State = jsDRPause or
       State = jsReset or
       State = jsIdle then
      return true;
    end if;
    return false;
  end IsStableState;

  -----------------------------------------------------------------------------
  -- Get TMS value for direct transition
  function GetTMS (
    constant FromState : TJTAGState;
    constant ToState   : TJTAGState
  )
    return std_logic is
  begin  -- GetTMS
    if    (FromState = jsUnknown)   and (ToState = jsReset)     then return '1';
    elsif (FromState = jsReset)     and (ToState = jsReset)     then return '1';
    elsif (FromState = jsReset)     and (ToState = jsIdle)      then return '0';
    elsif (FromState = jsIdle)      and (ToState = jsIdle)      then return '0';
    elsif (FromState = jsIdle)      and (ToState = jsDRSelect)  then return '1';
    -- Data Register
    elsif (FromState = jsDRSelect)  and (ToState = jsDRCapture) then return '0';
    elsif (FromState = jsDRSelect)  and (ToState = jsIRSelect)  then return '1';
    elsif (FromState = jsDRCapture) and (ToState = jsDRShift)   then return '0';
    elsif (FromState = jsDRCapture) and (ToState = jsDRExit1)   then return '1';
    elsif (FromState = jsDRShift)   and (ToState = jsDRShift)   then return '0';
    elsif (FromState = jsDRShift)   and (ToState = jsDRExit1)   then return '1';
    elsif (FromState = jsDRExit1)   and (ToState = jsDRPause)   then return '0';
    elsif (FromState = jsDRExit1)   and (ToState = jsDRUpdate)  then return '1';
    elsif (FromState = jsDRPause)   and (ToState = jsDRPause)   then return '0';
    elsif (FromState = jsDRPause)   and (ToState = jsDRExit2)   then return '1';
    elsif (FromState = jsDRExit2)   and (ToState = jsDRShift)   then return '0';
    elsif (FromState = jsDRExit2)   and (ToState = jsDRUpdate)  then return '1';
    elsif (FromState = jsDRUpdate)  and (ToState = jsDRSelect)  then return '1';
    elsif (FromState = jsDRUpdate)  and (ToState = jsIdle)      then return '0';
    -- Instruction Register
    elsif (FromState = jsIRSelect)  and (ToState = jsIRCapture) then return '0';
    elsif (FromState = jsIRSelect)  and (ToState = jsReset)     then return '1';
    elsif (FromState = jsIRCapture) and (ToState = jsIRShift)   then return '0';
    elsif (FromState = jsIRCapture) and (ToState = jsIRExit1)   then return '1';
    elsif (FromState = jsIRShift)   and (ToState = jsIRShift)   then return '0';
    elsif (FromState = jsIRShift)   and (ToState = jsIRExit1)   then return '1';
    elsif (FromState = jsIRExit1)   and (ToState = jsIRPause)   then return '0';
    elsif (FromState = jsIRExit1)   and (ToState = jsIRUpdate)  then return '1';
    elsif (FromState = jsIRPause)   and (ToState = jsIRPause)   then return '0';
    elsif (FromState = jsIRPause)   and (ToState = jsIRExit2)   then return '1';
    elsif (FromState = jsIRExit2)   and (ToState = jsIRShift)   then return '0';
    elsif (FromState = jsIRExit2)   and (ToState = jsIRUpdate)  then return '1';
    elsif (FromState = jsIRUpdate)  and (ToState = jsDRSelect)  then return '1';
    elsif (FromState = jsIRUpdate)  and (ToState = jsIdle)      then return '0';
    else
      assert false report "Invalid state transition from " &
        JTAGStateName(FromState) & " to " & JTAGStateName(ToState)
        severity error;
      return 'U';
    end if;
  end GetTMS;

  -----------------------------------------------------------------------------
  -- Get TMS value for transitions from and to a stable state
  function GetTMSStable (
    constant FromState : TJTAGState;
    constant ToState   : TJTAGState
  )
    return std_logic_vector is
  begin  -- GetTMSStable
    if    (FromState = jsUnknown)   and (ToState = jsReset)     then return "111111";
--    elsif (FromState = jsUnknown)   and (ToState = jsIdle)      then return "1111110";
--    elsif (FromState = jsUnknown)   and (ToState = jsDRPause)   then return "11111101010";
--    elsif (FromState = jsUnknown)   and (ToState = jsIRPause)   then return "111111011010";
    elsif (FromState = jsReset)     and (ToState = jsReset)     then return "1";
    elsif (FromState = jsReset)     and (ToState = jsIdle)      then return "0";
    elsif (FromState = jsReset)     and (ToState = jsDRPause)   then return "01010";
    elsif (FromState = jsReset)     and (ToState = jsIRPause)   then return "011010";
    elsif (FromState = jsIdle)      and (ToState = jsReset)     then return "1111";
    elsif (FromState = jsIdle)      and (ToState = jsIdle)      then return "0";
    elsif (FromState = jsIdle)      and (ToState = jsDRPause)   then return "1010";
    elsif (FromState = jsIdle)      and (ToState = jsIRPause)   then return "11010";
    elsif (FromState = jsDRPause)   and (ToState = jsReset)     then return "11111";
    elsif (FromState = jsDRPause)   and (ToState = jsIdle)      then return "110";
    elsif (FromState = jsDRPause)   and (ToState = jsDRPause)   then return "111010";
    elsif (FromState = jsDRPause)   and (ToState = jsIRPause)   then return "1111010";
    elsif (FromState = jsIRPause)   and (ToState = jsReset)     then return "11111";
    elsif (FromState = jsIRPause)   and (ToState = jsIdle)      then return "110";
    elsif (FromState = jsIRPause)   and (ToState = jsDRPause)   then return "111010";
    elsif (FromState = jsIRPause)   and (ToState = jsIRPause)   then return "1111010";
    else
      assert false report "Invalid state transition from " &
        JTAGStateName(FromState) & " to " & JTAGStateName(ToState)
        severity error;
      return "U";
    end if;
  end GetTMSStable;

  -----------------------------------------------------------------------------
  -- Get TMS value for transitions from a stable state to a shift state and back
  function GetTMSShift (
    constant FromState : TJTAGState;
    constant ToState   : TJTAGState
  )
    return std_logic_vector is
  begin  -- GetTMSShift
    -- stable state to shift
    if    (FromState = jsReset)     and (ToState = jsDRShift)   then return "0100";
    elsif (FromState = jsReset)     and (ToState = jsIRShift)   then return "01100";
    elsif (FromState = jsIdle)      and (ToState = jsDRShift)   then return "100";
    elsif (FromState = jsIdle)      and (ToState = jsIRShift)   then return "1100";
    elsif (FromState = jsDRPause)   and (ToState = jsDRShift)   then return "10";
    elsif (FromState = jsDRPause)   and (ToState = jsIRShift)   then return "111100";
    elsif (FromState = jsIRPause)   and (ToState = jsDRShift)   then return "11100";
    elsif (FromState = jsIRPause)   and (ToState = jsIRShift)   then return "10";
    -- shift to stable state (jsDRShift to jsDRExit1 (TMS=1) is already performed by Scan())
    elsif (FromState = jsDRShift)   and (ToState = jsReset)     then return "1111";
    elsif (FromState = jsDRShift)   and (ToState = jsIdle)      then return "10";
    elsif (FromState = jsDRShift)   and (ToState = jsDRPause)   then return "0";
    elsif (FromState = jsDRShift)   and (ToState = jsIRPause)   then return "111010";
    elsif (FromState = jsIRShift)   and (ToState = jsReset)     then return "1111";
    elsif (FromState = jsIRShift)   and (ToState = jsIdle)      then return "10";
    elsif (FromState = jsIRShift)   and (ToState = jsDRPause)   then return "11010";
    elsif (FromState = jsIRShift)   and (ToState = jsIRPause)   then return "0";
    else
      assert false report "Invalid state transition from " &
        JTAGStateName(FromState) & " to " & JTAGStateName(ToState)
        severity error;
      return "U";
    end if;
  end GetTMSShift;

  -----------------------------------------------------------------------------
  -- Parse a token to a JTAG state
  procedure TryParseToken (
    Token     : inout TToken;
    IsOk      : out   boolean;
    JTAGState : inout TJTAGState        -- using "inout" so that the value is not overwritten on an error
  ) is
  begin -- TryParseToken
    IsOk := true;
    if    Token.St.all = "RESET" then
      JTAGState := jsReset;
    elsif Token.St.all = "IDLE" then
      JTAGState := jsIdle;
    elsif Token.St.all = "DRSELECT" then
      JTAGState := jsDRSelect;
    elsif Token.St.all = "DRCAPTURE" then
      JTAGState := jsDRCapture;
    elsif Token.St.all = "DRSHIFT" then
      JTAGState := jsDRShift;
    elsif Token.St.all = "DREXIT1" then
      JTAGState := jsDRExit1;
    elsif Token.St.all = "DRPAUSE" then
      JTAGState := jsDRPause;
    elsif Token.St.all = "DREXIT2" then
      JTAGState := jsDRExit2;
    elsif Token.St.all = "DRUPDATE" then
      JTAGState := jsDRUpdate;
    elsif Token.St.all = "IRSELECT" then
      JTAGState := jsIRSelect;
    elsif Token.St.all = "IRCAPTURE" then
      JTAGState := jsIRCapture;
    elsif Token.St.all = "IRSHIFT" then
      JTAGState := jsIRShift;
    elsif Token.St.all = "IREXIT1" then
      JTAGState := jsIRExit1;
    elsif Token.St.all = "IRPAUSE" then
      JTAGState := jsIRPause;
    elsif Token.St.all = "IREXIT2" then
      JTAGState := jsIRExit2;
    elsif Token.St.all = "IRUPDATE" then
      JTAGState := jsIRUpdate;
    else
      IsOk := false;
    end if;
  end TryParseToken;

  -----------------------------------------------------------------------------
  -- Parse a token to a JTAG state, error on invalid JTAG state
  procedure ParseToken (
    Token     : inout TToken;
    JTAGState : inout TJTAGState
  ) is
    variable IsOk : boolean;
  begin -- ParseToken
    TryParseToken(Token,IsOk,JTAGState);
    if not IsOk then
      ParserError(Token,"JTAG state required but found '" & Token.St.all & "'");
    end if;
  end ParseToken;

  -----------------------------------------------------------------------------
  -- Read a token and convert it to a JTAG state
  procedure RequireToken(
    file F    : text;
    State     : inout TLexerState;
    Token     : inout TToken;
    JTAGState : inout TJTAGState
  ) is
  begin -- RequireToken
    RequireToken(F,State,Token,ttIdentifier);
    ParseToken(Token,JTAGState);
  end RequireToken;

  -----------------------------------------------------------------------------
  -- Data as used by HDR, HIR, SDR, SIR, TDR and TIR commands
  type TScanData is record
    Length : integer;
    TDI    : PStdLogicVector;
    TDO    : PStdLogicVector;
    MASK   : PStdLogicVector;
    SMASK  : PStdLogicVector;
  end record;

  -----------------------------------------------------------------------------
  -- Helper function: return smaller of two numbers
  function min(L, R: INTEGER) return INTEGER is
  begin
    if L < R then
      return L;
    else
      return R;
    end if;
  end;

  -----------------------------------------------------------------------------
  -- Helper function: return larger of two numbers
  function max(L, R: INTEGER) return INTEGER is
  begin
    if L > R then
      return L;
    else
      return R;
    end if;
  end;

  -----------------------------------------------------------------------------
  -- Helper function: Return integer value of hex digit
  --
  -- For an invalid character -1 is returned.
  function Ch2Hex (
    Ch    : character
  )
    return integer is
  begin  -- Ch2Hex
    if    (Ch >= '0') and (Ch <= '9') then
      return character'pos(Ch) - character'pos('0');
    elsif (Ch >= 'A') and (Ch <= 'F') then
      return character'pos(Ch) - character'pos('A') + 10;
    elsif (Ch >= 'a') and (Ch <= 'f') then
      return character'pos(Ch) - character'pos('a') + 10;
    else
      return -1;
    end if;
  end Ch2Hex;

  -----------------------------------------------------------------------------
  -- Helper function to convert a std_logic_vector to a string
  --
  -- Copied from ~/devel/snops/projects/chll/Testchip/src/vhdl/vhdl93_packs/tb/tbfuncs-p.vhd
  function Vector2String(constant V : in std_logic_vector) return string is
    variable i : integer;
    variable s : string(1 to V'length);
  begin
    s := (others => 'r');
    for i in V'high downto V'low loop
      case V(i) is
        when '0'    => s(V'high-i+1) := '0';
        when '1'    => s(V'high-i+1) := '1';
        when 'U'    => s(V'high-i+1) := 'U';
        when 'X'    => s(V'high-i+1) := 'X';
        when others => s(V'high-i+1) := '?';
      end case;
    end loop;
    return s;
  end Vector2String;

  -----------------------------------------------------------------------------
  -- Read a hex vector in paranthesis
  --
  -- Example: "(12AB)"
  --
  -- don't forget to deallocate(Vector)!
  procedure ParseHexVector (
    file F    : text;
    State     : inout TLexerState;
    Token     : inout TToken;
    Length    : in  integer;
    VectorOut : out PStdLogicVector
  ) is
    variable BitPos : integer;
    variable Ch     : character;
    variable HexInt : integer;
    variable HexVal : std_logic_vector(3 downto 0);
    variable Width  : integer;
    variable St,StO : PString;
    variable Vector : PStdLogicVector;
  begin  -- ParseHexVector
    RequireToken(F,State,Token,ttParOpen);
    -- allocate data vector of correct length
    Vector := new std_logic_vector(Length-1 downto 0);
    -- fill with 0s ((others=>'0') can not be used in unconstrained array aggregate)
    for Pos in Length-1 downto 0 loop
      Vector.all(Pos) := '0';
    end loop;  -- Pos
    -- read Hex digits
    St := new string'("");
    while true loop
      -- get new token
      GetToken(F,State,Token);
      if Token.TokenType = ttParClose then
        exit;
      elsif Token.TokenType /= ttIdentifier then
        ParserError(Token,"Invalid token " & Token2String(Token.TokenType,Token.St.ALL));
      end if;
      -- append to to total string
      StO := St;
      St := new string'(St.all & Token.St.all);
      deallocate(StO);
    end loop;
    -- iterate from last char up to first char
    BitPos := 0;
    for Pos in St.all'length downto 1 loop
      Ch := St.all(Pos);
      HexInt := Ch2Hex(Ch);
      if HexInt < 0 then
        ParserError(Token,"Invalid hex digit '" & Ch & "' at position " & integer'image(Pos) & " of hexadecimal scan data");
      end if;
      HexVal := conv_std_logic_vector(HexInt,4);
      Width := min(4,Length-BitPos);
      Vector.all(BitPos+Width-1 downto BitPos) := HexVal(Width-1 downto 0);
      BitPos := BitPos + 4;
    end loop;  -- Pos
--    assert false report "Converted hex string '" & St.all & "' to vector " & Vector2String(Vector.all) severity note;
    deallocate(St);
    VectorOut := Vector;
  end ParseHexVector;

  -----------------------------------------------------------------------------
  -- Parse parameters of commands HDR, HIR, SDR, SIR, TDR, TIR
  procedure ParseScanData (
    file F    : text;
    State     : inout TLexerState;
    Token     : inout TToken;
    ScanData  : inout TScanData
  ) is
    variable Length : integer;
  begin  -- ParseScanData
    RequireToken(F,State,Token,Length);
    if ScanData.Length /= Length then
      ScanData.Length := Length;
      -- if length changes, all old values are forgotten
      deallocate(ScanData.TDI);      ScanData.TDI   := null;
      deallocate(ScanData.TDO);      ScanData.TDO   := null;
      deallocate(ScanData.MASK);     ScanData.MASK  := null;
      deallocate(ScanData.SMASK);    ScanData.SMASK := null;
    end if;
    -- if TDO is missing, no comparison will be performed
    deallocate(ScanData.TDO);      ScanData.TDO   := null;
    while not State.Complete loop
      GetToken(F,State,Token);
      if Token.TokenType = ttSemicolon then
        exit;
      elsif Token.TokenType /= ttIdentifier then
        ParserError(Token,"Invalid token " & Token2String(Token.TokenType,Token.St.ALL));
      end if;
      -- ttIdentifier (each parameter is allowed only once, but we don't care here)
      if    Token.St.all = "TDI" then
        ParseHexVector(F,State,Token,Length,ScanData.TDI);
      elsif Token.St.all = "TDO" then
        ParseHexVector(F,State,Token,Length,ScanData.TDO);
      elsif Token.St.all = "MASK" then
        ParseHexVector(F,State,Token,Length,ScanData.MASK);
      elsif Token.St.all = "SMASK" then
        ParseHexVector(F,State,Token,Length,ScanData.SMASK);
      else
        ParserError(Token,"Unknown identifier '" & Token.St.all & "'");
      end if;
    end loop;
    if (ScanData.Length > 0) and (ScanData.TDI = null) then
      ParserError(Token,"At least TDI is required");
    end if;
  end ParseScanData;

  procedure ClockTCK (
    constant HalfPeriode : in  time;
    signal   TCK         : out std_logic
  ) is
  begin  -- ClockTCK
    TCK <= '1';
    wait for HalfPeriode;
    TCK <= '0';
    wait for HalfPeriode;
  end ClockTCK;

  function GetFrequency (
    constant Frequency : in  real
  ) return real is
  begin  -- GetFrequency
    if Frequency > 0.0 then
      return Frequency;
    else
      return DefaultFrequency;
    end if;
  end GetFrequency;

  function GetHalfPeriode (
    constant Frequency : in  real
  ) return time is
  begin  -- GetHalfPeriode
    return 0.5 sec / GetFrequency(Frequency);
  end GetHalfPeriode;

  procedure ScanTMS (
    constant Vector    : in  std_logic_vector;
    constant Frequency : in  real;
    signal   TMS       : out std_logic;
    signal   TCK       : out std_logic
  ) is
    variable HalfPeriode : time;
  begin  -- ScanTMS
    HalfPeriode := GetHalfPeriode(Frequency);
    for i in 0 to Vector'length-1 loop
      TMS <= Vector(i);
      wait for 1 ns;
      TCK <= '1';
      wait for HalfPeriode - 1 ns;
      TCK <= '0';
      wait for HalfPeriode;
    end loop;  -- i
  end ScanTMS;

  procedure Scan (
    variable ScanIn    : in  std_logic_vector;
    variable ScanOut   : out std_logic_vector;
    constant Frequency : in  real;
    constant GotoExit1 : in  boolean;
    signal   TMS       : out std_logic;
    signal   TDI       : out std_logic;
    signal   TCK       : out std_logic;
    signal   TDO       : in  std_logic 
  ) is
    variable HalfPeriode : time;
  begin  -- Scan
    HalfPeriode := GetHalfPeriode(Frequency);
    TMS <= '0';                         -- stay in jsDRShift or jsIRShift
    for i in 0 to ScanIn'length-1 loop
      if GotoExit1 and (i = ScanIn'length-1) then
        TMS <= '1';                     -- jsDRShift to jsDRExit1
      end if;
      TDI <= ScanIn(i);
      wait for 1 ns;
      TCK <= '1';
      wait for HalfPeriode - 1 ns;
      TCK <= '0';
      ScanOut(i) := TDO;
      wait for HalfPeriode;
    end loop;  -- i
  end Scan;

  procedure Scan (
    variable Token     : inout TToken;
    variable ScanData  : in  TScanData;
    constant Frequency : in  real;
    constant GotoExit1 : in  boolean;
    signal   TMS       : out std_logic;
    signal   TDI       : out std_logic;
    signal   TCK       : out std_logic;
    signal   TDO       : in  std_logic 
  ) is
    variable ScanIn  : PStdLogicVector;
    variable ScanOut : PStdLogicVector;
  begin
    if (ScanData.Length = 0) or (ScanData.TDI = null) then
      return;
    end if;
    ScanIn  := ScanData.TDI;
    ScanOut := new std_logic_vector(ScanIn.all'range);
    Scan(ScanIn.all,ScanOut.all,Frequency,GotoExit1,TMS,TDI,TCK,TDO);
    -- check data from TDO
    if ScanData.TDO /= null then
      if ScanData.MASK /= null then
        if (ScanOut.all and ScanData.MASK.all) /= (ScanData.TDO.all and ScanData.MASK.all) then
          MatchError(Token,
             "TDO data does not match: received '" & Vector2String(ScanOut.all) &
             "' but expected '" & Vector2String(ScanData.TDO.all) &
             "' (mask: '" & Vector2String(ScanData.MASK.all) & "')");
        end if;
      else
        -- no "MASK" specified -> all cares
        if ScanOut.all /= ScanData.TDO.all then
          MatchError(Token,
             "TDO data does not match: received '" & Vector2String(ScanOut.all) &
             "' but expected '" & Vector2String(ScanData.TDO.all) & "')");
        end if;
      end if;
    end if;
    -- deallocate
    deallocate(ScanOut);
  end Scan;

  -----------------------------------------------------------------------------
  -- Parse parameters of command RUNTEST and perform clocks
  --
  procedure ParseRunTest (
    file F             : text;
    State              : inout TLexerState;
    Token              : inout TToken;
    RUNTEST_Run_State  : inout TJTAGState;
    RUNTEST_End_State  : inout TJTAGState;
    JTAGState          : inout TJTAGState;
    Frequency          : in real;
    signal   TMS       : out std_logic;
    signal   TDI       : out std_logic;
    signal   TCK       : out std_logic;
    signal   TDO       : in  std_logic 
  ) is
    -- parsing the parameters is implemented as FSM
    type TParamState is (psTryRunState,psTryRunCount,psCheckRunCount,
                         psTryMinTime,psMinTimeWordSec,
                         psTryWordMaximum,psMaxTime,psMaxTimeWordSec,
                         psWordEndState,psEndState,
                         psSemicolon);
    variable ParamState : TParamState;  -- parameter parsing state
    variable DoGetToken : boolean;      -- true if a new token should be fetched for the next interation of the loop
    variable IsOk       : boolean;      -- helper variable for TryParseToken()
    variable RunCount   : integer;      -- number of TCK cycles
    variable MinTime    : real;         -- minimum time to perform TCK cycles
    variable MaxTime    : real;         -- maximum time to perform TCK cycles
    variable I          : integer;      -- helper variable when using MaxTime
  begin  -- ParseRunTest
    ParamState := psTryRunState;
    DoGetToken := true;
    RunCount := -1;
    MinTime := 0.0;
    MaxTime := 0.0;
    while not State.Complete loop
      if DoGetToken then
        -- get new token
        GetToken(F,State,Token);
        if Token.TokenType = ttSemicolon then
          exit;
        elsif Token.TokenType /= ttIdentifier then
          ParserError(Token,"Invalid token " & Token2String(Token.TokenType,Token.St.ALL));
        end if;
      end if;
      DoGetToken := true;
      case ParamState is
        when psTryRunState =>
          TryParseToken(Token,IsOk,RUNTEST_Run_State);
          if IsOk then
            if not IsStableState(RUNTEST_Run_State) then
              ParserError(Token,"Stable state required");
            end if;
          end if;
          ParamState := psTryRunCount;
          DoGetToken := IsOk;           -- if the token could not be parsed, try again as integer in psTryRunCount
        when  psTryRunCount =>
          TryParseToken(Token,IsOk,RunCount);
          if IsOk then
            -- Note: This could also be "min_time SEC" with an integer
            -- number. We will only know if "SEC" or "TCK"/"SCK" follows
            -- next state: check if we got it right
            ParamState := psCheckRunCount;
          else
            ParamState := psTryMinTime;
            DoGetToken := false;
          end if;
        when psCheckRunCount =>
          if Token.St.all = "TCK" then
            -- yes, it was "run_count" for TCK
            ParamState := psTryMinTime;
          elsif Token.St.all = "SCK" then
            -- yes, it was "run_count" for system clock SCK
            ParserWarning(Token,"System clock (SCK) is not supported!");
            ParamState := psTryMinTime;
          elsif Token.St.all = "SEC" then
            -- nope, it was "min_time SEC"
            MinTime := real(RunCount);
            RunCount := -1;             -- never wanted RunCount -> deactivate
            ParamState := psTryWordMaximum;
          else
            ParserError(Token,"Unknown identifier '" & Token.St.all & "'");
          end if;
        when psTryMinTime =>
          TryParseToken(Token,IsOk,MinTime);
          if IsOk then
            -- got min_time, this opens the possibility for
            -- "MAXIMUM max_time SEC", but first we need "SEC"
            ParamState := psMinTimeWordSec;
          else
            ParamState := psWordEndState;
            DoGetToken := false;
          end if;
        when psMinTimeWordSec =>
          if Token.St.all = "SEC" then
            ParamState := psTryWordMaximum;
          else
            ParserError(Token,"Unknown identifier '" & Token.St.all & "'");
          end if;
        when psTryWordMaximum =>
          if Token.St.all = "MAXIMUM" then
            ParamState := psMaxTime;
          else
            ParamState := psWordEndState;
            DoGetToken := false;
          end if;
        when psMaxTime =>
          ParseToken(Token,MaxTime);
          if (MinTime > 0.0) and (MaxTime <= MinTime) then
            ParserError(Token,"The maximum time must be greater than the minimum time.");
          end if;
          ParamState := psMaxTimeWordSec;
        when psMaxTimeWordSec =>
          if Token.St.all = "SEC" then
            ParamState := psWordEndState;
          else
            ParserError(Token,"Unknown identifier '" & Token.St.all & "'");
          end if;
        when psWordEndState =>
          if Token.St.all = "ENDSTATE" then
            ParamState := psEndState;
          else
            ParserError(Token,"Unknown identifier '" & Token.St.all & "'");
          end if;
        when psEndState =>
          ParseToken(Token,RUNTEST_End_State);
          if not IsStableState(RUNTEST_End_State) then
            ParserError(Token,"Stable state required");
          end if;
          ParamState := psSemicolon;
        when others => null;
      end case;
    end loop;
--    report "RUNTEST " & JTAGStateName(RUNTEST_Run_State) & " "
--      & integer'image(RunCount) & " TCK "
--      & real'image(MinTime) & " SEC "
--      & "MAXIMUM " & real'image(MaxTime) & " SEC "
--      & "ENDSTATE " & JTAGStateName(RUNTEST_End_State) severity note;

    if RunCount = 0 then
      ParserError(Token,"Run count must be greater than 0.");
    end if;
    if (RunCount = -1) and (MinTime = 0.0) then
      ParserError(Token,"At least run count or min time must be given!");
    end if;
    -- RunCount or MinTime, whichever is larger
    RunCount := max(integer(ceil(MinTime*GetFrequency(Frequency))),RunCount);
    -- but a maximum of MaxTime
    if MaxTime > 0.0 then
      I := integer(ceil(MaxTime*GetFrequency(Frequency)));
      if I < RunCount then
        -- Give a warning if the current frequency doesn't allow to run the
        -- specified number of cycles within the specified time limit.
        ParserWarning(Token,"Maximum time set to " & real'image(MaxTime) &
                      " sec., which doesn't allow all " & integer'image(RunCount) &
                      " cycles with current frequency " & real'image(GetFrequency(Frequency)) &
                      " Hz. Reducing number of cycles to " & integer'image(I));
        RunCount := I;
      end if;
    end if;
    ---------------------------------------------------------------------------
    -- Execute RUNTEST:
    -- go to run state
    if JTAGState /= RUNTEST_Run_State then
      ScanTMS(GetTMSStable(JTAGState,RUNTEST_Run_State),Frequency,TMS,TCK);
    end if;
    -- perform TCK cycles
    for i in 1 to RunCount loop
      ClockTCK(GetHalfPeriode(Frequency),TCK);
    end loop;  -- i
    -- go to end state
    if RUNTEST_Run_State /= RUNTEST_End_State then
      ScanTMS(GetTMSStable(RUNTEST_Run_State, RUNTEST_End_State),Frequency,TMS,TCK);
    end if;
    JTAGState := RUNTEST_End_State;
  end ParseRunTest;

  -----------------------------------------------------------------------------
  -- State information for SVF interpreter
  type TSVFState is record
    Frequency   : real;
    JTAGState   : TJTAGState;
    TRST_Mode   : TTRST_Mode;
    ENDDR_State : TJTAGState;
    ENDIR_State : TJTAGState;
    HeadIR      : TScanData;
    HeadDR      : TScanData;
    ScanIR      : TScanData;
    ScanDR      : TScanData;
    TrailIR     : TScanData;
    TrailDR     : TScanData;
    RUNTEST_Run_State : TJTAGState;
    RUNTEST_End_State : TJTAGState;
  end record;

  -----------------------------------------------------------------------------
  -- Read and execute an SVF file
  procedure ProcessSVF (
    constant Filename : string;
    signal   TMS      : out std_logic;
    signal   TDI      : out std_logic;
    signal   TCK      : out std_logic;
    signal   TDO      : in  std_logic 
  ) is
    file F : text open read_mode is Filename;
    variable State     : TLexerState;
    variable Token     : TToken;
    variable SVFState  : TSVFState;
    variable JTAGState : TJTAGState;
    variable JTAGStates : TJTAGStateArr   (MaxStateTransitions-1 downto 0);
    variable TMSVector  : std_logic_vector(MaxStateTransitions-1 downto 0);
    variable I,J       : integer;
  begin
    SVFState.JTAGState      := jsUnknown;
    SVFState.Frequency      := 0.0;
    SVFState.TRST_Mode      := tmAbsent;
    SVFState.ENDDR_State    := jsIdle;  -- default: IDLE
    SVFState.ENDIR_State    := jsIdle;  -- default: IDLE
    SVFState.HeadIR.Length  := 0;
    SVFState.HeadDR.Length  := 0;
    SVFState.ScanIR.Length  := 0;
    SVFState.ScanDR.Length  := 0;
    SVFState.TrailIR.Length := 0;
    SVFState.TrailDR.Length := 0;
    SVFState.RUNTEST_Run_State := jsIdle;  -- default: IDLE
    SVFState.RUNTEST_End_State := jsIdle;  -- default: IDLE
    State := StartLexer(Filename,F);
    TDI <= 'Z';
    while not State.Complete loop
      -- Parse a command
      GetToken(F,State,Token);
      -- quit loop on end of file
      if Token.TokenType = ttEOF then
        return;
      end if;
--      PrintToken(Token);
      -- commands have to be a ttIdentifier
      if Token.TokenType /= ttIdentifier then
        ParserError(Token,"Invalid token " & Token2String(Token.TokenType,Token.St.ALL));
      end if;
      -------------------------------------------------------------------------
      if    Token.St.all = "ENDIR" then
        RequireToken(F,State,Token,SVFState.ENDIR_State);
        if not IsStableState(SVFState.ENDIR_State) then
          ParserError(Token,"Stable state required");
        end if;
        RequireToken(F,State,Token,ttSemicolon);
      -------------------------------------------------------------------------
      elsif Token.St.all = "ENDDR" then
        RequireToken(F,State,Token,SVFState.ENDDR_State);
        if not IsStableState(SVFState.ENDDR_State) then
          ParserError(Token,"Stable state required");
        end if;
        RequireToken(F,State,Token,ttSemicolon);
      -------------------------------------------------------------------------
      elsif Token.St.all = "FREQUENCY" then
        -- FREQUENCY [cycles HZ];
        --  - "cycles" specifies the frequency in Hz (>0!)
        --  - If this optional parameter is not specified, the implementation
        --    dependent default frequency is used (DefaultFrequency).
        --  - "RUNTEST" should give an error if the current frequency doesn't
        --    allow to run the specified number of cycles within the
        --    specified time limit.
        GetToken(F,State,Token);
        if Token.TokenType = ttSemicolon then
          -- no value given -> use default value
          SVFState.Frequency := 0.0;
        elsif Token.TokenType = ttIdentifier then
          -- value given -> convert to Real
          ParseToken(Token,SVFState.Frequency);
          if SVFState.Frequency = 0.0 then
            ParserError(Token,"A value greater than 0.0 is required");
          end if;
          -- requre "HZ"
          RequireToken(F,State,Token,ttIdentifier);
          if Token.St.all /= "HZ" then
            ParserError(Token,"'HZ' expected");
          end if;
          -- require ';'
          RequireToken(F,State,Token,ttSemicolon);
        else
          ParserError(Token,"Invalid token " & Token2String(Token.TokenType,Token.St.ALL));
        end if;
      -------------------------------------------------------------------------
      elsif Token.St.all = "HDR" then
        -- HDR length [TDI (tdi)] [TDO (tdo)] [MASK (mask)] [SMASK (smask)];
        --  - Specifies a default header pattern that is shifted in before
        --    every scan operation. Used for scan paths with devices beyond
        --    the component of interest.
        --  - "length" specifies the number of bits to scan.
        --  - 0 removes the header.
        --  - "TDI (tdi)": scanned into the target
        --     - optional, will be remembered
        --     - must be specified the first time and when the length changes
        --  - "TDO (tdo)": value to be compared against
        --  - "MASK (mask)": mask used when comparing TDO
        --     - optional, will be remembered
        --     - if "length" changes and "MASK" is absent, "all cares"
        --       will be used
        --  - "SMASK (smask)": specifies that TDI data is don't care
        --     - TODO: I don't understand what this parameter means
        ParseScanData(F,State,Token,SVFState.HeadDR);
      -------------------------------------------------------------------------
      elsif Token.St.all = "HIR" then
        ParseScanData(F,State,Token,SVFState.HeadIR);
      -------------------------------------------------------------------------
      elsif Token.St.all = "PIO" then
        -- just eat all tokens up to the semicolon
        ParserWarning(Token,"Ignoring PIO command!");
        while not State.Complete loop
          GetToken(F,State,Token);
          if Token.TokenType = ttSemicolon then
            exit;
          end if;
        end loop;
        -- TODO: implement
      -------------------------------------------------------------------------
      elsif Token.St.all = "PIOMAP" then
        -- just eat all tokens up to the semicolon
        ParserWarning(Token,"Ignoring PIOMAP command!");
        while not State.Complete loop
          GetToken(F,State,Token);
          if Token.TokenType = ttSemicolon then
            exit;
          end if;
        end loop;
        -- TODO: implement
     -------------------------------------------------------------------------
      elsif Token.St.all = "RUNTEST" then
        ParseRunTest(F,State,Token,SVFState.RUNTEST_Run_State,SVFState.RUNTEST_End_State,SVFState.JTAGState,SVFState.Frequency,TMS,TDI,TCK,TDO);
      -------------------------------------------------------------------------
      elsif Token.St.all = "SDR" then
        ParseScanData(F,State,Token,SVFState.ScanDR);
        -- go to scanning state
        ScanTMS(GetTMSShift(SVFState.JTAGState,jsDRShift),SVFState.Frequency,TMS,TCK);
        -- do the scanning including HDR and TDR
        Scan(Token,SVFState.HeadDR, SVFState.Frequency,false,TMS,TDI,TCK,TDO);
        Scan(Token,SVFState.ScanDR, SVFState.Frequency,(SVFState.TrailDR.Length = 0),TMS,TDI,TCK,TDO);
        Scan(Token,SVFState.TrailDR,SVFState.Frequency,true, TMS,TDI,TCK,TDO);
        TDI <= 'Z';
        -- go to stable state
        ScanTMS(GetTMSShift(jsDRShift,SVFState.ENDDR_State),SVFState.Frequency,TMS,TCK);
        SVFState.JTAGState := SVFState.ENDDR_State;
      -------------------------------------------------------------------------
      elsif Token.St.all = "SIR" then
        ParseScanData(F,State,Token,SVFState.ScanIR);
        -- go to scanning state
        ScanTMS(GetTMSShift(SVFState.JTAGState,jsIRShift),SVFState.Frequency,TMS,TCK);
        -- do the scanning including HIR and TIR
        Scan(Token,SVFState.HeadIR, SVFState.Frequency,false,TMS,TDI,TCK,TDO);
        Scan(Token,SVFState.ScanIR, SVFState.Frequency,(SVFState.TrailIR.Length = 0),TMS,TDI,TCK,TDO);
        Scan(Token,SVFState.TrailIR,SVFState.Frequency,true, TMS,TDI,TCK,TDO);
        TDI <= 'Z';
        -- go to stable state
        ScanTMS(GetTMSShift(jsIRShift,SVFState.ENDIR_State),SVFState.Frequency,TMS,TCK);
        SVFState.JTAGState := SVFState.ENDIR_State;
      -------------------------------------------------------------------------
      elsif Token.St.all = "STATE" then
        -- read all states
        I := 0;
        while not State.Complete loop
          GetToken(F,State,Token);
          if    Token.TokenType = ttSemicolon then
            -- semicolon, leave the loop
            exit;
          elsif Token.TokenType = ttIdentifier then
            -- take state
            if I >= MaxStateTransitions then
              -- maximum exceeded
              ParserError(Token,"Maximum number of state transitions for command STATE of " & integer'image(MaxStateTransitions) & " is exceeded.");
            end if;
            ParseToken(Token,JTAGState);
            JTAGStates(I) := JTAGState;
            I := I + 1;
          else
            ParserError(Token,"Invalid token " & Token2String(Token.TokenType,Token.St.ALL));
          end if;
        end loop;
        -- check that the last given state is a stable state
        if not IsStableState(JTAGState) then
          ParserError(Token,"STATE requires the last state to be a stable state");
        end if;
        -- perform TMS states
        if I = 1 then
          -- if only one state was given, we have to find the path on our own
          JTAGState := JTAGStates(0);
          ScanTMS(GetTMSStable(SVFState.JTAGState,JTAGState),SVFState.Frequency,TMS,TCK);
          SVFState.JTAGState := JTAGState;
        else
          -- multiple states given, create TMS vector and scan it
          for J in 0 to I-1 loop
            JTAGState := JTAGStates(J);
            TMSVector(J) := GetTMS(SVFState.JTAGState,JTAGState);
            SVFState.JTAGState := JTAGState;
          end loop;  -- J
          ScanTMS(TMSVector(I-1 downto 0),SVFState.Frequency,TMS,TCK);
        end if;
      -------------------------------------------------------------------------
      elsif Token.St.all = "TDR" then
        ParseScanData(F,State,Token,SVFState.TrailDR);
      -------------------------------------------------------------------------
      elsif Token.St.all = "TIR" then
        ParseScanData(F,State,Token,SVFState.TrailIR);
      -------------------------------------------------------------------------
      elsif Token.St.all = "TRST" then
        RequireToken(F,State,Token,ttIdentifier);
        if    Token.St.all = "ON" then
          SVFState.TRST_Mode := tmOn;
        elsif Token.St.all = "OFF" then
          SVFState.TRST_Mode := tmOff;
        elsif Token.St.all = "Z" then
          SVFState.TRST_Mode := tmZ;
        elsif Token.St.all = "ABSENT" then
          SVFState.TRST_Mode := tmAbsent;
          -- additional limitations apply for this mode, but these are not implemented
        else
          ParserError(Token,"TRST requires (ON|OFF|Z|ABSENT) but found '" & Token.St.all & "'");
        end if;
        ParserWarning(Token,"TRST command is ignored");
        RequireToken(F,State,Token,ttSemicolon);
      -------------------------------------------------------------------------
      else
        ParserError(Token,"Unknown identifier '" & Token.St.all & "'");
      end if;
    end loop;
  end ProcessSVF;

end svf;
