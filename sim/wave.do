onerror {resume}
quietly WaveActivateNextPane {} 0
add wave -noupdate /tb_svf/TCK
add wave -noupdate /tb_svf/TDI
add wave -noupdate /tb_svf/TDO
add wave -noupdate /tb_svf/TMS
TreeUpdate [SetDefaultTree]
WaveRestoreCursors {{Cursor 1} {514510529 ns} 0}
configure wave -namecolwidth 150
configure wave -valuecolwidth 100
configure wave -justifyvalue left
configure wave -signalnamewidth 0
configure wave -snapdistance 10
configure wave -datasetprefix 0
configure wave -rowmargin 4
configure wave -childrowmargin 2
configure wave -gridoffset 0
configure wave -gridperiod 1
configure wave -griddelta 40
configure wave -timeline 0
configure wave -timelineunits ns
update
WaveRestoreZoom {0 ns} {2176568100 ns}
