vlib work
vmap work
set IgnoreSVAWarning 1
set IgnoreWarning 1
set StdArithNoWarnings 1
set NumericStdNoWarnings 1
set IgnoreNote 1
set IgnoreError 1
set IgnoreFailure 1
set Show_VitalChecksWarnings 1
vlog verilog/*.v
vopt +acc mkTb_Processor -o opt_tb
vsim opt_tb -onfinish stop +no_glitch_msg +no_timing_msg
onbreak {
echo "Resume macro at $now"
resume
}
force -freeze sim:/mkTb_Processor/CLK 1 0, 0 {2500 ps} -r {5 ns}
force -freeze {sim:/mkTb_Processor/RST_N} 0 0
run 20 ns
force -freeze {sim:/mkTb_Processor/RST_N} 1 0
run -all
