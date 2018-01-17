# Xilinx Vivado script
# Version: Vivado 2015.4
# Function:
#   Generate a vivado project for the shakti c-class

#set mem_data_width {64}
#set io_data_width {32}
#set axi_id_width {8}

set origin_dir "."
set base_dir "../"

set project_name [lindex $argv 0]
set CONFIG [lindex $argv 1]

# Set the directory path for the original project from where this script was exported
set orig_proj_dir [file normalize $origin_dir/$project_name]

# Create project
create_project $project_name $origin_dir/$project_name

# Set the directory path for the new project
set proj_dir [get_property directory [current_project]]

# Set project properties
set obj [get_projects $project_name]
set_property "default_lib" "xil_defaultlib" $obj
set_property "part" "xc7a100tcsg324-1" $obj
set_property "simulator_language" "Mixed" $obj

# Create 'sources_1' fileset (if not found)
if {[string equal [get_filesets -quiet sources_1] ""]} {
  create_fileset -srcset sources_1
}

# Set 'sources_1' fileset object
set files [list \
               [file normalize $base_dir/nexys4_ddr/Top.v ]\
               [file normalize $base_dir/verilog_fpga/BRAM2.v] \
               [file normalize $base_dir/verilog_fpga/BRAM2Load.]v \
               [file normalize $base_dir/verilog_fpga/BRAM2BELoad.]v \
               [file normalize $base_dir/verilog_fpga/Counter.v] \
               [file normalize $base_dir/verilog_fpga/FIFO1.v] \
               [file normalize $base_dir/verilog_fpga/FIFO2.v] \
               [file normalize $base_dir/verilog_fpga/FIFOL1.v] \
               [file normalize $base_dir/verilog_fpga/RevertReg.v] \
               [file normalize $base_dir/verilog_fpga/SizedFIFO.v] \
               [file normalize $base_dir/verilog_fpga/mkproc.v] \
               [file normalize $base_dir/verilog_fpga/mkcore.v] \
               [file normalize $base_dir/verilog_fpga/mkcsr.v] \
               [file normalize $base_dir/verilog_fpga/mkdcache.v] \
               [file normalize $base_dir/verilog_fpga/mkexecution_unit.v] \
               [file normalize $base_dir/verilog_fpga/mkfpu.v] \
               [file normalize $base_dir/verilog_fpga/mkicache.v] \
               [file normalize $base_dir/verilog_fpga/mkmemory_unit.v] \
               [file normalize $base_dir/verilog_fpga/mkmuldiv.v] \
               [file normalize $base_dir/verilog_fpga/mkregisterfile.v] \
							 [file normalize $base_dir/verilog_fpga/mkbpu_bimodal.v] \
               [file normalize $base_dir/verilog_fpga/mkriscv.v] \
               [file normalize $base_dir/verilog_fpga/module_fn_branch.v] \
               [file normalize $base_dir/verilog_fpga/module_fn_decoder.v] \
             ]
add_files -norecurse -fileset [get_filesets sources_1] $files

# add include path
set_property include_dirs [list \
                               [file normalize $origin_dir/src ]\
                               [file normalize $origin_dir/generated-src] \
                              ] [get_filesets sources_1]

#set_property verilog_define [list FPGA FPGA_FULL NEXYS4] [get_filesets sources_1]

# Set 'sources_1' fileset properties
set_property "top" "Top" [get_filesets sources_1]

#Generate clock divider IP.
create_ip -name clk_wiz -vendor xilinx.com -library ip -module_name artix_clockdivider

set_property -dict [list CONFIG.PRIM_IN_FREQ {100} \
													CONFIG.PRIMITIVE {MMCM} \
													CONFIG.CLKOUT1_REQUESTED_OUT_FREQ {20} \
													CONFIG.MMCM_DIVCLK_DIVIDE {1} \
													CONFIG.MMCM_CLKFBOUT_MULT_F {8.500} \
													CONFIG.MMCM_CLKIN1_PERIOD {10.0} \
													CONFIG.MMCM_CLKOUT0_DIVIDE_F {42.500} \
													CONFIG.CLKOUT1_JITTER {193.154} \
													CONFIG.CLKOUT1_PHASE_ERROR {109.126}] \
													[get_ips artix_clockdivider]
generate_target {instantiation_template} [get_files $proj_dir/$project_name.srcs/sources_1/ip/artix_clockdivider/artix_clockdivider.xci]

# Create 'constrs_1' fileset (if not found)
if {[string equal [get_filesets -quiet constrs_1] ""]} {
  create_fileset -constrset constrs_1
}

# Set 'constrs_1' fileset object
set obj [get_filesets constrs_1]

# Add/Import constrs file and set constrs file properties
set file "[file normalize "$origin_dir/constraints/constraints.xdc"]"
set file_added [add_files -norecurse -fileset $obj $file]

# generate all IP source code
generate_target all [get_ips]

# force create the synth_1 path (need to make soft link in Makefile)
launch_runs -scripts_only synth_1


# suppress some not very useful messages
# warning partial connection
set_msg_config -id "\[Synth 8-350\]" -suppress
# info do synthesis
set_msg_config -id "\[Synth 8-256\]" -suppress
set_msg_config -id "\[Synth 8-638\]" -suppress
# BRAM mapped to LUT due to optimization
set_msg_config -id "\[Synth 8-3969\]" -suppress
# BRAM with no output register
set_msg_config -id "\[Synth 8-4480\]" -suppress
# DSP without input pipelining
set_msg_config -id "\[Drc 23-20\]" -suppress
# Update IP version
set_msg_config -id "\[Netlist 29-345\]" -suppress


# do not flatten design
set_property STEPS.SYNTH_DESIGN.ARGS.FLATTEN_HIERARCHY none [get_runs synth_1]
set_property strategy Flow_PerfOptimized_high [get_runs synth_1]
