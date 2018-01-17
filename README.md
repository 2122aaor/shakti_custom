NOTE: 
SOME OF THE CODELINE (ESPECIALLY the C Class) IN THIS REPO IS OUTDATED. WE ARE IN THE MIDDLE OF A TAPEOUT AND THUS MAINTAINING A PRIVATE REPO WHICH CONTAINS SOME PROPREITARY FOUNDRY ITEMS. 
REPO WILL BE UPDATED TO THE LATEST CODELINE BY FIRST WEEK OF DECEMBER ONCE WE SANITIZE THE CODE FOR PUBLIC CONSUMPTION.
IF YOU ARE INTERESTED IN A EARLY ACCESS TO THE C-CLASS (64-BIT) PLEASE MAIL US AT:
Madhusudan : gs dot madhusudan at cse dot iitm dot ac dot in
Neel Gala  : neelgala at gmail dot com
----------------------------------------------------------------------------------------------------------------------------

SHAKTI Processor Project
-------------------------

The SHAKTI processor project aims to build variants of processors based on the RISC-V ISA from UC Berkeley (www.riscv.org). 
The project will develop a series of cores, SoC fabrics and a reference SoC for each core family. 
While the cores and most of the SoC components (including bus and interconnect fabrics) will be in open source, 
some standard components like PCIe controller, DDR controller and PHY IP will be 3rd party IP blocks.

All source is licensed using 3 part BSD license and will be royalty and patent free (as far as IIT-Madras is concerned, 
we will not assert any patents). While the primary focus is research, the SoCs are being designed to be competitive 
with commercial processors with respect to features, silicon area, power profile 
and frequency. This of course assumes that an optimal layout process is used to tape out our design. All FPGA data 
will also be made available

While we do plan to tape out a few variants, given the foundry NDA requirements, we will not be able to 
publish any layout/backend data. But the ASIC syntesis and P&R data to teh extent possible will be published
to allow others to replicate our ASIC flow.


Core Variants
--------------

E-Class
-------
1. 32 bit 3 stage in-order core aimed at 10 - 50 Mhz uC variants
2. Optional memory protection
3. Very low power static design
4. Variants with compressed/reduced ISA support
5. Bus - AHB Lite

C class 
-------
1. 32 and 64 bit 3-8 stage in-order core aimed at 10 Mhz - 1 Ghz controller requiremenets
2. Optional memory protection and MMU
3. Very low power static design varinats
4. Fault Tolerant variants for ISO26262 applications
5. IoT variants will have compressed/reduced ISA support
6. Optional FPU, VPU
7. Bus - AHB variants

I class
-------
1. 64-bit, 1-8 core, 8+ stage out of order, aimed at 200 Mhz - 2 Ghz industrial control / general purpose applications
2. Shared L2 cache, dual threading support, SIMD/VPU
3. BUS - Shakti NoC + AXI4

M Class
-------
1. Enhanced variants of the I-class processors aimed at general purpose compute, low end server and mobile applications
2. Enhancements over I class � larger issue size, quad-threaded, up to 16 cores, burst freq up to 3 Ghz, 
3. Bus/Fabric - Shati NoC, AXI4

S class
-------
1. 64-bit superscalar, multi-threaded variant for desktop/server applications.
2. 2-64 cores, crossbar/ring interconnect, segmented L3 cache
3. RapidIO based external cache coherent interconnect for multi-socket applications (up to 256 sockets)
4. Hybrid Memory Cube support, 4 channels    
5. Threaded Vector Processor Unit
6. Coprocessors for Database acceleration, Security acceleration, Machine Learning
7. Experimental variants will be used as test-bed for our Adaptive System Fabric project  
8. Variants with differentiated cores for running kernels and user space VMs.

H class
-------
1. 64-bit in-order/mildly OO, multi-threaded, HPC variant with 32-128 cores
2. 512/1024 bit VPU
3. Goal is 3-5 + Tflops (DP, sustained)

T class
-------
1. Experimental security oriented 128/64-bit cores
2. tagged ISA, single address space option, decoupling of protection from memory management.
	
N class
-------
1. Experimental cores for network processing targeting at executing the P4 NP language. based on custom ISA extensions

Experimental accelerators
-------------------------
1. Neuromorphic accelerators for AI and ML type applications
2. Stochastic cores for low power, non-deterministic computing tasks. An HVEC and AOMedia video decoder will be the first target.
3. Vector processor variants for - HPC, SDR cores for 5G modems/basestations, low power OFDM based SDR
	
SoC Fabrics
-----------
1. Lightweight  microcontroller bus
2. Fabric for 2 - 128 cores. Topogies - ring+mesh hybrid
3. Mesh fabric for HPC 


Socket Interconnect
-------------------

We are also developing a processor to processor, cache-coherent interconnect, to allow building of 
multi-socket server systems.The interconnect is based on the RapidIO standard. We are investigating 
a two tier scheme where a MOESI/MESIF style scheme is 
used for 2-8 socket systems and a directory based scheme for larger configurations (max 256 sockets)


Design Approach
---------------

The approach is to built optimal (high performance) building blocks that can be shared among the variants and 
then add variant specific IP blocks. The above variants are just canonical references and the Shakti family will
see variants that will be hybrids.


Where  possible, we will also provide the Synopsys/Cadence and Xilinx/Altera synthesis results for each module.


Final versions will contain the full BSV code, the generated Verilog code, testbenches, verification IP and FPGA support files.


Related projects
----------------
The SHAKTI effort is part of a larger effort to build complete systems. As part of this effort, 
IIT-Madras is developing interconnects (optical and copper) based on Gen 3 (10/25G per lane) RapidIO and 
a scale-out SSD storage system called lightsor (see lightstor.org) 
based on this interconnect. The final goal is to build a fabric called Adaptive System fabric 
which will use a combination of  Hybrid Memory Cubes and RapidIO to  unify support for compute, networking and storage.

Current Status
--------------
Alpha versions of the I, C and E classes have been released publicly

The source repository is composed of 2 directories, the core sources and sources for the non-core IP blocks.
Design documents and standards referenced in the design are provided in the doc directory.


Current Team Members: 
---------------------
1. Rahul Bodduna    (rahul.bodduna@gmail.com)
2. Neel Gala 	    (neelgala@gmail.com)
3. Arjun Menon      (c.arjunmenon@gmail.com)
4. G Vinod          (g.vinod1993@gmail.com) 
5. Abhinaya Agrawal (agrawal.abhinaya@gmail.com)
6. G S Madhusudan   (gs.madhusudan@cse.iitm.ac.in, madhu@macaque.in)
7. V. Kamakoti      (kama@cse.iitm.ac.in, veezhi@gmail.com)

For Queries/Collaboration/Feedback :
--------------------------------------

shakti.iitm@gmail.com
