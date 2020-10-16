# Compile the kernel make file

# Compile the kernel
kernel: kernel.asm, txtio.inc, commands.inc, console.inc, conver.inc, data.inc, FAT12drv.inc, FATtable.inc, HWtests.inc, int20.inc, int21.inc, int22.inc, int23.inc, int24.inc, int25.inc, int27.inc, int28.inc, int29.inc, int 2e,inc, int2f.inc, IntFuncs.inc, intrupt.inc
	nasm -f elf kernel.asm
	cp kernel KERNEL.COM
	# KERNEL.COM should be moved to the compiled directory

# Clean the spare kernel file.
cleanKernel:
	rm kernel

# Will add an install section later, however I am unsure if I can
#  refer to the floppy drive as /dev/fd0 when it is mounted