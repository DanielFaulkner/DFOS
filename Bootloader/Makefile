# Compile the boot loader
CompileBootloader: Source/ownbl5s2.asm, Source/ownbl5s1.asm
	nasm -f elf Source/ownbl5s1.asm
	nasm -f elf Source/ownbl5s2.asm
	cp ownbl5s1 Compiled/ownbl5s1
	cp ownbl5s2 Compiled/ownbl5s2

# Clean the spare boot loader compiled files
cleanBootloader:
	rm Source/ownbl5s1
	rm Source/ownbl5s2

# Install bit:
Install:
	dd if=Compiled/ownbl5s1 bs=512 of=/dev/fd0
	dd if=Compiled/ownbl5s2 bs=512 seek=1 of=/dev/fd0