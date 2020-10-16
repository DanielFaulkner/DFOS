echo off

rem DFOS boot loader ver 0.5 compile program:

echo.
echo DFTECH Bootloader Version 0.5 (FAT12)

echo.
echo Compiling... (You MUST have nasm in your path)

cd Source
nasm ownbl5s1.asm
nasm ownbl5s2.asm
cd ..

echo.
echo Done

echo.
echo Moving to correct directory...

move Source\ownbl5s1 Compiled
move Source\ownbl5s2 Compiled

echo.
echo Done

echo.
echo Completed
echo The boot loader files have been re-created. (Located in compiled directory)
echo.

echo on