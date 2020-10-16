echo off

rem DOS - DF-DOS compile program

echo.
echo DF-DOS compile program

echo.
echo Compiling kernel... (Requires nasm, DOS 16bit to be in the path)

cd Source
nasm kernel.asm -o KERNEL.COM
cd ..

echo.
echo Done

echo.
echo Moving

move Source\KERNEL.COM Compiled

echo.
echo Done

echo.
echo Completed
echo Kernel location: Compiled\KERNEL.COM
echo.

echo on