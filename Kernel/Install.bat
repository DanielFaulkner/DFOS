echo off

rem Install kernel program

echo.
echo DF-DOS kernel file install program

echo.
echo Please insert the floppy disc containing the FAT12 bootloader.
echo (DFTECH Bootloader Ver 0.5 recommended)
echo.
pause
echo Moving file to floppy disc (a:)
copy Compiled\KERNEL.COM a:\KERNEL.COM
echo.
echo Done
echo.
echo Completed
echo The floppy disc now contains the DF-DOS kernel.
echo.

echo on