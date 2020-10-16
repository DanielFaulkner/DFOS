        OEM_ID                  db      "DFOS    "      ; 8 char sys ID
        BytesPerSector          dw      512             ; Sector size in bytes
        SectorsPerCluster       db      1               ; Sector per cluster
        ReservedSectors         dw      4               ; Reserved sectors (This I think for this should become 2 for the boot strap, was 1)
        TotalFATs               db      2               ; Number of fats
        MaxRootEntries          dw      224             ; Root directory entries
        TotalSectorsSmall       dw      2880            ; Total Sectors
        MediaDescriptor         db      0F0h            ; Format ID (FAT12 ID number)
        SectorsPerFAT           dw      9               ; Sectors per FAT
        SectorsPerTrack         dw      18              ; Sectors per track
        NumHeads                dw      2               ; Number of heads (2 as double sided floppy)
        HiddenSectors           dd      0               ; Special hidden sectors
        TotalSectorsLarge       dd      0               ; More sectors
        DriveNumber             db      0               ; Drive Number (Primary Floppy is normally 0)
        Flags                   db      0               ; Reserved
        Signature               db      41              ; Boot signature
        VolumeID                dd      435101793       ; Volume serial number
        VolumeLabel             db      "NO NAME    "   ; Volume label (11 bytes)
        SystemID                db      "FAT12   "      ; File system (8 bytes)
