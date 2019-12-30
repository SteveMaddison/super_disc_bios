;
; Constants for SNES CD-ROM hardware (SFX-100)
;

; B-Bus Registers
.DEFINE SCD_SRAM_LOCK           $21D0 ; BIOS Cartridge Battery SRAM Lock (write $00)
.DEFINE SCD_SRAM_UNLOCK2        $21E0 ; BIOS Cartridge Battery SRAM Unlock Step 2 (write $0F down to $01)
.DEFINE SCD_MECHACON            $21E1 ; Mechacon
.DEFINE SCD_CXD_INDEX           $21E2 ; CDROM Decoder/FIFO Index
.DEFINE SCD_CXD_DATA            $21E3 ; CDROM Unit Decoder/FIFO Data
.DEFINE SCD_MYSTERY             $21E4 ; Control/Enable??
.DEFINE SCD_SRAM_UNLOCK1        $21E5 ; BIOS Cartridge Battery SRAM Unlock Step 1 (write $FF)


; CXD-1800 decoder registers. These can be numbered differently for reads and
; writes, so both flavours are written out in full. Values/flags apply to both
; reads and writes and are defined/described where it makes most sense.

; CXD Read Registers
.DEFINE CXD_RD_DMADATA          $00 ; SRAM-to-CPU Xfer Data
.DEFINE CXD_RD_INTSTS           $01 ; Interrupt Status
.DEFINE   CXD_INTSTS_DECINT     $10 ; DecInt set
.DEFINE CXD_RD_STS              $02 ; Status
.DEFINE   CXD_RD_STS_DRQ        $80 ; Data Request (DRQ Pin)                            (0=?, 1=?)
.DEFINE   CXD_RD_STS_ADPBSY     $40 ; ADPCM Playback Busy                               (0=No, 1=Busy)
.DEFINE   CXD_RD_STS_ERINBLK    $20 ; Erasure in Block; C2 flg anywhere except Syncmark (0=Okay, 1=Bad)
.DEFINE   CXD_RD_STS_CORINH     $10 ; Correction Inhibit; MODE/FORM error & AUTODIST=1  (0=Okay, 1=Bad)
.DEFINE   CXD_RD_STS_EDCOK      $08 ; EDC Error Detect Checksum (optional for FORM2)    (0=Bad, 1=Okay)
.DEFINE   CXD_RD_STS_ECCOK      $04 ; ECC Error Correction Codes (not for FORM2)        (0=Bad, 1=Okay)
.DEFINE   CXD_RD_STS_SHRTSCT    $02 ; Sync Mark too early, no ECC/EDC done              (0=Okay, 1=Bad)
.DEFINE   CXD_RD_STS_NOSYNC     $01 ; Sync Mark too late/missing, unreal SYNC inserted  (0=Okay, 1=Bad)

.DEFINE CXD_RD_HDRFLG           $03 ; Header Flags
.DEFINE   CXD_HDRFLG_MIN        $80 ; Header MM   (0=Okay, 1=Error) ;\
.DEFINE   CXD_HDRFLG_SEC        $40 ; Header SS   (0=Okay, 1=Error) ; Header from MODE1/MODE2 data
.DEFINE   CXD_HDRFLG_BLOCK      $20 ; Header FF   (0=Okay, 1=Error) ; sector (ie. not for audio)
.DEFINE   CXD_HDRFLG_MODE       $10 ; Header MODE (0=Okay, 1=Error) ;/
.DEFINE   CXD_HDRFLG_FILE       $08 ; Sub-Header  (0=Okay, 1=Error) ;\Subheader exists for MODE2 only
.DEFINE   CXD_HDRFLG_CHANNEL    $04 ; Sub-Header  (0=Okay, 1=Error) ; (the SNES CD BIOS wants these
.DEFINE   CXD_HDRFLG_SUBMODE    $02 ; Sub-Header  (0=Okay, 1=Error) ; bits to be zero for MODE1, too)
.DEFINE   CXD_HDRFLG_CI         $01 ; Sub-Header  (0=Okay, 1=Error) ;/

.DEFINE CXD_RD_HDR_MIN          $04 ; Header "MM" Minute
.DEFINE CXD_RD_HDR_SEC          $05 ; Header "SS" Second
.DEFINE CXD_RD_HDR_BLOCK        $06 ; Header "FF" Frame

.DEFINE CXD_RD_HDR_MODE         $07 ; Header Mode
.DEFINE CXD_RD_SHDR_FILE        $08 ; Sub-Header File
.DEFINE CXD_RD_SHDR_CH          $09 ; Sub-Header Channel
.DEFINE CXD_RD_SHDR_S_MODE      $0A ; Sub-Header SubMode
.DEFINE CXD_RD_SHDR_CI          $0B ; Sub-Header Coding Info
.DEFINE CXD_RD_CMADR_L          $0C ; Current Minute Address, Low
.DEFINE CXD_RD_CMADR_H          $0D

.DEFINE CXD_RD_MDFM             $0E ; MODE/FORM
.DEFINE   CXD_MDFM_RMODE2       $10 ; Raw MODE byte, Bit2-7 logic sum
.DEFINE   CXD_MDFM_RMODE1       $08 ; Raw MODE byte, Bit1
.DEFINE   CXD_MDFM_RMODE0       $04 ; Raw MODE byte, Bit0
.DEFINE   CXD_MDFM_CMODE        $02 ; Correction Mode (0=MODE1, 1=MODE2)
.DEFINE   CXD_MDFM_CFORM        $01 ; Correction Form (0=FORM1, 1=FORM2) (for MODE2 only)

.DEFINE CXD_RD_ADPCI            $0F ; ADPCM Coding Information
.DEFINE   CXD_ADPCI_MUTE        $80 ; DA data is muted on      (0=No, 1=Muted)      <--- from where?
.DEFINE   CXD_ADPCI_EMPHASIS    $40 ; ADPCM Emphasis           (0=Normal/Off, 1=Emphasis)
.DEFINE   CXD_ADPCI_EOR         $20 ; End of Record                         <--- (from SubMode.Bit0)
.DEFINE   CXD_ADPCI_BITLNGTH    $10 ; ADPCM Bit Length         (0=Normal/4bit, 1=8bit)
.DEFINE   CXD_ADPCI_FS          $04 ; ADPCM Sampling Frequency (0=37800Hz, 1=18900Hz)
.DEFINE   CXD_ADPCI_MS          $01 ; ADPCM Mono/Stereo        (0=Mono, 1=Stereo)

.DEFINE CXD_RD_TEST0            $10
.DEFINE CXD_RD_TEST1            $11
.DEFINE CXD_RD_TEST2            $12

.DEFINE CXD_RD_DMAXFRC_L        $18 ; SRAM-to-CPU Xfer Length
.DEFINE CXD_RD_DMAXFRC_H        $19
.DEFINE CXD_RD_DMAADRC_L        $1A ; SRAM-to-CPU Xfer Address
.DEFINE CXD_RD_DMAADRC_H        $1B
.DEFINE CXD_RD_DRVADRC_L        $1C ; Disc-to-SRAM Xfer Address
.DEFINE CXD_RD_DRVADRC_H        $1D


; CXD Write Registers
.DEFINE CXD_WR_RESERVED                 $00 ; Reserved

.DEFINE CXD_WR_DRVIF                    $01 ; Drive Interface
.DEFINE   CXD_DRVIF_XSLOW               $80 ; DMA/SRAM Speed (0=Slow/12 clks/320ns, 1=Fast/4 clks/120ns)
.DEFINE   CXD_DRVIF_C2PL1ST             $40 ; DATA input C2PO-byte-order (0=Upper first, 1=Lower first)
.DEFINE   CXD_DRVIF_LCHLOW              $20 ; Audio LRCK Polarity for Left channel (0=High, 1=Low)
.DEFINE   CXD_DRVIF_BCKRED              $10 ; Audio BCLK Edge for strobing DATA (0=Falling, 1=Rising)
.DEFINE   CXD_DRVIF_BCKMD1              $08 ; Audio BCLKs per WCLK cycle (0=16, 1=24, 2/3=32)
.DEFINE   CXD_DRVIF_BCKMD0              $04
.DEFINE   CXD_DRVIF_LSB1ST              $02 ; Audio DATA (bit?-)ordering (0=MSB First, 1=LSB first)
.DEFINE   CXD_DRVIF_CLKLOW              $01 ; CLK Pin Output (0=8.4672MHz, 1=Fixed Low)

.DEFINE CXD_WR_CHPCTL                   $02 ; Chip Control
.DEFINE   CXD_CHPCTL_CHPRST             $10 ; Chip Reset (takes 500ns)   (0=No change, 1=Reset the chip)
.DEFINE   CXD_CHPCTL_CD_DA              $08 ; CD-Digital Audio Mode      (0=Data/CDROM, 1=Audio/CD-DA)
.DEFINE   CXD_CHPCTL_SWOPN              $04 ; Sync Detection Window      (0=Only if Sync expected, 1=Anytime)
.DEFINE   CXD_CHPCTL_RPSTART            $02 ; Repeat Correction Start  (0=No change, 1=Repeat if repeat mode)
.DEFINE   CXD_CHPCTL_ADPEN              $01 ; ADPCM Decode (to be set max 11.5ms after DECINT) (0=No, 1=Yes)

.DEFINE CXD_WR_DECCTL                   $03 ; Decoder Control
.DEFINE   CXD_DECCTL_AUTOCI             $80 ; ADPCM Coding Information (0=Use CI Register, 1=Disc Subheader)
.DEFINE   CXD_DECCTL_MODESEL            $20 ; Mode Select (when AUTODIST=0)               (0=MODE1, 1=MODE2)
.DEFINE   CXD_DECCTL_FORMSEL            $10 ; Form Select (when AUTODIST=0 and MODESEL=1) (0=FORM1, 1=FORM2)
.DEFINE   CXD_DECCTL_AUTODIST           $08 ; Auto Distinction        (0=Use MODESEL/FORMSEL, 1=Disc Header)
.DEFINE   CXD_DECCTL_DECMD              $07 ; Decoder Mode            (00h-07h, see below)
.DEFINE     CXD_DECCTL_DECMD_DISABLE    $00 ; or $01 - Decoder disable (to be used for CD-DA Audio mode & during config)
.DEFINE     CXD_DECCTL_DECMD_MONITOR    $02 ; or $03 - Monitor only    (read Header/Subheader, but don't write SRAM?)
.DEFINE     CXD_DECCTL_DECMD_WR_ONLY    $04 ; Write only mode (write sectors to SRAM without error correction?)
.DEFINE     CXD_DECCTL_DECMD_RTC        $05 ; Real time correction (abort correction if it takes too long?)
.DEFINE     CXD_DECCTL_DECMD_RC         $06 ; Repeat correction (allow resume via RPSTART for important sectors?)
.DEFINE     CXD_DECCTL_DECMD_INHIBIT    $07 ; Inhibit (reserved)

.DEFINE CXD_WR_INTMSK                   $04 ; Interrupt Mask
.DEFINE   CXD_INTMSK_DECINT             $10 ; Enable DecInt

.DEFINE CXD_WR_INTCLR                   $05 ; Interrupt Clear/Ack
.DEFINE   CXD_INTCLR_DECINT             $10 ; DecInt
.DEFINE   CXD_INTCLR_DMACMP             $20 ; DMA complete

.DEFINE CXD_WR_CI                       $06 ; ADPCM Coding Information
.DEFINE   CXD_CI_EMPHASIS               $40 ; ADPCM Emphasis           (0=Normal/Off, 1=Emphasis)
.DEFINE   CXD_CI_BITL4H8                $10 ; ADPCM Bit Length         (0=Normal/4bit, 1=8bit)
.DEFINE   CXD_CI_FSL3H1                 $04 ; ADPCM Sampling Frequency (0=37800Hz, 1=18900Hz)
.DEFINE   CXD_CI_MONOSTE                $01 ; ADPCM Mono/Stereo        (0=Mono, 1=Stereo)

.DEFINE CXD_WR_DMAADRC_L                $07 ; SRAM-to-CPU Xfer Address
.DEFINE CXD_WR_DMAADRC_H                $08
.DEFINE CXD_WR_DMAXFRC_L                $09 ; SRAM-to-CPU Xfer Length
.DEFINE CXD_WR_DMAXFRC_H                $0A
.DEFINE CXD_WR_DRVADRC_L                $0B ; Disc-to-SRAM Xfer Address
.DEFINE CXD_WR_DRVADRC_H                $0C
.DEFINE CXD_WR_PLBA                     $0D

.DEFINE CXD_WR_TEST2                    $1D
.DEFINE CXD_WR_TEST1                    $1E
.DEFINE CXD_WR_TEST0                    $1F
