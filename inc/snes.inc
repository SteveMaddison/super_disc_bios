;
; Constants for SNES hardware.
;

; PPU (Write-Only Ports)
.DEFINE INIDISP     $2100 ; Display Control 1
.DEFINE   INIDISP_FORCED_BLANKING   $80
.DEFINE   INIDISP_MASTER_BRIGHTNESS $0F
.DEFINE OBSEL       $2101 ; Object Size and Object Base
.DEFINE OAMADDL     $2102 ; OAM Address (lower 8bit)
.DEFINE OAMADDH     $2103 ; OAM Address (upper 1bit) and Priority Rotation
.DEFINE OAMDATA     $2104 ; OAM Data Write (write-twice)
.DEFINE BGMODE      $2105 ; BG Mode and BG Character Size
.DEFINE MOSAIC      $2106 ; Mosaic Size and Mosaic Enable
.DEFINE BG1SC       $2107 ; BG1 Screen Base and Screen Size
.DEFINE BG2SC       $2108 ; BG2 Screen Base and Screen Size
.DEFINE BG3SC       $2109 ; BG3 Screen Base and Screen Size
.DEFINE BG4SC       $210A ; BG4 Screen Base and Screen Size
.DEFINE BG12NBA     $210B ; BG Character Data Area Designation
.DEFINE BG34NBA     $210C ; BG Character Data Area Designation
.DEFINE BG1HOFS     $210D ; BG1 Horizontal Scroll (X)
.DEFINE BG1VOFS     $210E ; BG1 Vertical Scroll (Y)
.DEFINE BG2HOFS     $210F ; BG2 Horizontal Scroll (X)
.DEFINE BG2VOFS     $2110 ; BG2 Vertical Scroll (Y)
.DEFINE BG3HOFS     $2111 ; BG3 Horizontal Scroll (X)
.DEFINE BG3VOFS     $2112 ; BG3 Vertical Scroll (Y)
.DEFINE BG4HOFS     $2113 ; BG4 Horizontal Scroll (X)
.DEFINE BG4VOFS     $2114 ; BG4 Vertical Scroll (Y)
.DEFINE VMAIN       $2115 ; VRAM Address Increment Mode
.DEFINE VMADDL      $2116 ; VRAM Address
.DEFINE VMADDH      $2117
.DEFINE VMDATAL     $2118 ; VRAM Data Write
.DEFINE VMDATAH     $2119
.DEFINE M7SEL       $211A ; Rotation/Scaling Mode Settings
.DEFINE M7A         $211B ; Rotation/Scaling Parameter A & Maths 16bit operand
.DEFINE M7B         $211C ; Rotation/Scaling Parameter B & Maths 8bit operand
.DEFINE M7C         $211D ; Rotation/Scaling Parameter C
.DEFINE M7D         $211E ; Rotation/Scaling Parameter D
.DEFINE M7X         $211F ; Rotation/Scaling Center Coordinate X
.DEFINE M7Y         $2120 ; Rotation/Scaling Center Coordinate Y
.DEFINE CGADD       $2121 ; Palette CGRAM Address
.DEFINE CGDATA      $2122 ; Palette CGRAM Data Write
.DEFINE W12SEL      $2123 ; Window BG1/BG2 Mask Settings
.DEFINE W34SEL      $2124 ; Window BG3/BG4 Mask Settings
.DEFINE WOBJSEL     $2125 ; Window OBJ/MATH Mask Settings
.DEFINE WH0         $2126 ; Window 1 Left Position (X1)
.DEFINE WH1         $2127 ; Window 1 Right Position (X2)
.DEFINE WH2         $2128 ; Window 2 Left Position (X1)
.DEFINE WH3         $2129 ; Window 2 Right Position (X2)
.DEFINE WBGLOG      $212A ; Window 1/2 Mask Logic (BG1-BG4)
.DEFINE WOBJLOG     $212B ; Window 1/2 Mask Logic (OBJ/MATH)
.DEFINE TM          $212C ; Main Screen Designation
.DEFINE TS          $212D ; Sub Screen Designation
.DEFINE TMW         $212E ; Window Area Main Screen Disable
.DEFINE TSW         $212F ; Window Area Sub Screen Disable
.DEFINE CGWSEL      $2130 ; Color Math Control Register A
.DEFINE CGADSUB     $2131 ; Color Math Control Register B
.DEFINE COLDATA     $2132 ; Color Math Sub Screen Backdrop Color
.DEFINE SETINI      $2133 ; Display Control 2

; PPU (Read-Only Ports)
.DEFINE MPYL        $2134 ; PPU1 Signed Multiply Result
.DEFINE MPYM        $2135 ; PPU1 Signed Multiply Result
.DEFINE MPYH        $2136 ; PPU1 Signed Multiply Result
.DEFINE SLHV        $2137 ; PPU1 Latch H/V-Counter by Software
.DEFINE RDOAM       $2138 ; PPU1 OAM Data Read
.DEFINE RDVRAML     $2139 ; PPU1 VRAM Data Read
.DEFINE RDVRAM      $213A ; PPU1 VRAM Data Read
.DEFINE RDCGRAM     $213B ; PPU2 CGRAM Data Read (Palette)
.DEFINE OPHCT       $213C ; PPU2 Horizontal Counter Latch
.DEFINE OPVCT       $213D ; PPU2 Vertical Counter Latch
.DEFINE STAT77      $213E ; PPU1 Status and PPU1 Version Number
.DEFINE STAT78      $213F ; PPU2 Status and PPU2 Version Number

; APU Audio Processing Unit (R/W)
.DEFINE APUI00      $2140 ; CPU to audio CPU Comm 0
.DEFINE APUI01      $2141 ; CPU to audio CPU Comm 1
.DEFINE APUI02      $2142 ; CPU to audio CPU Comm 2
.DEFINE APUI03      $2143 ; CPU to audio CPU Comm 3

; WRAM Access
.DEFINE WMDATA      $2180 ; WRAM Data Read/Write
.DEFINE WMADDL      $2181 ; WRAM Address
.DEFINE WMADDM      $2182
.DEFINE WMADDH      $2183

; CPU On-Chip I/O Ports
.DEFINE JOYWR       $4016h ; Joypad Output (Write)
.DEFINE JOYA        $4016h ; Joypad Input Register A (Read)
.DEFINE JOYB        $4017h ; Joypad Input Register B (Read)

.DEFINE NMITIMEN    $4200 ; Interrupt Enable and Joypad Request
.DEFINE   NMITIMEN_VBLANK  $80
.DEFINE   NMITIMEN_HV_IRQ  $30
.DEFINE   NMITIMEN_JOYPAD  $01
.DEFINE WRIO        $4201 ; Joypad Programmable I/O Port
.DEFINE WRMPYA      $4202 ; Set unsigned 8bit Multiplicand
.DEFINE WRMPYB      $4203 ; Set unsigned 8bit Multiplier and Start Multiplication
.DEFINE WRDIVL      $4204 ; Set unsigned 16bit Dividend (lower 8bit)
.DEFINE WRDIVH      $4205 ; Set unsigned 16bit Dividend (upper 8bit)
.DEFINE WRDIVB      $4206 ; Set unsigned 8bit Divisor and Start Division
.DEFINE HTIMEL      $4207 ; H-Count Timer Setting (lower 8bits)
.DEFINE HTIMEH      $4208 ; H-Count Timer Setting (upper 1bit)
.DEFINE VTIMEL      $4209 ; V-Count Timer Setting (lower 8bits)
.DEFINE VTIMEH      $420A ; V-Count Timer Setting (upper 1bit)
.DEFINE MDMAEN      $420B ; Select General Purpose DMA Channel(s) and Start Transfer
.DEFINE   MDMAEN_CHANNEL0   $01
.DEFINE   MDMAEN_CHANNEL1   $02
.DEFINE   MDMAEN_CHANNEL2   $04
.DEFINE   MDMAEN_CHANNEL3   $08
.DEFINE   MDMAEN_CHANNEL4   $10
.DEFINE   MDMAEN_CHANNEL5   $20
.DEFINE   MDMAEN_CHANNEL6   $40
.DEFINE   MDMAEN_CHANNEL7   $80
.DEFINE HDMAEN      $420C ; Select H-Blank DMA (H-DMA) Channel(s)
.DEFINE MEMSEL      $420D ; Memory-2 Waitstate Control

.DEFINE RDNMI       $4210 ; V-Blank NMI Flag and CPU Version Number
.DEFINE TIMEUP      $4211 ; H/V-Timer IRQ Flag
.DEFINE HVBJOY      $4212 ; H/V-Blank flag and Joypad Busy flag
.DEFINE RDIO        $4213 ; Joypad Programmable I/O Port
.DEFINE RDDIVL      $4214 ; Unsigned Division Result (Quotient)
.DEFINE RDDIVH      $4215
.DEFINE RDMPYL      $4216 ; Unsigned Division Remainder / Multiply Product
.DEFINE RDMPYH      $4217
.DEFINE JOY1L       $4218 ; Joypad 1
.DEFINE JOY1H       $4219
.DEFINE JOY2L       $421A ; Joypad 2
.DEFINE JOY2H       $421B
.DEFINE JOY3L       $421C ; Joypad 3
.DEFINE JOY3H       $421D
.DEFINE JOY4L       $421E ; Joypad 4
.DEFINE JOY4H       $421F

; CPU DMA (channel 0..7)
.DEFINE DMAP0       $4300 ; DMA/HDMA Parameters
.DEFINE BBAD0       $4301 ; DMA/HDMA I/O-Bus Address (PPU-Bus aka B-Bus)
.DEFINE A1T0L       $4302 ; HDMA Table Start Address / DMA Curr Addr
.DEFINE A1T0H       $4303
.DEFINE A1B0        $4304 ; HDMA Table Start Address / DMA Curr Addr Bank
.DEFINE DAS0L       $4305 ; Indirect HDMA Address / DMA Byte-Counter
.DEFINE DAS0H       $4306
.DEFINE DASB0       $4307 ; " Bank
.DEFINE A2A0L       $4308 ; HDMA Table Current Address
.DEFINE A2A0H       $4309
.DEFINE NTRL0       $430A ; HDMA Line Counter

.DEFINE DMAP1       $4310 ; DMA/HDMA Parameters
.DEFINE BBAD1       $4311 ; DMA/HDMA I/O-Bus Address (PPU-Bus aka B-Bus)
.DEFINE A1T1L       $4312 ; HDMA Table Start Address / DMA Curr Addr
.DEFINE A1T1H       $4313
.DEFINE A1B1        $4314 ; HDMA Table Start Address / DMA Curr Addr Bank
.DEFINE DAS1L       $4315 ; Indirect HDMA Address / DMA Byte-Counter
.DEFINE DAS1H       $4316
.DEFINE DASB1       $4317 ; " Bank
.DEFINE A2A1L       $4318 ; HDMA Table Current Address
.DEFINE A2A1H       $4319
.DEFINE NTRL1       $431A ; HDMA Line Counter

.DEFINE DMAP2       $4320 ; DMA/HDMA Parameters
.DEFINE BBAD2       $4321 ; DMA/HDMA I/O-Bus Address (PPU-Bus aka B-Bus)
.DEFINE A1T2L       $4322 ; HDMA Table Start Address / DMA Curr Addr
.DEFINE A1T2H       $4323
.DEFINE A1B2        $4324 ; HDMA Table Start Address / DMA Curr Addr Bank
.DEFINE DAS2L       $4325 ; Indirect HDMA Address / DMA Byte-Counter
.DEFINE DAS2H       $4326
.DEFINE DASB2       $4327 ; " Bank
.DEFINE A2A2L       $4328 ; HDMA Table Current Address
.DEFINE A2A2H       $4329
.DEFINE NTRL2       $432A ; HDMA Line Counter

.DEFINE DMAP3       $4330 ; DMA/HDMA Parameters
.DEFINE BBAD3       $4331 ; DMA/HDMA I/O-Bus Address (PPU-Bus aka B-Bus)
.DEFINE A1T3L       $4332 ; HDMA Table Start Address / DMA Curr Addr
.DEFINE A1T3H       $4333
.DEFINE A1B3        $4334 ; HDMA Table Start Address / DMA Curr Addr Bank
.DEFINE DAS3L       $4335 ; Indirect HDMA Address / DMA Byte-Counter
.DEFINE DAS3H       $4336
.DEFINE DASB3       $4337 ; " Bank
.DEFINE A2A3L       $4338 ; HDMA Table Current Address
.DEFINE A2A3H       $4339
.DEFINE NTRL3       $433A ; HDMA Line Counter

.DEFINE DMAP4       $4340 ; DMA/HDMA Parameters
.DEFINE BBAD4       $4341 ; DMA/HDMA I/O-Bus Address (PPU-Bus aka B-Bus)
.DEFINE A1T4L       $4342 ; HDMA Table Start Address / DMA Curr Addr
.DEFINE A1T4H       $4343
.DEFINE A1B4        $4344 ; HDMA Table Start Address / DMA Curr Addr Bank
.DEFINE DAS4L       $4345 ; Indirect HDMA Address / DMA Byte-Counter
.DEFINE DAS4H       $4346
.DEFINE DASB4       $4347 ; " Bank
.DEFINE A2A4L       $4348 ; HDMA Table Current Address
.DEFINE A2A4H       $4349
.DEFINE NTRL4       $434A ; HDMA Line Counter

.DEFINE DMAP5       $4350 ; DMA/HDMA Parameters
.DEFINE BBAD5       $4351 ; DMA/HDMA I/O-Bus Address (PPU-Bus aka B-Bus)
.DEFINE A1T5L       $4352 ; HDMA Table Start Address / DMA Curr Addr
.DEFINE A1T5H       $4353
.DEFINE A1B5        $4354 ; HDMA Table Start Address / DMA Curr Addr Bank
.DEFINE DAS5L       $4355 ; Indirect HDMA Address / DMA Byte-Counter
.DEFINE DAS5H       $4356
.DEFINE DASB5       $4357 ; " Bank
.DEFINE A2A5L       $4358 ; HDMA Table Current Address
.DEFINE A2A5H       $4359
.DEFINE NTRL5       $435A ; HDMA Line Counter

.DEFINE DMAP6       $4360 ; DMA/HDMA Parameters
.DEFINE BBAD6       $4361 ; DMA/HDMA I/O-Bus Address (PPU-Bus aka B-Bus)
.DEFINE A1T6L       $4362 ; HDMA Table Start Address / DMA Curr Addr
.DEFINE A1T6H       $4363
.DEFINE A1B6        $4364 ; HDMA Table Start Address / DMA Curr Addr Bank
.DEFINE DAS6L       $4365 ; Indirect HDMA Address / DMA Byte-Counter
.DEFINE DAS6H       $4366
.DEFINE DASB6       $4367 ; " Bank
.DEFINE A2A6L       $4368 ; HDMA Table Current Address
.DEFINE A2A6H       $4369
.DEFINE NTRL6       $436A ; HDMA Line Counter

.DEFINE DMAP7       $4370 ; DMA/HDMA Parameters
.DEFINE BBAD7       $4371 ; DMA/HDMA I/O-Bus Address (PPU-Bus aka B-Bus)
.DEFINE A1T7L       $4372 ; HDMA Table Start Address / DMA Curr Addr
.DEFINE A1T7H       $4373
.DEFINE A1B7        $4374 ; HDMA Table Start Address / DMA Curr Addr Bank
.DEFINE DAS7L       $4375 ; Indirect HDMA Address / DMA Byte-Counter
.DEFINE DAS7H       $4376
.DEFINE DASB7       $4377 ; " Bank
.DEFINE A2A7L       $4378 ; HDMA Table Current Address
.DEFINE A2A7H       $4379
.DEFINE NTRL7       $437A ; HDMA Line Counter

.DEFINE DMAP_BBUS_TO_CPU  $80
.DEFINE DMAP_CPU_TO_BBUS  $00
.DEFINE DMAP_SINGLE_BYTES $01

.DEFINE OAM_SIZE          (512 + 32)

; Joypad status
.DEFINE JOYPAD_L_A        $80
.DEFINE JOYPAD_L_X        $40
.DEFINE JOYPAD_L_L        $20
.DEFINE JOYPAD_L_R        $10

.DEFINE JOYPAD_H_B        $80
.DEFINE JOYPAD_H_Y        $40
.DEFINE JOYPAD_H_SELECT   $20
.DEFINE JOYPAD_H_START    $10
.DEFINE JOYPAD_H_UP       $08
.DEFINE JOYPAD_H_DOWN     $04
.DEFINE JOYPAD_H_LEFT     $02
.DEFINE JOYPAD_H_RIGHT    $01
