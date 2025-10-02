!
! SYS_SIZE is the number of clicks (16 bytes) to be loaded.
! 0x3000 is 0x30000 bytes = 196kB, more than enough for current
! versions of linux
!
SYSSIZE = 0x3000
!
!	bootsect.s		(C) 1991 Linus Torvalds
!
! bootsect.s is loaded at 0x7c00 by the bios-startup routines, and moves
! iself out of the way to address 0x90000, and jumps there.
!
! It then loads 'setup' directly after itself (0x90200), and the system
! at 0x10000, using BIOS interrupts. 
!
! NOTE! currently system is at most 8*65536 bytes long. This should be no
! problem, even in the future. I want to keep it simple. This 512 kB
! kernel size should be enough, especially as this doesn't contain the
! buffer cache as in minix
!
! The loader has been made as simple as possible, and continuos
! read errors will result in a unbreakable loop. Reboot by hand. It
! loads pretty fast by getting whole sectors at a time whenever possible.

.globl begtext, begdata, begbss, endtext, enddata, endbss
.text
begtext:
.data
begdata:
.bss
begbss:
.text

SETUPLEN = 4				! nr of setup-sectors
BOOTSEG  = 0x07c0			! original address of boot-sector
INITSEG  = 0x9000			! we move boot here - out of the way
SETUPSEG = 0x9020			! setup starts here
SYSSEG   = 0x1000			! system loaded at 0x10000 (65536).
ENDSEG   = SYSSEG + SYSSIZE		! where to stop loading

! ROOT_DEV:	0x000 - same type of floppy as boot.
!		0x301 - first partition on first drive etc
ROOT_DEV = 0x306

entry start
start:
	! BIOS 已经把你的代码放在了 0x7C00
	! 原理：0 盘 0 道 1 扇区的内容一共有 512 个字节，如果末尾的两个字节分别是 0x55 和 0xaa，那么 BIOS 就会认为它是个启动区; 加载，就是把这 512 个字节的内容，一个比特都不少的全部复制到内存的 0x7c00 这个位置
	! 所以只要从 0x7C00 开始执行指令。而执行指令只需设置CS:IP
	mov	ax,#BOOTSEG ; 
	mov	ds,ax ! ds此时保存的是0x07c0, 0x07c0是默认的MBR的起始地址 - 需要定位到这个地址
	mov	ax,#INITSEG 
	mov	es,ax ! es保存的是0x9000,为复制命令做准备：从 DS:SI 开始复制 256 个字（512 字节）到 ES:DI，其中SI,DI都被清零了。
	mov	cx,#256	! linus在这里写的是10进制, 为了下面重复执行256次movw指令
	sub	si,si
	sub	di,di ! 这两个指令都是清零寄存器
	rep movw ! rep 是一个 指令前缀，表示“重复执行下一条字符串操作指令”，直到 CX 寄存器为 0
	! 执行 256 次 movw（每次复制一个字，2 字节） 总共复制：256 × 2 = 512 字节（一个扇区大小）. 从 DS:SI 开始复制 256 个字（512 字节）到 ES:DI
	! DS:SI = 0x07C00 + 0 = 0x07c00 = 31744 字节处
	! ES:SI = 0X90000 + 0 = 0X90000 = 
	jmpi	go,INITSEG ! jmpi offset目标代码在段内的偏移地址, segment目标代码所在的段地址
	! 这是一条 远跳转（far jump） 指令，它会同时设置：CS = INITSEG = 0x9000 IP = go 的偏移（比如 0x20）
go:	mov	ax,cs ! 此时CS是0X90000，IP指向go标号的地址
	mov	ds,ax
	mov	es,ax
! put stack at 0x9ff00.
	mov	ss,ax ! SS 指向 0x9000,SS:IP 指向 0x9ff00
	mov	sp,#0xFF00		! arbitrary value >>512

! load the setup-sectors directly after the bootblock.
! Note that 'es' is already set up.

load_setup:
	mov	dx,#0x0000		! drive 0, head 0 ; mov dh, 0 ; 磁头号（Head） mov dl, 0 ; 驱动器号：0 = 软盘 A
	mov	cx,#0x0002		! sector 2, track 0 ; mov ch, 0 ; 柱面号（Cylinder）mov cl, 2 ; 扇区号（Sector）。2其实表示第二个扇区
	; IBM PC BIOS 规范（如 INT 13h）明确规定： “The sector number is 1-based. Sector numbering starts at 1.”
	mov	bx,#0x0200		! address = 512, in INITSEG
	mov	ax,#0x0200+SETUPLEN	! service 2, nr of sectors ; 0x0204 : mov ah, 0x02 ; 功能号：读取扇区 mov al, 4; 要读取的扇区数
	int	0x13			! read it
	jnc	ok_load_setup	! ok - continue ; 如果成功（没有出错，CF=0）→ jnc ok_load_setup → 继续引导
	mov	dx,#0x0000		; DH=0: 磁头 0; DL=0: 选择 A: 软驱 ; DL 寄存器用于指定要操作的磁盘驱动器
	mov	ax,#0x0000		! reset the diskette ;mov ah, 0x00 ; 功能号：磁盘系统复位（Reset Disk System）; AL = 0x00 → 在此功能中通常无意义（可忽略）
	int	0x13
	j	load_setup		; 现在磁盘已经复位，操作失败，重新尝试读取。这里会一直重试，也许会卡住

ok_load_setup:

! Get disk drive parameters, specifically nr of sectors/track

	mov	dl,#0x00		; DL=0: 选择 A: 软驱
	mov	ax,#0x0800		! AH=8 is get drive parameters ; AL 未使用（此处为 0）
	int	0x13
	; 返回值（在寄存器中）：
	; CH = 柱面数（最大柱面号，从 0 开始）
	; CL = 最大扇区号（即每磁道扇区数）
	; DH = 最大磁头号
	; DL = 实际检测到的驱动器数量（如 1 表示有 A:）
	; 	对于 1.44MB 软盘：
	; CH = 79（80 个柱面）
	; CL = 18（每磁道 18 个扇区）
	; DH = 1（2 个磁头，双面）
	; DL = 1（1 个软驱）
	mov	ch,#0x00 ; 将 CH 清零。原因：CH 原本是柱面号（最高 79），但我们只关心 CL 中的扇区数。
	; 清零 CH 是为了后续将 CX 作为一个整体使用（CH=0, CL=扇区数），便于存储。
	seg cs ; 表示“在代码段（CS）中操作”
	mov	sectors,cx ; 将 CX 寄存器的值（即 CL 中的每磁道扇区数）存入变量 sectors
	; sectors 是一个在引导代码中定义的内存变量，用于后续计算磁盘读取时的 CHS 地址。
	; CHS : Cylinder-Head-Sector 柱面 - 磁头 - 扇区
	; 作用：保存软盘每磁道的扇区数，供后续读取内核时使用。
	mov	ax,#INITSEG
	mov	es,ax
	; 将 ES 段寄存器设置为 0x9000
	; 原因：前面 int 0x13 可能会修改 ES（虽然 AH=8 通常不会），这里恢复 ES 到正确的段，确保后续内存操作正确。
	; 在 bootsect 被加载到 0x7C00 后，通常会将其移动到 0x90000（即 0x9000:0x0000），所以 INITSEG = 0x9000

! Print some inane message

	mov	ah,#0x03		! read cursor pos
	xor	bh,bh			! page 0
	int	0x10			
	; 功能：读取当前光标位置
	; 返回 DH=行（0-based）, DL=列, CX = 光标形状
	mov	cx,#24
	mov	bx,#0x0007		! page 0, attribute 7 (normal)
	mov	bp,#msg1
	mov	ax,#0x1301		! write string, move cursor
	int	0x10

! ok, we've written the message, now
! we want to load the system (at 0x10000)

	mov	ax,#SYSSEG
	mov	es,ax		! segment of 0x010000
	call	read_it
	call	kill_motor

! After that we check which root-device to use. If the device is
! defined (!= 0), nothing is done and the given device is used.
! Otherwise, either /dev/PS0 (2,28) or /dev/at0 (2,8), depending
! on the number of sectors that the BIOS reports currently.

	seg cs
	mov	ax,root_dev
	cmp	ax,#0
	jne	root_defined
	seg cs
	mov	bx,sectors
	mov	ax,#0x0208		! /dev/ps0 - 1.2Mb
	cmp	bx,#15
	je	root_defined
	mov	ax,#0x021c		! /dev/PS0 - 1.44Mb
	cmp	bx,#18
	je	root_defined
undef_root:
	jmp undef_root
root_defined:
	seg cs
	mov	root_dev,ax

! after that (everyting loaded), we jump to
! the setup-routine loaded directly after
! the bootblock:

	jmpi	0,SETUPSEG

! This routine loads the system at address 0x10000, making sure
! no 64kB boundaries are crossed. We try to load it as fast as
! possible, loading whole tracks whenever we can.
!
! in:	es - starting address segment (normally 0x1000)
!
sread:	.word 1+SETUPLEN	! sectors read of current track
head:	.word 0			! current head
track:	.word 0			! current track

read_it:
	mov ax,es
	test ax,#0x0fff
die:	jne die			! es must be at 64kB boundary
	xor bx,bx		! bx is starting address within segment
rp_read:
	mov ax,es
	cmp ax,#ENDSEG		! have we loaded all yet?
	jb ok1_read
	ret
ok1_read:
	seg cs
	mov ax,sectors
	sub ax,sread
	mov cx,ax
	shl cx,#9
	add cx,bx
	jnc ok2_read
	je ok2_read
	xor ax,ax
	sub ax,bx
	shr ax,#9
ok2_read:
	call read_track
	mov cx,ax
	add ax,sread
	seg cs
	cmp ax,sectors
	jne ok3_read
	mov ax,#1
	sub ax,head
	jne ok4_read
	inc track
ok4_read:
	mov head,ax
	xor ax,ax
ok3_read:
	mov sread,ax
	shl cx,#9
	add bx,cx
	jnc rp_read
	mov ax,es
	add ax,#0x1000
	mov es,ax
	xor bx,bx
	jmp rp_read

read_track:
	push ax
	push bx
	push cx
	push dx
	mov dx,track
	mov cx,sread
	inc cx
	mov ch,dl
	mov dx,head
	mov dh,dl
	mov dl,#0
	and dx,#0x0100
	mov ah,#2
	int 0x13
	jc bad_rt
	pop dx
	pop cx
	pop bx
	pop ax
	ret
bad_rt:	mov ax,#0
	mov dx,#0
	int 0x13
	pop dx
	pop cx
	pop bx
	pop ax
	jmp read_track

/*
 * This procedure turns off the floppy drive motor, so
 * that we enter the kernel in a known state, and
 * don't have to worry about it later.
 */
kill_motor:
	push dx
	mov dx,#0x3f2
	mov al,#0
	outb
	pop dx
	ret

sectors:
	.word 0

msg1:
	.byte 13,10
	.ascii "Loading system ..."
	.byte 13,10,13,10

.org 508
root_dev:
	.word ROOT_DEV
boot_flag:
	.word 0xAA55

.text
endtext:
.data
enddata:
.bss
endbss:
