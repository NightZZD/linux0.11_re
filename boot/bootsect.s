!
! SYS_SIZE is the number of clicks (16 bytes) to be loaded.
! 0x3000 is 0x30000 bytes = 196kB, more than enough for current
! versions of linux
; 0x30000 bytes = 196,608 B = 196kB = 192 KiB
; 在 1991 年那个时代，这种混用非常普遍，甚至操作系统、BIOS 和磁盘厂商都经常用 KB 来指代 1024 字节，
; 但用 1000 来计算磁盘容量，造成了长期的混淆。
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
	mov	ax,#BOOTSEG ; cs = 0, ip = 0x7C00, 所以这条指令所在的物理内存地址是 cs:ip = 0x7C00
	mov	ds,ax ! ds此时保存的是0x07c0, 0x07c0是默认的MBR的起始地址 - 需要定位到这个地址
	mov	ax,#INITSEG 
	mov	es,ax ! es保存的是0x9000,为复制命令做准备：从 DS:SI 开始复制 256 个字（512 字节）到 ES:DI，其中SI,DI都被清零了。
	mov	cx,#256	! linus在这里写的是10进制, 为了下面重复执行256次movw指令
	sub	si,si
	sub	di,di ! 这两个指令都是清零寄存器
	rep movw ! rep 是一个 指令前缀，表示“重复执行下一条字符串操作指令”，直到 CX 寄存器为 0
	! 执行 256 次 movw（每次复制一个字，2 字节） 总共复制：256 × 2 = 512 字节（一个扇区大小）. 从 DS:SI 开始复制 256 个字（512 字节）到 ES:DI
	! DS:SI = 0x07C00 + 0 = 0x07c00 = 31744 字节处
	! ES:DI = 0X90000 + 0 = 0X90000 = 
	jmpi	go,INITSEG ! jmpi offset目标代码在段内的偏移地址, segment目标代码所在的段地址
	! 这是一条 远跳转（far jump） 指令，它会同时设置：CS = INITSEG = 0x9000 IP = go 的偏移（比如 0x20）
	; 把自己（位于 0x7C00）复制到 0x90000（即 0x9000:0x0000）
    ; 然后跳转过去执行
go:	mov	ax,cs ! 此时CS是0x90000，IP指向go标号的地址
	mov	ds,ax
	mov	es,ax
	; 设置ds,es指向新段地址0x90000
! put stack at 0x9ff00.
	mov	ss,ax ! SS 指向 0x9000,SS:IP 指向 0x9ff00
	mov	sp,#0xFF00		! arbitrary value >>512

! load the setup-sectors directly after the bootblock.
! Note that 'es' is already set up.

load_setup:
	mov	dx,#0x0000		! drive 0, head 0 ; mov dh, 0 ; 磁头号（Head） mov dl, 0 ; 驱动器号：0 = 软盘 A
	mov	cx,#0x0002		! sector 2, track 0 ; mov ch, 0 ; 柱面号（Cylinder）mov cl, 2 ; 扇区号（Sector）。2其实表示第二个扇区
	; IBM PC BIOS 规范（如 INT 13h）明确规定： “The sector number is 1-based. Sector numbering starts at 1.”
	; 第一扇区是bootsect.s 从第二扇区开始读取setup.s
	; 扇区 1：引导扇区（bootsect.s，被 BIOS 加载到 0x7c00）
	; 扇区 2~5：setup 模块（setup.s 编译后，约 4 个扇区 = 2048 字节）
	; 扇区 6 开始：system 模块（内核）
	mov	bx,#0x0200		! address = 512, in INITSEG
	; BIOS 将读取到的数据放到内存的哪个位置。这个目标地址由两个寄存器指定：
	; ES (Extra Segment): 目标内存的段地址。
	; BX (Base Register): 段内的偏移地址。
	; 0x90200
	mov	ax,#0x0200+SETUPLEN	! service 2, nr of sectors ; 0x0204 : mov ah, 0x02 ; 功能号：读取扇区 mov al, 4; 要读取的扇区数
	int	0x13			! read it
	; 0x90200	setup 模块 被 load_setup 读到这里
	; setup 模块 结束于：物理地址 0x90200 + 2048 = 0x90A00
	; 0x90200 + 2048	2048=512 * 4。 system 模块（内核）随后被加载
	jnc	ok_load_setup	! ok - continue ; 如果成功（没有出错，CF=0）→ jnc ok_load_setup → 继续引导
	mov	dx,#0x0000		; DH=0: 磁头 0; DL=0: 选择 A: 软驱 ; DL 寄存器用于指定要操作的磁盘驱动器
	mov	ax,#0x0000		! reset the diskette ;mov ah, 0x00 ; 功能号：磁盘系统复位（Reset Disk System）; AL = 0x00 → 在此功能中通常无意义（可忽略）
	int	0x13
	; 执行复位操作，为重新读取做准备
	j	load_setup		; 现在磁盘已经复位，操作失败，重新尝试读取。这里会一直重试，也许会卡住
; 现在setup.s加载到0x90200处，一共四个扇区
; 512 B = 2^9 B = 0x100 B
; 0x90200 - 0x902FF （包含边界）装入setup.s
; 接下来目标是把余下扇区加载到内存
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
	mov	cx,#24			; 十进制24。CX：字符串长度 ; 作用：指定要显示的字符串的长度（字符数）
	mov	bx,#0x0007		! page 0, attribute 7 (normal) ; BX 在 int 0x10 / AH=13h 中用于指定：
	;BH = 0x00 → 视频页 0（大多数系统只使用一页）
	;BL = 0x07 → 字符属性：黑底白字（正常文本）
	; 低 4 位 0x7 = 白色字体（RGB: 111）
	; 高 4 位 0x0 = 黑色背景
	mov	bp,#msg1		; BP：字符串地址
	mov	ax,#0x1301		! write string, move cursor ; AH=13h, AL=01h → 字符串地址在 ES:BP
	; AH = 0x13 → BIOS int 0x10 的功能号：Write String（写字符串）
	; AL = 0x01 → 子功能（Sub-function）：
	; AL = 01：写字符串后移动光标到字符串末尾 ; AL = 00：写字符串但不移动光标
	; AL = 01 表示“打印完字符串后，光标停在末尾”，便于后续输出。
	int	0x10


! ok, we've written the message, now
! we want to load the system (at 0x10000)

	mov	ax,#SYSSEG ; SYSSEG = 0x1000
	mov	es,ax		! segment of 0x010000
	; 物理地址 = 段地址 × 16 + 偏移
	; 0x1000 × 16 = 0x10000 → 即 16^4 = 2^16 B = 64 * 1024 B = 64KB 处
	call	read_it ; 读取 setup 和内核到 0x10000
	call	kill_motor ; 关闭软盘马达

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
; 定义在代码段中的变量
head:	.word 0			! current head
track:	.word 0			! current track

read_it:
	mov ax,es ; ES 是之前设置的目标内存段，通常是 SYSSEG = 0x1000（对应物理地址 0x10000）
	test ax,#0x0fff ;地址 `0x10000` 是 4KB 页面对齐 的（因为低 12 位为 0）。此时 `ax & 0x0FFF == 0` → **ZF = 1**
	; 0x10000 = 2^16 = 64 KB
	; 目的：验证 ES 段是否指向一个 4KB 边界对齐的物理地址，确保内存布局正确，防止因配置错误导致后续加载失败。
	; 一种防御性编程技巧，用于确保系统启动的可靠性。
die:	jne die			! es must be at 64kB boundary
	; Jump if Not Equal，即当零标志 ZF = 0 时跳转。
	; 如果 test ax, #0x0fff 的结果 不为零（即低 12 位有 1），说明 es 没有对齐到 64KB 边界 → 跳转到 die 标签。
	; 这是一种简单的“死机”或“断言失败”处理方式：如果段地址不对齐，就卡死在这里。
	xor bx,bx		! bx is starting address within segment
	; 如果 ax 低 12 位为 0 → xor bx, bx
	; 在 int0x13 BIOS 调用中，写入数据的内存地址由 es:bx 指定，这里从0处开始写
	; es = SYSSEG = 0x1000, 因为mov	ax,#SYSSEG
	; es:bx = 0x1000 << 4 + 0 = 0x100000
	; 从内存地址0x100000开始写入剩下的程序代码（软盘5扇区开始后的数据，转移到内存）
rp_read:
; "repeat read"，表示这是一个读取循环的入口。
	mov ax,es
	cmp ax,#ENDSEG		! have we loaded all yet?
	jb ok1_read
	; Jump if Below，即“如果小于则跳转”（无符号比较）。
	; 如果 ax < ENDSEG（即 es < 0x4000），说明还没有加载完，跳转到 ok1_read 标签处，继续执行加载逻辑。
	; 如果 ax >= ENDSEG，则不跳转，继续执行下一条指令。
	ret
	; 如果 es >= ENDSEG，说明：
	; 已经加载了 SYSSIZE 大小的数据（即整个内核）
	; 所有数据都已从软盘读入内存 0x10000 起始的位置
	; 此时执行 ret，从 read_it 子程序返回，回到调用它的地方：
	; call read_it
	; call kill_motor   ; ← 下一条执行的指令
ok1_read:
	seg cs ;**段前缀（segment override prefix）**
	; 它表示接下来的内存访问（如 `sectors`, `sread`）使用 `cs` 段寄存器，
	; 而不是默认的 `ds`
	; 思考，在哪里设置过cs寄存器
	; jmpi	go,INITSEG ; 
	; 将 INITSEG（即 0x9000）加载到 CS 寄存器
	; 将 go 的偏移地址（比如 0x20）加载到 IP 寄存器
	mov ax,sectors
	; 将 sectors 的值(扇区数量)加载到 ax
	; 思考，在哪里获取过扇区数量
	; ok_load_setup:
	; mov	sectors,cx ; 将 CX 寄存器的值（即 CL 中的每磁道扇区数）存入变量 sectors
	; 所以 sectors 是之前通过 BIOS 中断 int 0x13, ah=8 获取的每磁道扇区数
	sub ax,sread ; sread 表示“当前磁道上已经读取的扇区数”。
	; 初始值：1 + SETUPLEN = 1 + 4 = 5（因为 bootsect 本身占 1 扇区，setup 占 4 扇区）
	; sread: .word 1+SETUPLEN
	; 所以 ax = sectors - sread → 当前磁道上剩余的扇区数
	mov cx,ax ; cx = 剩余扇区数
	shl cx,#9 ; cx = 剩余扇区数 × 512（因为 2^9 = 512 字节/扇区）
	; shl 是 逻辑左移（Shift Left） 指令
	; cx = cx << 9 = 剩余扇区数 × 512 = 剩余扇区总字节数
	add cx,bx ; cx = 剩余数据字节数 + 当前内存偏移 → 即：最终写入地址 = bx + cx
	; 将 bx（当前内存偏移量）加到 cx 中
	; “要读的字节数” + “当前偏移”
	; 检测是否会跨越 64KB 内存段边界
	; 为了调用下面的 jnc ok2_read 做准备,因为
	; add cx, bx，它可能会导致 16位寄存器溢出，
	; 从而设置 进位标志（Carry Flag, CF）
	; 如果 cx + bx < 65536（即没有超过 64KB 段边界），
	; 不会产生进位 → CF = 0 → jnc 成立 → 跳转到 ok2_read
	jnc ok2_read ; 如果没有进位（CF == 0），跳转
	je ok2_read ; 如果结果为零（ZF == 1），跳转
	; je 处理“有进位但结果为 0”的特殊情况
	; 剩下的（CF=1 且 ZF=0）才是真正危险的“跨段中间”的情况，需要计算安全扇区数
	; 什么时候 cx + bx == 0？
	; 由于是 16 位加法，结果是模 65536 的，所以：
	; (cx + bx) mod 65536 == 0
	; 即：cx + bx 刚好等于 65536 的整数倍，最常见的是 刚好等于 65536
	; 此时，虽然 add 产生了进位（CF=1），
	; 但最终地址 刚好落在段边界上（如 0x10000）
	; 刚好对齐到 64KB 边界，仍然可以安全读取，无需限制扇区数
	xor ax,ax ; ax = 0
	sub ax,bx ; ax = 0 - bx = -bx（即 bx 的补码，相当于 65536 - bx mod 65536）
	; 补码的本质就是 模运算下的加法逆元
	; 在这里，我们先用有符号语义计算 -bx，然后把它当作无符号数看待来算容量。
	shr ax,#9 ; ax = ax >> 9 = (65536 - bx) / 512
	; 最多还能读多少个扇区而不跨段
	; 此时, ax = 本次最多可安全读取的扇区数（受限于 64KB 边界）
; 计算当前磁道上剩余的扇区数，并准备一次性读取尽可能多的数据，避免跨 64KB 内存段边界。

ok2_read:
	call read_track
	; 在成功读取一个（或一批）扇区后，
	; 更新状态变量（sread, head, track），为下一次读取做准备。
	; read_track 内部会 pop ax 恢复寄存器，
	; 所以 ax 的值是 调用 read_track 前 ax 的原始值。
	; 在 ok1_read 中，ax 被设置为 sectors - sread（当前磁道剩余扇区数）
	mov cx,ax ;将“本次计划读取的扇区数”暂存到 cx 中，用于后续计算字节数。
	add ax,sread ; ax = (sectors - sread) + sread = sectors
	; ax 原来是 sectors - sread（剩余）
	; sread 是“已读扇区数”
	seg cs
	cmp ax,sectors
	jne ok3_read ; ax == sectors，所以 不跳转，继续执行下一条
	; 判断：本次读取是否读到了当前磁道的最后一个扇区？
	; 如果 ax < sectors，说明还没读完，跳转到 ok3_read
	; 如果 ax == sectors，说明 本次读取后，当前磁道已满，需要处理磁头/磁道切换
	; 但由于 ax = sectors，所以 不跳转，进入磁头/磁道切换逻辑。
	mov ax,#1
	sub ax,head
	; 经典软盘读取策略：先读磁道 0 的磁头 0，再读磁道 0 的磁头 1，然后进入磁道 1。
	; 如果 head = 0 → ax = ax-head = 1 → jne 成立 → 跳转到 ok4_read
	; 即：当前磁头是 0：读完当前磁道后，切换到磁头 1
	; 如果 head = 1 → ax = ax-head = 0 → jne 不成立 → 继续执行
	; 即：当前磁头是 1：读完当前磁道后，不切换磁头，而是增加磁道号inc track（换道）
	; 即：继续执行 inc track
	jne ok4_read ; 当前磁道还有磁头1没有用到，继续使用磁头1
	inc track ; 只有当 head = 1 且读完当前磁道时，才会执行这句

ok4_read:
	mov head,ax ; ax保存的是切换后的磁头号
	xor ax,ax ; 清当前磁道已读扇区数。为 ok3_read 做准备
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
	; 保护现场
	mov dx,track ;当前磁道号
	mov cx,sread ;已读扇区
	; sread 初始值是 1 + SETUPLEN = 5
	;（因为 bootsect 占 1 扇区，setup 占 4 扇区）
	; cx = 5 表示：前 5 个扇区已读，下一个要读的是第 6 个扇区
	inc cx ; 指向下一个扇区
	; 将“已读数”转换为“下一个要读的扇区号”
	mov ch,dl
	; 设置中断参数
	; dl 当前保存的是 track（来自 mov dx, track）
	; ch ← dl = track
	; ch 现在正确设置了磁道号
	mov dx,head ; 将变量 head（当前磁头号）加载到 dx
	mov dh,dl ; dl = head（当前磁头号）
	; dh 现在正确设置了磁头号
	mov dl,#0 ; dl = 驱动器号（0 表示 A: 软驱）
	; 指定从第一个软盘驱动器读取
	and dx,#0x0100 ; 第 8 位保留，其余清零
	; 如果 head = 0 → dx = 0x0000 → and 后仍是 0x0000
	; 如果 head = 1 → dx = 0x0100 → and 后仍是 0x0100
	; 这行代码实际上是冗余的，对功能无影响
	mov ah,#2 ;常见功能号，从磁盘读取扇区
	int 0x13 ; 数据写入 ES:BX 指向的内存
	jc bad_rt ; bad read track
	pop dx
	pop cx
	pop bx
	pop ax
	; 恢复现场
	ret
bad_rt:	mov ax,#0 ; 常见功能号，磁盘系统复位
	mov dx,#0
	int 0x13
	pop dx
	pop cx
	pop bx
	pop ax
	; 恢复现场
	jmp read_track
	; 重新读

/*
 * This procedure turns off the floppy drive motor, so
 * that we enter the kernel in a known state, and
 * don't have to worry about it later.
 */
kill_motor:
	push dx ; 保存当前 DX 寄存器的值到栈中。
	mov dx,#0x3f2 ; 0x3f2 是 软盘控制器数字控制端口（Digital Output Register, DOR） 的 I/O 端口地址。
	mov al,#0 ; 这是要写入端口 0x3f2 的数据值。
	outb ; 执行 字节输出指令，将 AL 中的值写入 DX 指定的 I/O 端口。
	; 效果：向端口 0x3f2 写入 0x00。
	pop dx ; 恢复之前保存的 DX 寄存器原始值。
	ret ; 返回到调用者。
; 典型的 x86 实模式下的端口 I/O 操作，用于在 PC 架构中 关闭软盘驱动器（Floppy Disk Drive）的电机。

sectors:
	.word 0
; 定义在代码段中的变量
; ds:0x0139
msg1:
	.byte 13,10
	.ascii "ZZD Loading system ..."
	.byte 13,10,13,10
	; 13 = ASCII 回车符 \r（Carriage Return）
	; 10 = ASCII 换行符 \n（Line Feed）

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
