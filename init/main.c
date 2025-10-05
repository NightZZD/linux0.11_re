/*
 *  linux/init/main.c
 *
 *  (C) 1991  Linus Torvalds
 */

#define __LIBRARY__
#include <unistd.h>
#include <time.h>

/*
 * we need this inline - forking from kernel space will result
 * in NO COPY ON WRITE (!!!), until an execve is executed. This
 * is no problem, but for the stack. This is handled by not letting
 * main() use the stack at all after fork(). Thus, no function
 * calls - which means inline code for fork too, as otherwise we
 * would use the stack upon exit from 'fork()'.
 *
 * Actually only pause and fork are needed inline, so that there
 * won't be any messing with the stack from main(), but we define
 * some others too.
 */
static inline _syscall0(int,fork)
static inline _syscall0(int,pause)
static inline _syscall1(int,setup,void *,BIOS)
static inline _syscall0(int,sync)

#include <linux/tty.h>
#include <linux/sched.h>
#include <linux/head.h>
#include <asm/system.h>
#include <asm/io.h>

#include <stddef.h>
#include <stdarg.h>
#include <unistd.h>
#include <fcntl.h>
#include <sys/types.h>

#include <linux/fs.h>

static char printbuf[1024];

extern int vsprintf();
extern void init(void);
extern void blk_dev_init(void);
extern void chr_dev_init(void);
extern void hd_init(void);
extern void floppy_init(void);
extern void mem_init(long start, long end);
extern long rd_init(long mem_start, int length);
extern long kernel_mktime(struct tm * tm);
extern long startup_time;

/*
 * This is set up by the setup-routine at boot-time
 */
#define EXT_MEM_K (*(unsigned short *)0x90002)
#define DRIVE_INFO (*(struct drive_info *)0x90080)
#define ORIG_ROOT_DEV (*(unsigned short *)0x901FC)

/*
 * Yeah, yeah, it's ugly, but I cannot find how to do this correctly
 * and this seems to work. I anybody has more info on the real-time
 * clock I'd be interested. Most of this was trial and error, and some
 * bios-listing reading. Urghh.
 */

#define CMOS_READ(addr) ({ \
outb_p(0x80|addr,0x70); \
inb_p(0x71); \
})

#define BCD_TO_BIN(val) ((val)=((val)&15) + ((val)>>4)*10)

static void time_init(void)
{
	struct tm time;

	do {
		time.tm_sec = CMOS_READ(0);
		time.tm_min = CMOS_READ(2);
		time.tm_hour = CMOS_READ(4);
		time.tm_mday = CMOS_READ(7);
		time.tm_mon = CMOS_READ(8);
		time.tm_year = CMOS_READ(9);
	} while (time.tm_sec != CMOS_READ(0));
	BCD_TO_BIN(time.tm_sec);
	BCD_TO_BIN(time.tm_min);
	BCD_TO_BIN(time.tm_hour);
	BCD_TO_BIN(time.tm_mday);
	BCD_TO_BIN(time.tm_mon);
	BCD_TO_BIN(time.tm_year);
	time.tm_mon--;
	startup_time = kernel_mktime(&time);
}

static long memory_end = 0;
static long buffer_memory_end = 0;
static long main_memory_start = 0;

struct drive_info { char dummy[32]; } drive_info;

void main(void)		/* This really IS void, no error here. */
{			/* The startup routine assumes (well, ...) this */
/*
 * Interrupts are still disabled. Do necessary setups, then
 * enable them
 */
 	ROOT_DEV = ORIG_ROOT_DEV; // #define ORIG_ROOT_DEV (*(unsigned short *)0x901FC)
	/*
	ORIG_ROOT_DEV 是从地址 0x901FC 读取一个 unsigned short
	*/
	// struct drive_info { char dummy[32]; } drive_info;
 	drive_info = DRIVE_INFO; // #define DRIVE_INFO (*(struct drive_info *)0x90080)
	/*
	从内存地址 0x90080 处读取一个 struct drive_info 类型的数据。
	*/
	memory_end = (1<<20) + (EXT_MEM_K<<10); // #define EXT_MEM_K (*(unsigned short *)0x90002)
	/*
	从地址 0x90002 读取的一个值，表示“扩展内存大小”，单位是 KB。
	(1<<20) 是 2^20 字节 = 1048576 字节 = 1MB。
	这是 PC 的基本内存（base memory）。
	总内存大小 = 1MB 基本内存 + 扩展内存（KB 转字节）
	*/
	memory_end &= 0xfffff000; // 内存结束地址与4KB对齐
	/*
	0xfffff000 在二进制中是很多个1后面跟着12个0。这和内存对齐有关。
	2^12 B = 4 KB
	将 memory_end 向下对齐到 4KB 边界。
	x86 架构的页大小是 4KB（2^12 字节）。
	这样后续的内存分配和页表管理才能正常工作。
	*/
	if (memory_end > 16*1024*1024)
		memory_end = 16*1024*1024;
	/*
	16MB = 2^24 字节
	这意味着地址需要用 24 位来表示
	早期的系统设计中，某些硬件（如 DMA 控制器）只能寻址前 16MB 内存
	即使机器有更多内存，内核也只使用前 16MB，以避免与硬件兼容性问题
	在功能与稳定性之间取得平衡
	*/
	if (memory_end > 12*1024*1024) 
		buffer_memory_end = 4*1024*1024;
	else if (memory_end > 6*1024*1024)
		buffer_memory_end = 2*1024*1024;
	else
		buffer_memory_end = 1*1024*1024;
	/*
	根据物理内存大小，决定给“缓冲区”（buffer）分配多少内存
	内存越大，缓冲区也越大
	*/
	main_memory_start = buffer_memory_end;
#ifdef RAMDISK
	/*
	RAMDISK 不是物理硬盘，而是一个 用内存模拟 的硬盘。
	把一块内存区域假装成硬盘来用。
	虽然它像硬盘一样可以读写文件，但因为是内存，所以速度非常快，断电后数据会丢失。
	在系统刚启动时，真正的硬盘可能还没有准备好，或者根文件系统（root filesystem）需要从一个临时的地方加载。
	所以，RAMDISK 常被用作 临时的根文件系统。
	*/
	main_memory_start += rd_init(main_memory_start, RAMDISK*1024);
	/*
	rd_init 会初始化 RAMDISK，并返回它占用的内存大小（以字节为单位）。
	然后把这个大小加到 main_memory_start 上，
	意味着主内存的起始位置向后移动，避让出 RAMDISK 所需的空间。
	在内存里划出几块区域：
		RAMDISK（如果启用）
		缓冲区（buffer）
		主内存（给进程用）
	*/
#endif
	mem_init(main_memory_start,memory_end);
	/*
	初始化内存管理子系统，告诉内核哪些内存可以分配给进程使用
	几乎所有后续功能都需要内存，所以它必须早初始化。
	*/
	trap_init();
	/*
	初始化中断向量表（比如除零、缺页等异常处理）。
	此时中断还没打开，所以可以安全设置。
	*/
	blk_dev_init();
	chr_dev_init();
	/*
	初始化块设备和字符设备。
	设备驱动可能需要分配内存，所以必须在 mem_init 之后。
	*/
	tty_init();
	/*
	tty 是 "teletype" 的缩写，意思是“电传打字机”。
	在早期计算机中，用户通过一种叫“电传打字机”的设备和计算机交互
	现在，tty 已经成为终端设备的统称。
	作用是：初始化系统的终端子系统，让内核能够处理键盘输入和屏幕输出。
	*/
	time_init();
	/*
	它初始化系统的时间和定时器，比如：
	读取 CMOS 时钟获取当前时间
	设置定时器中断（如每 10ms 一次），用于进程调度、延时等 
	*/
	sched_init();
	/*
	进程调度器的初始化。
	它设置好调度所需的数据结构，比如：
	当前运行的进程指针
	进程状态管理
	之后系统才能进行多任务切换。
	*/
	buffer_init(buffer_memory_end);
	/*
	初始化缓冲区缓存（buffer cache），用于临时存储从硬盘读写的数据块。
	buffer_memory_end 告诉它：缓冲区的上限在哪里。
	这能提高 I/O 性能，避免频繁访问慢速硬盘。
	*/
	hd_init();
	floppy_init();
	/*
	分别初始化：
	hd_init()：硬盘（Hard Disk）
	floppy_init()：软盘（Floppy Disk）
	它们设置设备控制器，注册中断处理程序，为后续的块设备读写做准备。
	*/
	sti();
	/*
	“set interrupt flag”，开启中断。 
	必须等到所有中断处理程序（如 trap、设备中断）都注册好了，才能开中断。
	否则，一旦中断到来，系统可能无法处理。
	*/
	move_to_user_mode();
	if (!fork()) {		/* we count on this going ok */
		init(); // 如果是子进程，就执行 init()
	}
	/*
	fork() 的作用是创建一个子进程，它会复制当前进程（即内核进程0）。
	fork 出 init 进程（PID=1）
	进程0（task0）是特殊的空闲进程（idle process）
	它不能直接变成 init 进程，
	否则，系统就没有空闲进程了。
	所以，必须创建一个新进程来运行 init，
	而原来的进程0继续做它该做的事。
	*/
/*
 *   NOTE!!   For any other task 'pause()' would mean we have to get a
 * signal to awaken, but task0 is the sole exception (see 'schedule()')
 * as task 0 gets activated at every idle moment (when no other tasks
 * can run). For task0 'pause()' just means we go check if some other
 * task can run, and if not we return here.
 */
	for(;;) pause();
	/*
	让当前进程进入睡眠状态，直到有信号唤醒它。
	进程0是唯一一个可以被 pause() 唤醒的特殊进程。
	在 schedule() 调度器中，当没有其他进程可运行时，就会调度进程0。
	让进程0进入空闲状态，
	当系统没有其他任务时，就“回来”运行它，从而避免 CPU 空跑。
	主动让出CPU，但随时准备被调度器召回。
	对任务0来说，pause() 只是去检查有没有别的任务能运行，
	如果没有，就回来继续循环。
	当所有其他进程都在睡眠或阻塞时，schedule() 最终会选择进程0来运行。
	即使进程0调用了 pause() 进入睡眠，调度器还是会把它选回来。
	这个机制确保了：
	CPU 在空闲时不忙等（不浪费资源）
	系统能及时响应新任务（一旦有进程可运行，调度器就不会选进程0）
	for(;;) → pause() → 睡眠 → 被调度器选中 → 醒来（pause() 返回）
	→ 回到 for(;;) → 再次 pause() → ...
	1.进入 for(;;) 循环
	2.执行 pause()
		进程0进入睡眠状态
		调用 schedule() 让出CPU
	3.由于没有其他进程可运行，调度器选择进程0再次运行
		进程0“醒来”，从	pause() 返回
	4.回到 for(;;) 的开头，再次执行 pause()
	重复步骤2-4
	*/
}

static int printf(const char *fmt, ...)
{
	va_list args;
	int i;

	va_start(args, fmt);
	write(1,printbuf,i=vsprintf(printbuf, fmt, args));
	va_end(args);
	return i;
}

static char * argv_rc[] = { "/bin/sh", NULL };
static char * envp_rc[] = { "HOME=/", NULL };

static char * argv[] = { "-/bin/sh",NULL };
static char * envp[] = { "HOME=/usr/root", NULL };

void init(void)
{
	int pid,i;

	setup((void *) &drive_info);
	(void) open("/dev/tty0",O_RDWR,0);
	(void) dup(0);
	(void) dup(0);
	printf("%d buffers = %d bytes buffer space\n\r",NR_BUFFERS,
		NR_BUFFERS*BLOCK_SIZE);
	printf("Free mem: %d bytes\n\r",memory_end-main_memory_start);
	if (!(pid=fork())) {
		close(0);
		if (open("/etc/rc",O_RDONLY,0))
			_exit(1);
		execve("/bin/sh",argv_rc,envp_rc);
		_exit(2);
	}
	if (pid>0)
		while (pid != wait(&i))
			/* nothing */;
	while (1) {
		if ((pid=fork())<0) {
			printf("Fork failed in init\r\n");
			continue;
		}
		if (!pid) {
			close(0);close(1);close(2);
			setsid();
			(void) open("/dev/tty0",O_RDWR,0);
			(void) dup(0);
			(void) dup(0);
			_exit(execve("/bin/sh",argv,envp));
		}
		while (1)
			if (pid == wait(&i))
				break;
		printf("\n\rchild %d died with code %04x\n\r",pid,i);
		sync();
	}
	_exit(0);	/* NOTE! _exit, not exit() */
}
