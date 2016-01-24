/************************************************************************
*
*	Device Driver for INMOS-B004/B008 compatible link interfaces
*
*	Copyright (C) 1993,94,95 by Christoph Niemann
*				 niemann@swt.ruhr-uni-bochum.de
*	based on /linux/kernel/drivers/char/lp.c
*
*	The driver may be copied under the same conditions as the
*	Linux operating system
*
*
*	Version 0.8a, for Linux 1.2.X, March 1996.
*	(1) Module support added by Mark Bowyer <mark@lindhard.demon.co.uk>
*
*	Version 0.9  for Linux 2.2.x   November 2000   Mark Bowyer
*	(1) Kernel 2.2.X support by Mark Bowyer, November 2000
*
*	Version 0.91, for Linux 2.2.X  December 2000.  -Kev
*	(1) Removed assembler code for link management. There is
*       no need for this style on fast CPU systems.
*   (2) Removed reference to link adapter mapped to I/O space 0x300
*       because it gets confused with EtherExpress Ethernet cards.
*   (3) Linked all timers to system jiffie counter. This will remove the
*       problem of CPU dependancies which don't work when faster newer
*       CPU's are running this code.
*   (4) Changed Read and Write routines to be more efficient, shorter
*       now everything is timed using jiffie counters.
*
*	Version 0.92, for Linux 2.4.X, March 2002.
*		by Mark Bowyer <m.bowyer@ieee.org>
*	(1) fixed file_operations for 2.4.X (tested on RH 2.4.9 kernel)
*	(2) removed assembler remnant in LINK_REALLY_SLOW_C012
*
*	Version 0.93, for Linux 3.X.X, November 2014.
*		by John Snowdon <john.snowdon@newcastle.ac.uk>
*	(1) updated to new Kernel kbuild scheme (tested on i386 Debian 7, 3.2.63-2+deb7u1)
*	(2) removed non-module defines (no longer possible to build into a static kernel)
*	(3) updated to current Linux kernel header file locations
*	(4) additional debugging code for monitoring data read/written to link device
*	(5) updated to new ioctl interface struct type (.ioctl replaced by .unlocked_ioctl)
*	(6) tidied up some syntax to remove warnings in modern GCC compilers
*	(7) removed MOD_DEC and MOD_INC macros - no longer supported
*       (8) removed slow link defines - unless you're using a 386/486, these are now superfluous
*	TODO: Still using old register_chrdev calls - needs to be converted to cdev
*
*************************************************************************/

/* Copyright (C) 1992 by Jim Weigand, Linus Torvalds, and Michael K. Johnson */


#ifndef __i386__
#error This driver requires the Intel architecture
#endif


#include <linux/errno.h>
#include <linux/kernel.h>
#include <linux/sched.h>
#include <linux/timer.h>
#include <linux/stat.h>
#include <linux/ioport.h>
#include <linux/linkage.h>
#include <linux/kdev_t.h>
#include <linux/fs.h>

#include <asm/io.h>
#include <asm/segment.h>
#include <asm/uaccess.h>

#include "link-driver.h"
#include <linux/module.h>
#include <linux/version.h>
#include <generated/utsrelease.h>

char kernel_version[] = UTS_RELEASE;

/*
 * Define these symbols if you want to debug the code.
 */

#define LINK_DEBUG 
#undef LINK_DEBUG_MORE

#define out		outb
#define in		inb

#ifdef LINK_DEBUG
#define DEB(x)		x
#define PRINTK(f, a...)	printk(f, ##a)
#define NO_DEB(x)
#else
#define DEB(x)
#define PRINTK(f, a...)
#define NO_DEB(x)	x
#endif

#ifdef LINK_DEBUG_MORE
#define DEB_MORE(x) x
#else
#define DEB_MORE(x)
#endif

#if !defined(SYMBOL_NAME_STR)
#if defined(__ELF__)
#define SYMBOL_NAME_STR(X) #X
#else
#define SYMBOL_NAME_STR(X) "_"#X
#endif
#endif


/**** This structure defines each interface control ****/
static struct link_struct link_table[LINK_NO] =
{
	 { LINK_READ_ABORT | LINK_WRITE_ABORT, 0, 0, 0, 0,
	 LINK_INIT_READ_TIMEOUT, LINK_INIT_WRITE_TIMEOUT, LINK_B004 },
	 { LINK_READ_ABORT | LINK_WRITE_ABORT, 0, 0, 0, 0,
	 LINK_INIT_READ_TIMEOUT, LINK_INIT_WRITE_TIMEOUT, LINK_B004 },
};

static unsigned char link_devices = 0;	/* number of devices dectected */

/**** These are the board interface IO addresses the driver will search ****/
static const short link_base_addresses[] = { 0x150, 0x170, 0x190, 0 };
static const char *link_name(dev_t minor)
{
	static char name[] = LINK_NAME"?";

	if (minor < link_devices)
	{
		name[4] = minor + '0';
		return name;
	}
	else
		return (const char*)0;
}


const char *byte_to_binary(int x)
{
    static char b[9];
    b[0] = '\0';
    int z;
    for (z = 128; z > 0; z >>= 1)
    {
        strcat(b, ((x & z) == z) ? "1" : "0");
    }
    return b;
}

/*********************************************************************
 This function is used to generate a fixed delay count of 100 milli-
 seconds. The timer is used to generate fixed delay intervals for the
 hardware reset and analyse sequences of the Transputer.

 This code replaces the original code that relied on a tight CPU loop
 to generate the delay. Unfortunately the delay does not work very
 well when CPU speeds are increased to faster and faster types.
**********************************************************************/

static void link_delay(void)
{
	unsigned int	timer;
	timer = jiffies + (100 / (1000 / HZ));
	while (1)
	{
		if (jiffies > timer){
			break;
		}
		schedule();
	}
}


/****************************************************************************
 *
 * static void link_reset():
 *
 * reset transputer network.
 *
 ****************************************************************************/

static int link_reset( int minor )
{

	PRINTK("LINK(%d) resetting transputer. (0x%x)\n", minor, LINK_RESET(minor));

	PRINTK("LINK(%d) reset() data [0x%02x]\n", minor, LINK_DEASSERT_ANALYSE);
	out( LINK_DEASSERT_ANALYSE, LINK_ANALYSE(minor));
	link_delay();

	PRINTK("LINK(%d) reset() data [0x%02x]\n", minor, LINK_DEASSERT_RESET);
	out( LINK_DEASSERT_RESET, LINK_RESET(minor));
	link_delay();

	PRINTK("LINK(%d) reset() data [0x%02x]\n", minor, LINK_ASSERT_RESET);
	out( LINK_ASSERT_RESET, LINK_RESET(minor));
	link_delay();

	PRINTK("LINK(%d) reset() data [0x%02x]\n", minor, LINK_DEASSERT_RESET);
	out( LINK_DEASSERT_RESET, LINK_RESET(minor));
	link_delay();

	DEB_MORE(printk("LINK(%d) reset() success!\n\n", minor);)
	return 0;
}

/****************************************************************************
 *
 * static void link_analyse():
 *
 * switch transputer network to analyse mode.
 *
 ****************************************************************************/

static void link_analyse( const int minor )
{

	PRINTK("LINK(%d) switching transputer to analyse mode.\n", minor);

	out( LINK_DEASSERT_ANALYSE, LINK_ANALYSE(minor));
	link_delay();

	out( LINK_ASSERT_ANALYSE, LINK_ANALYSE(minor));
	link_delay();

	out( LINK_ASSERT_RESET, LINK_RESET(minor));
	link_delay();

	out( LINK_DEASSERT_RESET, LINK_RESET(minor));
	link_delay();

	out( LINK_DEASSERT_ANALYSE, LINK_ANALYSE(minor));
	link_delay();

	DEB_MORE(printk("LINK(%d) analyse() success!\n\n", minor);)
} /* link_analyse() */


/****************************************************************************
 *
 * static int link_read() - read bytes from the link interface.
 *
 * All reads from the Transputer link adapter are handled here. Simply it is
 * is a major loop that polls a number of exit conditions then reacts when one
 * becomes true.
 *
 * The loop will react to "count" bytes being read from the Link adapter
 * Rx side of the UART. Or it will react to a timer timeout, or a break
 * condition instructed by the system.
 *
 * Timing is derived from the jiffie counter which removes any dependancy
 * on CPU execution speed for timing.
 *
 * By default, LINK_READ_ABORT is not set.
 *
 * On exit: -EINTR		- Break due to interrupt.
 *          count		- Count of character actually read from Rx.
 *****************************************************************************/

static ssize_t link_read( struct file * file,
                          char * buf,
                          size_t count,
                          loff_t *ppos )
{
	const unsigned int	minor = MINOR(file->f_dentry->d_inode->i_rdev);
	      unsigned int	timer;
	      unsigned int	sleep_timer;
	               int	l_count;
	               int	max_sleep;
	               int	end;
	               int	copy_result;
	              char 	*temp = buf;
	      unsigned char	c;
	              char	buffer[LINK_MAX_BYTES];
	DEB(  unsigned int	link_total_bytes_read = 0; )

	PRINTK("LINK(%d) read() set to %d bytes\n", minor, count);

	if ( count < 0)
	{
		PRINTK("LINK(%d) read() EINVAL! count = %d\n\n", minor, count);
		return( -EINVAL );
	}

	if ( (LINK_F(minor) &= ~LINK_BUSY) == 0)
	{
		DEB_MORE(printk("LINK(%d) read() EINVAL!\n\n", minor);)
		return( -EINVAL );
	}

	max_sleep = 0;
	while ( count )
	{
		l_count = 0;

		end = count;

		if (end > LINK_MAX_BYTES) end = LINK_MAX_BYTES;

		/**** Main character read from UART ****/

		timer = jiffies + (100 / (1000 / HZ));
		sleep_timer = jiffies + (20 / (1000 / HZ));
		
		while (end)
		{
			if (timer < jiffies) break;
			
			if ( in(LINK_ISR(minor)) & LINK_READBYTE )
			{
				c = (char)in(LINK_IDR(minor));
				DEB_MORE(PRINTK("LINK(%d) read() data [0x%02x]\n", minor, c);)
				buffer[l_count] = c;

				DEB(link_total_bytes_read++;)
				end--;
				l_count++;
				
				/* Reset sleep count after a successful read */
				max_sleep = 0;
			}

			if (sleep_timer < jiffies)
			{				
				/* 
				 * return if rescheduled more than 100 times - a single byte shouldn't
				 * take more than a few timer reschedules to read! Stops user space 
				 * code from hanging indefinitely.
				 */
				if (max_sleep > 50)
				{
					PRINTK("LINK(%d) read() EINTR! reschedule sleep timer exceeded!\n\n", minor);
					return( -EINTR );
				}
				
				sleep_timer = jiffies + (20 / (1000 / HZ));
				
				if (signal_pending(current))
				{
					DEB_MORE(printk("LINK(%d) read() EINTR! interrupted!\n\n", minor);)
					return( -EINTR );
				}

				if (need_resched())
				{
					max_sleep++;
					DEB_MORE(PRINTK("LINK(%d) read() sleep for IO [%d remaining]\n", minor, (100 - max_sleep));)
					schedule();
				}
				
			}
		}

		/**** Move received characters to user space ****/

		if (l_count)
		{
			copy_result = copy_to_user( temp, buffer, l_count );
			count -= l_count;
			temp += l_count;
		}
	}

	DEB_MORE(printk("LINK(%d) read() success!\n", minor);)
	PRINTK("LINK(%d) [bytes read: %d]\n\n", minor, l_count);

	return( temp - buf );

} /* LINK_read() */




/****************************************************************************
 *
 * static int link_write() - write to the link interface.
 *
 * This function works in a similar way to link_read() accept that it writes
 * characters out to the Transputers os-link UART (Tx). It also uses
 * jiffies to measure all timing intervals to remove reliance on CPU speeds
 * in tight loops as timing mechanisms.
 *
 * The function sets up a major running loop where it polls a number of events
 * to become true. Should any of these do so then the loop is exited with an
 * appropriate exit code or byte count.
 *
 * The main exit point will be all characters sent, however, UART lockups will
 * trigger a timeout event or a system signal will cause a premature exit.
 *
 * There is no limit to the number of bytes this function can send in a 
 * single call beyond memory limits. The count of bytes actually sent will be
 * returned to the caller on exit.
 *
 * On exit:  -EINT    - System requested exit.
 *           -EINVAL  - Timeout due to locked UART.
 *           count    - Number or bytes written to Transputer.
 *****************************************************************************/

static ssize_t link_write( struct file * file,
                           const char * buf,
                           size_t count,
                           loff_t *ppos )
{
	const unsigned int	minor = MINOR(file->f_dentry->d_inode->i_rdev);
	      unsigned int	timer;
	      unsigned int	sleep_timer;
	               int	l_count = 0;
	               int	size = count;
	               int	copy_result;
	               int	max_sleep;
	               int	link_notready_times = 0;
	               int	end;
	        const char	*cptr = buf;
	              char	buffer[LINK_MAX_BYTES];
	DEB(  unsigned int link_total_bytes_written = 0; )

	PRINTK("LINK(%d) write() %d bytes\n", minor, count);

	if ( (LINK_F(minor) &= ~LINK_BUSY) == 0)
	{
		return( -EINVAL );
	}

	if ( count < 0)
	{
		PRINTK("LINK(%d) write() invalid argument: count = %d.\n", minor, count);
		return( -EINVAL );
	}

	while ( count )
	{
		l_count = 0;
		end = count;

		if (end > LINK_MAX_BYTES) end = LINK_MAX_BYTES;

		copy_result = copy_from_user( buffer, cptr, end );
		cptr += end;

		/**** Setup timers and begin to send data out the UART ****/

		timer = jiffies + (100 / (1000 / HZ));
		sleep_timer = jiffies + (20 / (1000 / HZ));

		max_sleep = 0;
		while (end)
		{
			if (in(LINK_OSR(minor)) & LINK_WRITEBYTE)
			{
				DEB_MORE(PRINTK("LINK(%d) write() data [0x%02x]\n", minor, buffer[l_count]);)
				out( buffer[l_count], LINK_ODR(minor) );

				end--;
				l_count++;

				/* Reset sleep count after a successful write */
				max_sleep = 0;
				DEB(link_total_bytes_written++;)
			} else {
				DEB_MORE(PRINTK("LINK(%d) write() output register not ready!\n", minor);)
				if (link_notready_times > LINK_MAXTRY)
				{
					PRINTK("LINK(%d) write() EINVAL! Link not ready!\n", minor );
					PRINTK("LINK(%d) [bytes remaining: %d | notready wait: %d]\n\n", minor, count, link_notready_times);
					return( -EINVAL );
				}
				link_notready_times++;

				if (timer < jiffies)
				{
					PRINTK("LINK(%d) write() EINVAL! Timed out waiting for Tx register!\n", minor );
					PRINTK("LINK(%d) [bytes remaining: %d | notready wait: %d]\n\n", minor, count, link_notready_times);
					return( -EINVAL );
				}
	
				if (sleep_timer < jiffies)
				{
					
					/* 
					 * return if rescheduled more than XX times - a single byte shouldn't
					 * take more than a few timer reschedules to write! Stops user space 
					 * code from hanging indefinitely.
					 */
					if (max_sleep > 50)
					{
						PRINTK("LINK(%d) write() EINTR! reschedule sleep timer exceeded!\n", minor);
						PRINTK("LINK(%d) [bytes remaining: %d | notready wait: %d]\n\n", minor, count, link_notready_times);
						return( -EINTR );
					}
					
					sleep_timer = jiffies + (20 / (1000 / HZ));
	
					if (signal_pending( current ))
					{
						return( -EINTR );
					}
	
					if (need_resched()){
						max_sleep++;
						DEB_MORE(PRINTK("LINK(%d) write() sleep for IO [%d remaining]\n", minor, (100 - max_sleep));)
						schedule();
					}
				}
			}
		}

		count -= l_count;
	}

	DEB_MORE(printk("LINK(%d) write() success!\n", minor);)
	PRINTK("LINK(%d) [bytes remaining: %d | notready wait: %d]\n\n", minor, count, link_notready_times);
	return( size - count );
} /* link_write() */

/****************************************************************************
 *
 * static int link_lseek()
 *
 ***************************************************************************/

static loff_t link_lseek(struct file * file, long long offset, int origin)
{
	return -ESPIPE;
}

/****************************************************************************
 *
 * static int link_open()
 *
 * open the link-device.
 *
 ***************************************************************************/

static int link_open(struct inode * inode, struct file * file)
{
	const unsigned int minor = MINOR(inode->i_rdev);

	if (minor >= link_devices)
	{
		PRINTK("LINK not opened, minor device number >= %d.\n", link_devices);
		return -ENODEV;
	}

	if (LINK_F(minor) & LINK_BUSY)
	{
		PRINTK("LINK not opened, LINK-board busy (minor = %d).\n", minor);
		return -EBUSY;
	}

	LINK_F(minor) |= LINK_BUSY;

	PRINTK( "LINK(%d) opened.\n\n", minor);
	return 0;

} /* link_open() */


/****************************************************************************
 *
 * static int link_release()
 *
 * close the link device.
 *
 ****************************************************************************/

static int link_release(struct inode * inode, struct file * file)
{
	const unsigned int minor = MINOR(inode->i_rdev);

	if (minor >= link_devices)
	{
		PRINTK("LINK not released, minor device number >= %d.\n", link_devices);
		return 0;
	}

	LINK_F(minor) &= ~LINK_BUSY;

	PRINTK("LINK(%d) released.\n\n", minor);

	return 0;

} /* link_release() */


/****************************************************************************
 *
 * static int link_ioctl()
 *
 * This function performs the various ioctl() functions: resetting the
 * transputer, switching to analyse-mode, testing the status, changing
 * timeouts etc.
 *
 *****************************************************************************/

static int link_ioctl( struct file *file,
		               unsigned int cmd,
		               unsigned long arg )
{
	const unsigned int	minor = MINOR(file->f_dentry->d_inode->i_rdev);
	               int	result = arg;

	PRINTK("LINK(%d) ioctl, cmd: 0x%x, arg: 0x%x.\n", minor, cmd, (int) arg);

	if (minor >= link_devices || !(LINK_F(minor) & LINK_BUSY) )
	{
		DEB(
			if (minor >= link_devices)
				printk("LINK ioctl exit, minor >= %d.\n", link_devices );
			else
				printk("LINK ioctl exit, device not opened.\n" );
		)
		return -ENODEV;
	}

	switch (cmd)
	{
		case LINKRESET:		/* reset transputer */
			link_reset(minor);
			break;
		case LINKWRITEABLE:	/* can we write a byte to the C012 ? */
	 		return ( ( in(LINK_OSR(minor)) & LINK_WRITEBYTE) != 0 ); 
		case LINKREADABLE:	/* can we read a byte from C012 ? */
	 		return ( ( in(LINK_ISR(minor)) & LINK_READBYTE) != 0 ); 
		case LINKANALYSE:	/* switch transputer to analyse mode */
			link_analyse(minor);
			break;
		case LINKERROR:		/* test error-flag */
			return ( in(LINK_BASE(minor) + LINK_ERROR_OFFSET) & LINK_TEST_ERROR) ? 0 : 1;
		case LINKREADTIMEOUT:	/* set timeout for reading */
			result = LINK_READ_TIMEOUT(minor);
			LINK_READ_TIMEOUT(minor) = arg;
			break;
		case LINKWRITETIMEOUT:	/* set timeout for writing */
			result = LINK_WRITE_TIMEOUT(minor);
			LINK_WRITE_TIMEOUT(minor) = arg;
			break;
		case LINKREADABORT:	/* abort after a timeout ? */
			if ( arg )
				LINK_F(minor) |= LINK_READ_ABORT;
			else
				LINK_F(minor) &= ~LINK_READ_ABORT;
			break;
		case LINKWRITEABORT:	/* abort after a timeout ? */
			if ( arg )
				LINK_F(minor) |= LINK_WRITE_ABORT;
			else
				LINK_F(minor) &= ~LINK_WRITE_ABORT;
			break;
		default: result = -EINVAL;
	}

	PRINTK("LINK(%d) ioctl done.\n\n", minor);

	return result;

} 

static struct file_operations link_fops = {
	.owner 		= THIS_MODULE,	
	.llseek 	= link_lseek,
	.read 		= link_read,
	.write 		= link_write,
	.unlocked_ioctl	= link_ioctl,
	.open 		= link_open,
	.release 	= link_release
};

/****************************************************************************
 *
 * long link_init()
 *
 * This function initializes the driver. It tries to detect the hardware
 * and sets up all relevant data structures.
 *
 ****************************************************************************/
long link_init(long kmem_start)
{
	unsigned int test, i;

	if ( register_chrdev( LINK_MAJOR, LINK_NAME, &link_fops ) )
	{
		printk("link_init: unable to get major %d for link interface.\n", LINK_MAJOR );
		return kmem_start;
	}

	/*
	   After a reset it should be possible to write a byte to
	   the LINK. So let's do a reset and then test the output status
	   register
	*/
	for (test = 0; link_base_addresses[test] && link_devices < LINK_NO; test++)
	{
		link_delay();
		LINK_BASE((int) link_devices) = link_base_addresses[test];
		LINK_ODR((int) link_devices) = LINK_BASE((int) link_devices) + LINK_ODR_OFFSET;
		LINK_ISR((int) link_devices) = LINK_BASE((int) link_devices) + LINK_ISR_OFFSET;
		LINK_OSR((int) link_devices) = LINK_BASE((int) link_devices) + LINK_OSR_OFFSET;
		link_reset(link_devices);
		link_delay();

		for (i = 0; i < LINK_MAXTRY; i++)
		{
			if ( in(LINK_OSR((int) link_devices)) == LINK_WRITEBYTE)
			{
				out(LINK_BASE((int) link_devices) + B008_INT_OFFSET, 0);
				link_delay();
				if ((in(LINK_BASE((int) link_devices) + B008_INT_OFFSET) & 0x0f) == 0)
					LINK_BOARDTYPE((int) link_devices) = LINK_B008;
				else
					LINK_BOARDTYPE((int) link_devices) = LINK_B004;
				printk("link%d at 0x0%x (polling) is a B00%s\n", link_devices,LINK_IDR((int) link_devices), LINK_BOARDTYPE((int) link_devices) == LINK_B004 ? "4" : "8");
				request_region(LINK_IDR((int) link_devices), B008_IO_SIZE, LINK_NAME);
				link_devices++;
				break;
			}
		}
	}
	if (link_devices == 0) printk("link: no interfaces found.\n");
	return( kmem_start );
}

/* Load module */
int init_module(void)
{
	long dummy = 0;
	dummy = link_init(dummy);
	return 0;
}

/* Unload module */
void cleanup_module(void)
{
	int i;
	unregister_chrdev( LINK_MAJOR, LINK_NAME );
	for (i = 0; i < link_devices; i++)
		release_region(LINK_IDR(i), B008_IO_SIZE);
}
