/************************************************************************
*									*
*	Device Driver for INMOS-B004/B008 compatible link interfaces	*
*									*
*	Copyright (C) 1993,94,95 by Christoph Niemann			*
*				    niemann@swt.ruhr-uni-bochum.de	*
*	based on							*
*		usr/include/linux/lp.h c.1991-1992 James Wiegand	*
*		many modifications copyright (C) 1992 Michael K. Johnson*
*					                                *
*	Version 0.93, For Linux 3.X.X, November 2014                    *
*		by John Snowdon john.snowdon@newcastle.ac.uk            *
*		Changed ioctl call cmd for LINKRESET			*
*									*
*	The driver may be copied under the same conditions as the	*
*	Linux operating system						*
*									*
*************************************************************************/

#ifndef _LINUX_LINK_H
#define _LINUX_LINK_H


/*
 * Per POSIX guidelines, this module reserves the LINK and link prefixes
 */

#define LINK_MAJOR		24		/* Major device number */
#define LINK_NAME		"link"		/* Name of the device to register */
#define LINK_NO			2		/* Number of supported boards */

/*
 * These are the link_table[].flags flags...
 */
#define LINK_EXIST	 	0x0001		/* Is a B004-Board with at least one 
										   Transputer present ? */
#define LINK_BUSY	  	0x0002		/* Is the B004-board in use ? */
#define LINK_READ_ABORT		0x0004 		/* abort reading after timeout ? */
#define LINK_WRITE_ABORT	0x0008 		/* abort writing after timeout ? */

/*
 * IOCTL numbers
 */
#define LINKTIME		0x0001		/* no longer available */
#define LINKRESET		0x0012		/* reset transputer */
#define LINKWRITEABLE		0x0004		/* check if we can send a byte */
#define LINKREADABLE		0x0008		/* check if we can receive a byte */
#define LINKANALYSE		0x0010		/* go to analyse mode */
#define LINKERROR		0x0020
#define LINKREADTIMEOUT		0x0040		/* set timeout for reading */
#define LINKWRITETIMEOUT	0x0080		/* set timeout for writing */
#define LINKREADABORT		0x0100
#define LINKWRITEABORT		0x0200

/*
 * timeout for printk'ing a timeout, in jiffies (100ths of a second).
 * This is also used for re-checking error conditions if LINK_READ_ABORT or 
 * LINK_WRITE_ABORT is not set. This is the default behavior for reading.
 * Writing has the timeout enabled per default.
 */
#define LINK_INIT_WRITE_TIMEOUT	1000
#define LINK_INIT_READ_TIMEOUT	100

/*
 * If the link interface is not ready for reading or writing the driver starts
 * to poll the interface.
 * At the begining, the driver sleeps for LINK_START_SLEEP jiffies. If the
 * link interface is still unable to send or receive a byte, the driver
 * sleeps again for the same duration.
 * After LINK_INC times sleeping LINK_START_SLEEP jiffies, the driver adds one
 * jiffy to its sleeping time. After LINK_INC times sleeping the new amount
 * of time, it is incremented again and so on.
 * The maximum time to sleep without rechecking the link interface is specified
 * by LINK_MAX_SLEEP.
 */

#define LINK_MAX_SLEEP		20
#define LINK_START_SLEEP 	1
#define LINK_INC		16

/*
 * The addresses of the C012 hardware registers relative to the
 * base address.
 */

#define LINK_IDR_OFFSET		0		/* Input Data Register */
#define LINK_ODR_OFFSET		1		/* Output Data Register */
#define LINK_ISR_OFFSET		2		/* Input Status Register */
#define LINK_OSR_OFFSET		3		/* Output Status Register */
#define LINK_RESET_OFFSET	16		/* Reset/Error Register */
#define LINK_ERROR_OFFSET	16
#define LINK_ANALYSE_OFFSET	17		/* Analyse Register */
#define B008_DMA_OFFSET		18		/* B008: DMA request register */
#define B008_INT_OFFSET		19		/* B008: Interrupt control reg */
#define B004_IO_SIZE		20		/* Some 'non' B008 boards have additional registers, allocate the space anyway! */
#define B008_IO_SIZE		20

struct link_struct {
	int flags;			/* various flags */
	int idr;			/* address of the input data register */
	int odr;			/* address if the output data register */
	int isr;			/* address of the input status register */
	int osr;			/* address of the output status register */
	unsigned int read_timeout;	/* timeout for reading from the link */
	unsigned int write_timeout;	/* timeout for writing to the link */
	int boardtype;			/* what kind of board is installed */
};

/*
 * Id's for the supported boards
 */
#define LINK_B004 1
#define LINK_B008 2

/*
 * Defines for easier access to the link_table.
 */
#define LINK_F(minor)			link_table[minor].flags
#define LINK_READ_TIMEOUT(minor)	link_table[minor].read_timeout
#define LINK_WRITE_TIMEOUT(minor)	link_table[minor].write_timeout
#define LINK_BASE(minor)		LINK_IDR(minor)
#define LINK_IDR(minor)			link_table[minor].idr
#define LINK_ODR(minor)			link_table[minor].odr
#define LINK_ISR(minor)			link_table[minor].isr
#define LINK_OSR(minor)			link_table[minor].osr
#define LINK_WAIT(minor)		link_table[minor].wait
#define LINK_BOARDTYPE(minor)		link_table[minor].boardtype
#define LINK_RESET(minor)		(LINK_BASE(minor) + LINK_RESET_OFFSET)
#define LINK_ANALYSE(minor)		(LINK_BASE(minor) + LINK_ANALYSE_OFFSET)

/*
 * Additonal defines for B008-boards
 */
#define B008_DMA(minor)		link_table[minor].int
#define B008_INT(minor)		link_table[minor].dma

/*
 * Number of tries to access isr or osr before reading or writing fails
 */
#define LINK_MAXTRY 		1000	/* Was 300 */

/*
 * Maximum number of bytes to transfer without calling the scheduler
 */

#define LINK_MAX_BYTES		32

/*
 * bit defines for C012 status ports at base + 2/3
 * accessed with LINK_IS, LINK_OS, which gets the byte...
 */
#define LINK_READBYTE		1
#define LINK_WRITEBYTE		1

/*
 * bit defines for C012 reset/error port at base + 16
 */
#define LINK_ASSERT_RESET	0x01	/* resetting the transputer */
#define LINK_DEASSERT_RESET	0x00
#define LINK_TEST_ERROR		0x01	/* for testing the transputer's error flag */

/*
 * bit defines for C012 analyse port at base + 17
 */
#define LINK_ASSERT_ANALYSE	0x01	/* switch transputer to analyse-mode */
#define LINK_DEASSERT_ANALYSE	0x00

/*
 * function prototypes
 */
extern long link_init(long kmem_start);

#endif

