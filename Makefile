###################################################################
#
# New style 'kbuild' makefile for the INMOS Transputer link driver
#
# John Snowdon <john.snowdon@newcastle.ac.uk>, November 2014
#
###################################################################

obj-m := link-driver.o

all:
	make -C /lib/modules/$(shell uname -r)/build M=$(PWD) modules

clean:
	make -C /lib/modules/$(shell uname -r)/build M=$(PWD) clean
