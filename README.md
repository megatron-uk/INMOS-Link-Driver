# INMOS-Link-Driver

Updated INMOS Transputer link driver model for Linux Kernel 3.x.x

This supports the INMOS B004/B008 series of link adaptors to enable the Linux kernel to communicate with the 1980's/1990's INMOS Transputer hardware. 

## Using the Driver

Should build cleanly with a simple 'make' command, as long as you have Kernel headers and the kbuild package installed.

After install, run a 'mknod /dev/link0 c 24 0' to build the device file. I've provided a simple bash script which creates the required devices, builds the driver and inserts it into the running kernel; if you've ever compiled akernel module before it's the same process.

## Testing

Build and installs cleaning under Debian 7 (32bit) and Linux kernel 3.2.26, as of November 2014.

On insertion of the module, it will scan for the INMOS link adapter at the standard port addresses - you should see something like this in your syslog:

```
Jan 24 09:40:23 transpooty kernel: [ 1734.232455] link_driver: module license 'unspecified' taints kernel.
Jan 24 09:40:23 transpooty kernel: [ 1734.232520] Disabling lock debugging due to kernel taint
Jan 24 09:40:24 transpooty kernel: [ 1734.340833] LINK(0) resetting transputer. (0x160)
Jan 24 09:40:24 transpooty kernel: [ 1734.340879] LINK(0) reset() data [0x00]
Jan 24 09:40:24 transpooty kernel: [ 1734.444810] LINK(0) reset() data [0x00]
Jan 24 09:40:24 transpooty kernel: [ 1734.548817] LINK(0) reset() data [0x01]
Jan 24 09:40:24 transpooty kernel: [ 1734.652803] LINK(0) reset() data [0x00]
Jan 24 09:40:24 transpooty kernel: [ 1734.964805] link0 at 0x0150 (polling) is a B004
```

The above log shows a single interface card detected at 0x150 (the standard B004 compatible base address).

## Compatible Hardware

I only have one Transputer board to test this way (a Transtech TMB04), but the code is based on the older Linux driver which supports any INMOS B004 compatible interface.

See my other [https://github.com/megatron-uk/INMOS-ispy](projects) for updated user-level code (ispy, mtest etc) to utilise the new Link driver module.
