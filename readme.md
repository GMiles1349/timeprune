# gembackupdate

timeprune is a Linux utility for pruning (deleting) old and excessive timeshift snapshots. The user can use the default values hardcoded into the source (which will then be written to the timeprune.conf file upon first execution), or change the maximum number or age of snapshots to keep through commandline options.

## !IMPORTANT!

You currently can't compile this. It relied on one of my own units that I apparently deleted at some point. It wouldn't take much to fix it, so maybe I do at some point.

## Dependencies

- Binaries
  * bash
  * timeshift

- Source (for compilation)
  * Classes.pp (FPC)
  * SysUtils.pp (FPC)
  * Unix.pp (FPC)
  * StrUtils (FPC)
  * CRT (FPC)
  * gemutil (in Units repo)
  * gemclock (in Units repo)


