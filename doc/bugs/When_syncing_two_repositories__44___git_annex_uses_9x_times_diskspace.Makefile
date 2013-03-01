What steps will reproduce the problem?
Created a repository (working directory), then an another one on the same computer, 
in a different folder for backup repository.

The initial folder, what I imported, contained:
741MB, 23381 files, most of the files are 30-150kB in sizes.

After 18 hours of continous work 
(I started 28 hours ago, but the laptop died one time, 
and I needed to restart git-annex 4 times in total, and the laptop overheated once):
The initial directory (/home/user/down) contains at this point (still not finished):
8.1GB

The target directory(/mnt/dat/annex2) contains:
975MB


What is the expected output? What do you see instead?
I expect maximum three times the usefull data size. So I can calculate with (rule of thumb). 
(Ie. if I want to put 1GB data into annex, it would need maximum (at any point of time 3GB of data)

Currently it uses 11x(!) times the original size, and still growing.

I also expect to stop syncing, when it used up all the available disk space 
(ie. it should leave at least 200MB on the original partition, 
none of the other programs like to have 0 bytes left).
The machine just freeze, ie. takes almost 10 minutes to kill process, 
and delete something to get back to life again.

Also some kind of feedback how many files has been synchronized, 
because currently the dashboard does not indicate any useful info. 
Also the log page seems only growing, making firefox crashes.

Self-controlled resource hogging. Ie. dont use more then 50% processor, 
or dont make heavy disk usage for 3-4 hours, because the laptop can overheat.

What version of git-annex are you using? On what operating system?
http://downloads.kitenet.net/git-annex/linux/current/git-annex-standalone-i386.tar.gz
Version: 4.20130227
Ubuntu 10.04, laptop Toshiba L300, Intel Core2Duo T5800@2.0Ghz, 3GB ram

Please provide any additional information below.
The syncing is still not finished. 
I hope it will finish within additional 24hours.

Best,
 Laszlo
