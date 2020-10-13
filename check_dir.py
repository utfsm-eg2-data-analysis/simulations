#!/usr/bin/env python
from sys import argv, exit
from os import path, makedirs

def check_dir(dir):
    if not path.exists(dir):
        print "making dir : "+ dir 
        makedirs(dir)

if len(argv)!=2:
    print "nothing to do, exit"
    print "use just one argument:"
    print "%s <dir>"%argv[0]
    exit(1)

base_dir=argv[1]

#base_dir="/data/tsunami/user/b/borquez/Simulation-C_C/" + dir
check_dir(base_dir)

save_dir_ct=base_dir + "/ClasTool"
check_dir(save_dir_ct)

save_dir_nt=base_dir + "/NT10"
check_dir(save_dir_nt) 

save_dir_rec=base_dir + "/REC"
check_dir(save_dir_rec) 

tcl_dir=base_dir + "/tcl_scripts"
check_dir(tcl_dir) 

mcin_dir=base_dir + "/MCIN"
check_dir(mcin_dir) 

gsim_dir=base_dir + "/GSIM"
check_dir(gsim_dir) 

gpp_dir=base_dir + "/GPP"
check_dir(gpp_dir) 

stdout_dir=base_dir + "/STDOUT"
check_dir(stdout_dir) 

stderr_dir=base_dir + "/STDERR"
check_dir(stderr_dir) 
