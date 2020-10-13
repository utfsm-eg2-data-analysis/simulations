source $env(RECSIS)/tcl/recsis_proc.tcl;

#define packages
turnoff ALL;
global_section off;
turnon seb trk cc tof egn lac user pid;

inputfile

setc chist_filename
setc log_file_name

setc outbanknames(1) "TRGSHEADPARTTBIDHEVTEVNTDCPBCCPBSCPBECPBCALLTBERTRKSTBTRSCRCCL01LCPBBMPREPICVERTMVRTTGBIHLSMCTKMCVX";

outputfile

setc prlink_file_name "prlink_e1f_tgm25.bos";
setc bfield_file_name "bgrid_T67to33.fpk";

#level of analysis 0: raw  2: hbt 4: tbt 
set trk_level 4;
set trk_maxiter 6;                                                                 

set trigger_particle 11;                                                       

set torus_current       2250;
set mini_torus_current  6000;
set poltarget_current   0;
set TargetPos(3)

#user_evnt.F
set lall_nt_do -1;

set lseb_nt_do  -1;
set lscr_nt_do  -1;
set lseb_hist   -1;
set lseb_h_do   -1;
set lmon_hist   -1;
set ltrk_h_do   -1;                                                          
set legn_h_do   -1;                                                        
set ltof_h_do   -1;                                                        
set lec1_h_do   -1;                                                              
set lfec_hist   -1;      
set lfec_h_do   -1;
set lpart_nt_do -1;
set lmctk_nt_do -1;
set touch_id     0;

#for simulations only
set dc_xvst_choice 0;

fpack "timestop -9999999999"

set lscat $false;
set ldisplay_all $false;

setc rec_prompt "CLAS_recsis> ";

go

exit_pend;
