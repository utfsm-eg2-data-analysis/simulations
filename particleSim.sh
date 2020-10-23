#!/bin/bash

function rep_str()
{
  pos=$1
  ostr=$2
  ostrlen=${#ostr}
  vu=${ostr:pos:1}
  vl=`echo "${vu}" | tr '[:upper:]' '[:lower:]'`
  v1=${ostr:0:pos}
  v3=${ostr:pos+1:ostrlen}
  nstr=${v1}${vl}${v3}
  echo ${nstr}
}

targName=("D"   "C"   "Fe"  "Pb")
targType=("lt"  "st"  "st"  "st")
targA=(    2     12    56    207)
targZ=(    1     6     26    82)
targZpos=( -30   -25   -25   -25)
targVG2=(  1     2     2     2)

Nevts=
pid=
bkg=
targ=
SIMINDIR=

tarA="${targA[$targ]}"
tarZ="${targZ[$targ]}"
tarZpos="${targZpos[$targ]}"
tarVG2="${targVG2[$targ]}"
tarName="${targName[$targ]}"
tarType="${targType[$targ]}"

leptobosfile=lepto${tarName}.A00
leptologfile=lepto${tarName}.log
leptoinfile=lepto${tarName}.txt

ffreadfile="ffread_eg2${tarName}_${tarType}.gsim"
gsimbosfile=gsim${tarName}.A00
gsimlogfile=gsim${tarName}.log

gppbosfile=gpp${tarName}.A00
gppntpfile=gpp${tarName}.hbook
gpplogfile=gpp${tarName}.log

tclfile=recsis${tarName}.tcl
rechisfile=recsis${tarName}_histos.hbook
recntpfile=recsis${tarName}.hbook
recbosfile=recsis${tarName}.A00
reclogfile=recsis${tarName}.log
recsislogfile=recsis${tarName}_log.log

recrootfile=recsis${tarName}.root
wrdstlogfile=WriteRootDst${tarName}.log

#Part 1: Setting environment variables
echo
echo "%%% Setting environment variables... %%%"
export SIMINDIR=${HOME}/simulations
source ${SIMINDIR}/set_env.sh
echo "%%% Environment variables ready. %%%"
echo

#Part 2: start lepto process
echo
echo "%%% Running LEPTO... %%%"
echo "${Nevts} ${tarA} ${tarZ} ${pid}" > ./lepto.txt
if [[ "$bkg" == "0" ]]; then
    ${SIMINDIR}/Lepto64Sim/bin/lepto.exe | ${SIMINDIR}/leptotxt.pl | ${SOFT}/txt2part_src/bin/txt2part.exe -o${leptobosfile} 2>&1 | tee ${leptologfile} # only omegas
elif [[ "$bkg" == "1" ]]; then
    ${SIMINDIR}/Lepto64Sim/bin/lepto_bkg.exe | ${SIMINDIR}/leptotxt.pl | ${SOFT}/txt2part_src/bin/txt2part.exe -o${leptobosfile} 2>&1 | tee ${leptologfile} # no omegas
elif [[ "$bkg" == "2" ]]; then
    ${SIMINDIR}/Lepto64Sim/bin/lepto_bkg_v2.exe | ${SIMINDIR}/leptotxt.pl | ${SOFT}/txt2part_src/bin/txt2part.exe -o${leptobosfile} 2>&1 | tee ${leptologfile} # no omegas, no etas
fi
mv lepto.txt ${leptoinfile}
echo "%%% LEPTO ended. %%%"
echo

#Part 3: start gsim process
echo
echo "%%% Running GSIM... %%%"
sed -i "s/TGTP/TGTP ${tarA}/g"    ${ffreadfile}
sed -i "s/VEG2/VEG2 ${tarVG2}/g"  ${ffreadfile}
sed -i "s/TRIG/TRIG ${Nevts}/g"   ${ffreadfile}
${CLAS_BIN}/gsim_bat -ffread ${ffreadfile} -mcin ${leptobosfile} -bosout ${gsimbosfile} 2>&1 | tee ${gsimlogfile}
if [ -f "${gsimbosfile}.A00" ]; then
    mv "${gsimbosfile}.A00" ${gsimbosfile}
fi
echo "%%% GSIM ready. %%%"
echo

##Part 4: start gpp process
echo
echo "%%% Running GPP... %%%"
export CLAS_CALDB_RUNINDEX=calib_user.RunindexLorenzo
${CLAS_BIN}/gpp -P0x1f -Y -o${gppbosfile} -a1.2 -b0.86 -c0.87 -f1. -R41147 ${gsimbosfile} 2>&1 | tee ${gpplogfile}
if [ -f gpp.hbook ]; then
    mv gpp.hbook ${gppntpfile}
fi
echo "%%% GPP ended. %%%"
echo

#Part 5: recsis process
echo
echo "%%% Running USER_ANA %%%"
export CLAS_CALDB_RUNINDEX="calib.RunIndex"
sed -i "s|inputfile|inputfile ${gppbosfile};|g"                     ${tclfile}
sed -i "s|setc chist_filename|setc chist_filename ${recntpfile};|g" ${tclfile}
sed -i "s|setc log_file_name|setc log_file_name ${reclogfile};|g"   ${tclfile}
sed -i "s|outputfile|outputfile ${recbosfile} PROC1 2047;|g"        ${tclfile}
sed -i "s|set TargetPos(3)|set TargetPos(3) ${tarZpos};|g"          ${tclfile}
sed -i "s|go|go ${Nevts};|g"                                        ${tclfile}
${CLAS_BIN}/user_ana -t ${tclfile} 2>&1 | tee ${recsislogfile}
echo "%%% USER_ANA ended. %%%"
echo

#Part 6: convert recsis ntuple name to upper case
echo
echo "%%% Check USER_ANA output %%%"
if [ ! -f ${recntpfile} ]; then
    new_recntpfile=$(rep_str 6 "${recntpfile}")
    if [ -f ${new_recntpfile} ]; then
	mv ${new_recntpfile} ${recntpfile}
    fi
fi
if [ -f histo.hbook ]; then
    mv histo.hbook ${rechisfile}
fi
echo "%%% Check USER_ANA output ended %%%"
echo

#Part 7: start ClasTool process
echo
echo "%%% Running WRITE_ROOT_DST... %%%"
${CLAS_BIN}/WriteRootDst_b2r ${recbosfile} -GSIM -o ${recrootfile} 2>&1 | tee ${wrdstlogfile}
echo "%%% WRITE_ROOT_DST ended %%%"
echo

#Part 8: remove trash files
echo
echo "%%% Removing trash files... %%%"
GLOBIGNORE=${recrootfile}
rm -v *
unset GLOBIGNORE
echo "%%% Everything ready, sir! %%%"
echo
