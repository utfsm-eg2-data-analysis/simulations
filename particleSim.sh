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
targ=
SOFTDIR=
OUTDIR=
THIS_HOST=

tarA="${targA[$targ]}"
tarZ="${targZ[$targ]}"
tarZpos="${targZpos[$targ]}"
tarVG2="${targVG2[$targ]}"
tarName="${targName[$targ]}"
tarType="${targType[$targ]}"

leptoinfile=lepto.txt
leptotxtfile=lepto${tarName}.txt
leptocvsfile=lepto${tarName}.cvs
leptobosfile=lepto${tarName}.A00
leptologfile=lepto${tarName}.log

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
source ${SOFTDIR}/env_scripts/set_all.sh
echo "%%% Environment variables ready. %%%"
echo

echo
echo "%%% Creating dirs and copying necessary files... %%%"
mkdir -vp ${OUTDIR}/lepto
mkdir -vp ${OUTDIR}/gsim
cp -v ${SOFTDIR}/simulations/ffread_eg2.gsim ${OUTDIR}/gsim/${ffreadfile}
mkdir -vp ${OUTDIR}/gpp
mkdir -vp ${OUTDIR}/user_ana
cp -v ${SOFTDIR}/simulations/recsis_eg2.tcl ${OUTDIR}/user_ana/${tclfile}
mkdir -vp ${OUTDIR}/clastool
echo "%%% Dirs and files ready.%%%"
echo

#Part 2: start lepto process
echo
echo "%%% Running LEPTO... %%%"
cd ${OUTDIR}/lepto # enter dir
echo "${Nevts} ${tarA} ${tarZ} ${pid}" > ${leptoinfile}
${SOFT_DIR}/Lepto64Sim/bin/lepto.exe > ${leptotxtfile}
perl ${SOFT_DIR}/Lepto64Sim/Utilities/leptotxt.pl < ${leptotxtfile} > ${leptocvsfile}
${SOFT_DIR}/Lepto64Sim/Utilities/txt2part/bin/txt2part.exe -o${leptobosfile} < ${leptocvsfile} 2>&1 | tee ${leptologfile}
cp -v ${leptobosfile} ${OUTDIR}/gsim/ # copy to next step
echo "%%% LEPTO ended. %%%"
echo

#Part 3: start gsim process
echo
echo "%%% Running GSIM... %%%"
cd ${OUTDIR}/gsim # enter dir
sed -i "s/TGTP/TGTP ${tarA}/g"    ${ffreadfile}
sed -i "s/VEG2/VEG2 ${tarVG2}/g"  ${ffreadfile}
sed -i "s/TRIG/TRIG ${Nevts}/g"   ${ffreadfile}
export CLAS_CALDB_RUNINDEX="clas_calib.RunIndex"
${CLAS_BIN}/gsim_bat -ffread ${ffreadfile} -mcin ${leptobosfile} -bosout ${gsimbosfile} 2>&1 | tee ${gsimlogfile}
if [[ -f "${gsimbosfile}.A00" ]]; then
    mv "${gsimbosfile}.A00" ${gsimbosfile}
fi
cp -v ${gsimbosfile} ${OUTDIR}/gpp/ # copy to next step
rm -v ${leptobosfile} # remove copied file from prev step
echo "%%% GSIM ready. %%%"
echo

##Part 4: start gpp process
echo
echo "%%% Running GPP... %%%"
cd ${OUTDIR}/gpp # enter dir
export CLAS_CALDB_RUNINDEX="clas_user_calib.RunindexLorenzo"
${CLAS_BIN}/gpp -P0x1f -Y -o${gppbosfile} -a1.2 -b0.86 -c0.87 -f1. -R41147 ${gsimbosfile} 2>&1 | tee ${gpplogfile}
if [[ -f gpp.hbook ]]; then
    mv gpp.hbook ${gppntpfile}
fi
cp -v ${gppbosfile} ${OUTDIR}/user_ana/ # copy to next step
rm -v ${gsimbosfile} # remove copied file from prev step
echo "%%% GPP ended. %%%"
echo

#Part 5: recsis process
echo
echo "%%% Running USER_ANA %%%"
cd ${OUTDIR}/user_ana # enter dir
export CLAS_CALDB_RUNINDEX="clas_calib.RunIndex"
sed -i "s|inputfile|inputfile ${gppbosfile};|g"                     ${tclfile}
sed -i "s|setc chist_filename|setc chist_filename ${recntpfile};|g" ${tclfile}
sed -i "s|setc log_file_name|setc log_file_name ${reclogfile};|g"   ${tclfile}
sed -i "s|outputfile|outputfile ${recbosfile} PROC1 2047;|g"        ${tclfile}
sed -i "s|set TargetPos(3)|set TargetPos(3) ${tarZpos};|g"          ${tclfile}
sed -i "s|go|go ${Nevts};|g"                                        ${tclfile}
${CLAS_BIN}/user_ana -t ${tclfile} 2>&1 | tee ${recsislogfile}
echo "%%% USER_ANA ended. %%%"
cp -v ${recbosfile} ${OUTDIR}/clastool/ # copy to next step
rm -v ${gppbosfile} # remove copied file from prev step
echo

#Part 6: convert recsis ntuple name to upper case
echo
echo "%%% Check USER_ANA output %%%"
if [[ ! -f ${recntpfile} ]]; then
    new_recntpfile=$(rep_str 6 "${recntpfile}")
    if [[ -f ${new_recntpfile} ]]; then
	mv ${new_recntpfile} ${recntpfile}
    fi
fi
if [[ -f histo.hbook ]]; then
    mv histo.hbook ${rechisfile}
fi
echo "%%% Check USER_ANA output ended %%%"
echo

#Part 7: start ClasTool process
echo
echo "%%% Running WRITE_ROOT_DST... %%%"
cd ${OUTDIR}/clastool # enter dir
${CLAS_BIN}/WriteRootDst_b2r ${recbosfile} -GSIM -o ${recrootfile} 2>&1 | tee ${wrdstlogfile}
echo "%%% WRITE_ROOT_DST ended %%%"
rm -v ${recbosfile} # remove copied file from prev step
echo
