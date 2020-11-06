#!/bin/bash

function print_help()
{
  echo "#######################################################################";
  echo "Usage:";
  echo "./run_particleSim.sh --mode <mode> --Nevts <Nevts> --targ <targ> --pid <pid> --bkg <bkg> --run1 <run1> --run2 <run2>";
  echo "where:";
  echo "  <mode>  = 0 (interactive), 1 (farm)";
  echo "  <Nevts> = number of events to generate";
  echo "  <targ>  = selects target: (0,1,2,3) <==> (\"D\", \"C\", \"Fe\", \"Pb\")";
  echo "  <pid>   = pid of detected particle. eg. (223,221,2212) for (omega,eta,proton)";
  echo "  <bkg>   = 0 (generate events with at least one of selected particle in the final state), 1 (generate all particles but selected one)";
  echo "  <run1,run2> = integers >=0 to loop over";
  echo "eg: ./run_particleSim.sh --mode 0 --Nevts 100 --targ 0 --pid 223 --bkg 0 --run1 0 --run2 0";
  echo "#######################################################################";

  exit 1;
}

#######################################################################

function process_args()
{
  arr=("$@")
  ic=0
  while [ $ic -le $((${#arr[@]}-1)) ]; do
    if [ "${arr[$ic]}" == "--mode" ]; then
      mode=${arr[$((ic+1))]}
    elif [ "${arr[$ic]}" == "--Nevts" ]; then
      Nevts=${arr[$((ic+1))]}
    elif [ "${arr[$ic]}" == "--targ" ]; then
      targ=${arr[$((ic+1))]}
    elif [ "${arr[$ic]}" == "--pid" ]; then
      pid=${arr[$((ic+1))]}
    elif [ "${arr[$ic]}" == "--bkg" ]; then
      bkg=${arr[$((ic+1))]}
    elif [ "${arr[$ic]}" == "--run1" ]; then
      run1=${arr[$((ic+1))]}
    elif [ "${arr[$ic]}" == "--run2" ]; then
      run2=${arr[$((ic+1))]}
    else
      echo "*** Aborting: Unrecognized argument: ${arr[$((ic))]} ***";
      print_help;
    fi
    ((ic+=2))
  done
}

#######################################################################

function check_args()
{
  if [ $mode -ne 0 -a $mode -ne 1 ]; then
    echo "*** Aborting: wrong mode value. Possible values are 0 or 1 ***";
    print_help;
  fi

  if [ $Nevts -le 0 ]; then
    echo "*** Aborting: Number of events should be positive ***";
    print_help;
  fi

  if [ $targ -lt 0 -o $targ -gt 3 ]; then
    echo "*** Aborting: unrecognized target. Possible values are 0, 1, 2, 3 ***";
    print_help;
  fi

#  if [ $pid -ne 223 -a $pid -ne 221 ]; then
#    echo "*** Aborting: unrecognized pid. Possible values are 223 ***";
#    print_help;
#  fi

  if [ $run1 -lt 0 -o $run2 -lt 0 ]; then
    echo "*** Aborting: run number should be >= 0 ***";
    print_help;
  fi

  if [ $bkg -lt 0 -o $bkg -gt 1 ]; then
    echo "*** Aborting: bkg option should be 0 or 1 ***";
    print_help;
  fi
}

#######################################################################

function print_args()
{
  echo "mode:    $mode"
  echo "Nevts:   $Nevts"
  echo "targ:    $targ"
  echo "pid:     $pid"
  echo "bkg:     $bkg"
  echo "run1:    $run1"
  echo "run2:    $run2"
}

#######################################################################

function get_run()
{
  sr=$1
  srn=""
  if [ $sr -lt 10 ]; then
    srn="000$sr"
  elif [ $sr -lt 100 ]; then
    srn="00$sr"
  elif [ $sr -lt 1000 ]; then
    srn="0$sr"
  else
    srn="$sr"
  fi
  echo $srn
}

#######################################################################
###############################          ##############################
###############################   Main   ##############################
###############################          ##############################
#######################################################################

NARGS=14
if [ $# -ne $NARGS ]; then
  echo "Missing arguments. You provided $# args. It should be $NARGS."
  print_help;
fi

argArray=("$@")
process_args "${argArray[@]}"
check_args
print_args

if [[ "$bkg" == "0" ]]; then
    TOPOUDIR="/eos/user/${USER:0:1}/${USER}/particleSim"
elif [[ "$bkg" == "1" ]]; then
    TOPOUDIR="/eos/user/${USER:0:1}/${USER}/bkgSim"
fi
SIMINDIR="${HOME}/simulations"

if [ ! -d $SIMINDIR ]; then
  echo "Directory SIMINDIR:$SIMINDIR does not exist"
  exit 1
fi

targName=("D"   "C"   "Fe"   "Pb")
targType=("lt"  "st"  "st"  "st")
targA=(    2     12    56    208)
targZ=(    1     6     26    82)
targZpos=( -30   -25   -25   -25)
targVG2=(  1     2     2       2)

tarA="${targA[$targ]}"
tarZ="${targZ[$targ]}"
tarZpos="${targZpos[$targ]}"
tarVG2="${targVG2[$targ]}"
tarName="${targName[$targ]}"
tarType="${targType[$targ]}"

simfile="particleSim${tarName}.sh"

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

#Check if input files exist
if [ ! -f ${SIMINDIR}/particleSim.sh ]; then
  echo "${SIMINDIR}/particleSim.sh does not exist"
  exit 1
fi

if [ ! -f ${SIMINDIR}/ffread_eg2.gsim ]; then
  echo "${SIMINDIR}/ffread_eg2.gsim does not exist"
  exit 1
fi

if [ ! -f ${SIMINDIR}/leptotxt.pl ]; then
  echo "${SIMINDIR}/leptotxt.pl does not exist"
  exit 1
fi

if [ ! -f ${SIMINDIR}/recsis_eg2.tcl ]; then
  echo "${SIMINDIR}/recsis_eg2.tcl does not exist"
  exit 1
fi

for (( ir=$run1; ir<=$run2; ir++ )); do

  echo ""
  srun=$(get_run "$ir")
  run="run${srun}"

  if [ "${mode}" == "0" ]; then

    IFARMDIR="${TOPOUDIR}/ifarm/${tarName}/${run}"
    mkdir -p ${IFARMDIR}

    cd ${IFARMDIR}
    cp ${SIMINDIR}/particleSim.sh  ${IFARMDIR}/${simfile}
    cp ${SIMINDIR}/ffread_eg2.gsim ${IFARMDIR}/${ffreadfile}
    cp ${SIMINDIR}/recsis_eg2.tcl  ${IFARMDIR}/${tclfile}

    sed -i "s|^Nevts=|Nevts=${Nevts}|g"           ${simfile}
    sed -i "s|^pid=|pid=${pid}|g"                 ${simfile}
    sed -i "s|^bkg=|bkg=${bkg}|g"                 ${simfile}
    sed -i "s|^targ=|targ=${targ}|g"              ${simfile}
    sed -i "s|^SIMINDIR=|SIMINDIR=${SIMINDIR}|g"  ${simfile}

    chmod 755 ./${simfile}
    ./${simfile}

  elif [ "${mode}" == "1" ]; then

    FARMDIR="${TOPOUDIR}/farm/${tarName}/${run}"
    mkdir -p ${FARMDIR}

    cd ${FARMDIR}
    cp ${SIMINDIR}/particleSim.sh  ${FARMDIR}/${simfile}
    cp ${SIMINDIR}/leptotxt.pl     ${FARMDIR}/leptotxt.pl
    cp ${SIMINDIR}/ffread_eg2.gsim ${FARMDIR}/${ffreadfile}
    cp ${SIMINDIR}/recsis_eg2.tcl  ${FARMDIR}/${tclfile}

    jobfile="${FARMDIR}/job_particle.sh"
    jobname=particleSim${tarName}_${srun}
    memusage="512"

    echo "#!/bin/bash"                                                 > $jobfile
    echo "#SBATCH -J ${jobname}"                                      >> $jobfile
    echo "#SBATCH -o ${FARMDIR}/job${tarName}.out"                    >> $jobfile
    echo "#SBATCH -e ${FARMDIR}/job${tarName}.err"                    >> $jobfile
    echo "#SBATCH --time=3:00:00"                                     >> $jobfile
    echo "#SBATCH --mem=${memusage}MB"                                >> $jobfile
    echo ""                                                           >> $jobfile
    echo "sed -i \"s|^Nevts=|Nevts=${Nevts}|g\"           ${simfile}" >> $jobfile
    echo "sed -i \"s|^pid=|pid=${pid}|g\"                 ${simfile}" >> $jobfile
    echo "sed -i \"s|^bkg=|bkg=${bkg}|g\"                 ${simfile}" >> $jobfile
    echo "sed -i \"s|^targ=|targ=${targ}|g\"              ${simfile}" >> $jobfile
    echo "sed -i \"s|^SIMINDIR=|SIMINDIR=${SIMINDIR}|g\"  ${simfile}" >> $jobfile
    echo "chmod 755 ./${simfile}"                                     >> $jobfile
    echo "sh ${simfile}"                                              >> $jobfile

    echo "Running job $jobfile..."
    sbatch $jobfile
  fi
done
