#!/bin/bash -f

echo ">>> Setting Environment Variables for CLAS6 SIMULATIONS <<<"

export ARCH="64bit"
export ARCHT="64bit"

export SIMINDIR=${HOME}/simulations
export SOFT=/home/ahmede/software # external, ahmed dir

echo ""
echo " > SIMINDIR is set to   $SIMINDIR"
echo " > SOFT is set to   $SOFT"
echo " > ARCH is set to       $ARCH"
echo " > ARCHT is set to      $ARCHT"

##############
# QT settings
##############

echo ""
echo ">> Setting Environment Variables for QT"
echo ""

export QT_VER=5.10.1 # hardcoded
export QT_SYS=gcc_64
export QT_TOP=/site/12gev_phys/2.3/Linux_CentOS7.2.1511-x86_64-gcc4.8.5/qt/${QT_VER}/${QT_VER}/${QT_SYS} # external
export QT_BIN=${QT_TOP}/bin
export QT_LIB=${QT_TOP}/lib

if [ ! -d $QT_TOP ]; then
  echo "QT Error: $QT_TOP Not Found"
  return -1
fi

if [ ! -d $QT_BIN ]; then
  echo "QT Error: $QT_BIN Not Found"
  return -1
fi

if [ ! -d $QT_LIB ]; then
  echo "QT Error: $QT_LIB Not Found"
  return -1
fi

echo "QT_VER               is set to ${QT_VER}"
echo "QT_SYS               is set to ${QT_SYS}"
echo "QT_TOP               is set to ${QT_TOP}"
echo "QT_BIN               is set to ${QT_BIN}"
echo "QT_LIB               is set to ${QT_LIB}"

if [ -z "${PATH}" ]; then
  PATH=${QT_BIN}
else
  PATH=${QT_BIN}:${PATH}
fi

if [ -z "${LIBRARY_PATH}" ]; then
  LIBRARY_PATH=${QT_LIB}
else
  LIBRARY_PATH=${QT_LIB}:${LIBRARY_PATH}
fi

if [ -z "${LD_LIBRARY_PATH}" ]; then
  LD_LIBRARY_PATH=${QT_LIB}
else
  LD_LIBRARY_PATH=${QT_LIB}:${LD_LIBRARY_PATH}
fi

################
# cern settings
################

echo ""
echo ">> Setting Environment Variables for CERN"
echo ""

export CERNVER=2005 # hardcoded
export CERN=/apps/cernlib/x86_64_rhel7 # external
export CERN_LEVEL=$CERNVER
export CERN_ROOT=${CERN}/${CERN_LEVEL}
export CERN_LIB=${CERN_ROOT}/lib
export CERN_BIN=${CERN_ROOT}/bin
export CERN_INC=${CERN_ROOT}/include
export CERNLIBDIR=${CERN_LIB}
export CERN_INCLUDEDIR=${CERN_INC}

if [ ! -d $CERN_LIB ]; then
  echo "CERN Error: $CERN_LIB Not Found"
  return -1
fi

if [ ! -d $CERN_BIN ]; then
  echo "CERN Error: $CERN_BIN Not Found"
  return -1
fi

if [ ! -d $CERN_INC ]; then
  echo "CERN Error: $CERN_INC Not Found"
  return -1
fi

echo "CERN_LEVEL           is set to ${CERN_LEVEL}"
echo "CERN_ROOT            is set to ${CERN_ROOT}"
echo "CERN_LIB             is set to ${CERN_LIB}"
echo "CERN_BIN             is set to ${CERN_BIN}"
echo "CERN_INC             is set to ${CERN_INC}"

if [ -z "${PATH}" ]; then
  PATH=${CERN_BIN}
else
  PATH=${CERN_BIN}:${PATH}
fi

if [ -z "$LIBRARY_PATH" ]; then
  export LIBRARY_PATH="${CERN_LIB}"
else
  export LIBRARY_PATH="${CERN_LIB}:$LIBRARY_PATH"
fi

if [ -z "${LD_LIBRARY_PATH}" ]; then
  LD_LIBRARY_PATH=${CERN_LIB}
else
  LD_LIBRARY_PATH=${CERN_LIB}:${LD_LIBRARY_PATH}
fi

################
# root settings
################

echo ""
echo ">> Setting Environment Variables for ROOT"
echo ""

export ROOTVER=6.14.04 # hardcoded
export ROOTSYS=${SOFT}/root/${ROOTVER} # external, ahmed dir
export ROOTLIB=${ROOTSYS}/lib
export ROOTBIN=${ROOTSYS}/bin
export ROOTINC=${ROOTSYS}/include

if [ ! -d $ROOTLIB ]; then
  echo "ROOT Error: $ROOTLIB Not Found"
  return -1
fi

if [ ! -d $ROOTBIN ]; then
  echo "ROOT Error: $ROOTBIN Not Found"
  return -1
fi

if [ ! -d $ROOTINC ]; then
  echo "ROOT Error: $ROOTINC Not Found"
  return -1
fi

echo "ROOTVER              is set to ${ROOTVER}"
echo "ROOTSYS              is set to ${ROOTSYS}"
echo "ROOTLIB              is set to ${ROOTLIB}"
echo "ROOTBIN              is set to ${ROOTBIN}"
echo "ROOTINC              is set to ${ROOTINC}"

if [ -z "$PATH" ]; then
  export PATH="$ROOTBIN"
else
  export PATH="$ROOTBIN:$PATH"
fi

if [ -z "$LIBRARY_PATH" ]; then
  export LIBRARY_PATH="$ROOTLIB"
else
  export LIBRARY_PATH="$ROOTLIB:$LIBRARY_PATH"
fi

if [ -z "$LD_LIBRARY_PATH" ]; then
  export LD_LIBRARY_PATH="$ROOTLIB"
else
  export LD_LIBRARY_PATH="$ROOTLIB:$LD_LIBRARY_PATH"
fi

#################
# mysql settings
#################

echo ""
echo ">> Setting Environment Variables for MySQL"
echo ""

MYSQL=/usr/bin
MYSQL_INCLUDE_PATH=/usr/include/mysql # external
if [ -f /etc/os-release ]; then
  MYSQL_LIB_PATH=/usr/lib64/mysql
elif [ -f /etc/debian_version ]; then
  MYSQL_LIB_PATH=/usr/lib/x86_64-linux-gnu
elif [ `uname -s` == "Darwin" ]; then
  MYSQL_DIR=/usr/local/Cellar/mysql/5.7.19
  MYSQL_INCLUDE_PATH=$MYSQL_DIR/include
  MYSQL_LIB_PATH=$MYSQL_DIR/lib
  MYSQL=$MYSQL_DIR/bin
fi

if [ ! -d $MYSQL ]; then
  echo "MYSQL Error: $MYSQL Not Found"
  return -1
fi

if [ ! -d $MYSQL_LIB_PATH ]; then
  echo "MYSQL Error: $MYSQL_LIB_PATH Not Found"
  return -1
fi

if [ ! -d $MYSQL_INCLUDE_PATH ]; then
  echo "MYSQL Error: $MYSQL_INCLUDE_PATH Not Found"
  return -1
fi

export MYSQL_INCLUDE_PATH
export MYSQL_LIB_PATH
export MYSQL

export MYSQLLIB=${MYSQL_LIB_PATH}
export MYSQLBIN=${MYSQL}
export MYSQLINC=${MYSQL_INCLUDE_PATH}
export MYSQL_LIB=${MYSQL_LIB_PATH}
export MYSQL_BIN=${MYSQL}
export MYSQL_INC=${MYSQL_INCLUDE_PATH}
export MYSQL_INCLUDE=${MYSQL_INCLUDE_PATH}

echo "MYSQL_LIB            is set to ${MYSQL_LIB}"
echo "MYSQL_BIN            is set to ${MYSQL_BIN}"
echo "MYSQL_INCLUDE        is set to ${MYSQL_INCLUDE}"

if [ -z "${PATH}" ]; then
  PATH=${MYSQL_BIN}
else
  PATH=${MYSQL_BIN}:${PATH}
fi

if [ -z "${LIBRARY_PATH}" ]; then
  LIBRARY_PATH=${MYSQL_LIB}
else
  LIBRARY_PATH=${MYSQL_LIB}:${LIBRARY_PATH}
fi

if [ -z "${LD_LIBRARY_PATH}" ]; then
  LD_LIBRARY_PATH=${MYSQL_LIB}
else
  LD_LIBRARY_PATH=${MYSQL_LIB}:${LD_LIBRARY_PATH}
fi

##################
# Tcl/Tk settings
##################

echo ""
echo ">> Setting Environment Variables for tcl/tk"
echo ""

if [ -f /etc/os-release ]; then
  TCL_INC=/usr/include # external
  TCL_LIB=/usr/lib64
elif [ -f /etc/debian_version ]; then
  TCL_INC=/usr/include/tcl$1
  TCL_LIB=/usr/lib/x86_64-linux-gnu
elif [ `uname -s` == "Darwin" ]; then
  TCL_INC=/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include/
  TCL_LIB=/usr/lib
fi

if [ ! -d $TCL_LIB ]; then
  echo "TCL Error: $TCL_LIB Not Found"
  return -1
fi

if [ ! -d $TCL_INC ]; then
  echo "TCL Error: $TCL_INC Not Found"
  return -1
fi

export TCL_VERSION=8.5 # hardcoded
export TCL_VER=TCL85
export TCL_LIB
export TCL_INC

echo "TCL_VERSION          is set to ${TCL_VERSION}"
echo "TCL_VER              is set to ${TCL_VER}"
echo "TCL_LIB              is set to ${TCL_LIB}"
echo "TCL_INC              is set to ${TCL_INC}"

if [ -z "${LIBRARY_PATH}" ]; then
  LIBRARY_PATH=${TCL_LIB}
else
  LIBRARY_PATH=${TCL_LIB}:${LIBRARY_PATH}
fi

if [ -z "${LD_LIBRARY_PATH}" ]; then
  LD_LIBRARY_PATH=${TCL_LIB}
else
  LD_LIBRARY_PATH=${TCL_LIB}:${LD_LIBRARY_PATH}
fi

#################
# lhapdf setting
#################

# echo ""
# echo " >> Setting Environment Variables for lhapdf"
# echo ""
# 
# export LHAVER=6.2.3 # hardcoded
# export LHAPDF=${SOFT}/lhapdf/${LHAVER}
# export LHABIN=${LHAPDF}/bin
# export LHALIB=${LHAPDF}/lib
# export LHAPATH=${SOFT}/lhapdf/pdfsets/${LHAVER}
# export LHAPDF_DATA_PATH=${LHAPATH}
# 
# if [ ! -d $LHAPDF ]; then
#   echo "LHAPDF Error: $LHAPDF ot Found"
#   return -1
# fi
# 
# if [ ! -d $LHABIN ]; then
#   echo "LHAPDF Error: $LHALIB Not Found"
#   return -1
# fi
# 
# if [ ! -d $LHALIB ]; then
#   echo "LHAPDF Error: $LHABIN Not Found"
#   return -1
# fi
# 
# if [ ! -d $LHAPATH ]; then
#   echo "LHAPDF Error: $LHAPATH Not Found"
#   return -1
# fi
# 
# echo "LHAVER               is set to ${LHAVER}"
# echo "LHAPDF               is set to ${LHAPDF}"
# echo "LHABIN               is set to ${LHABIN}"
# echo "LHALIB               is set to ${LHALIB}"
# echo "LHAPATH              is set to ${LHAPATH}"
# 
# if [ -z "${PATH}" ]; then
#   PATH=${LHABIN}
# else
#   PATH=${LHABIN}:${PATH}
# fi
# 
# if [ -z "${LIBRARY_PATH}" ]; then
#   LIBRARY_PATH=${LHALIB}
# else
#   LIBRARY_PATH=${LHALIB}:${LIBRARY_PATH}
# fi
# 
# if [ -z "${LD_LIBRARY_PATH}" ]; then
#   LD_LIBRARY_PATH=${LHALIB}
# else
#   LD_LIBRARY_PATH=${LHALIB}:${LD_LIBRARY_PATH}
# fi

################
# CLAS settings
################

echo ""
echo ">> Setting Environment variables for CLAS"
echo ""

export CLAS_ROOT=${SOFT}/clas_software_ver1 # external, ahmed dir

export CVS_RSH="/usr/bin/ssh"
export CVSROOT="ahmede@cvs.jlab.org:/group/clas/clas_cvs"
export TOP_DIR=${CLAS_ROOT}
export CLAS_BUILD=${CLAS_ROOT}
export CLAS_PACK=${CLAS_ROOT}/packages
export CLAS_CMS=${CLAS_PACK}/cms
export OSNAME=Linux64RHEL7 # hardcoded
export OS_NAME=${OSNAME}
export CLAS_TOOL=${CLAS_PACK}/ClasTool
export CLASTOOL=${CLAS_TOOL}
export CLAS_TOOL_BIN=${CLAS_TOOL}/bin/${OS_NAME}
export CLAS_SCRIPTS=${CLAS_PACK}/scripts
export CLAS_LIB=${CLAS_ROOT}/lib/${OS_NAME}
export CLAS_BIN=${CLAS_ROOT}/bin/${OS_NAME}
export HV_LOCATION=${CLAS_PACK}/Hv
export RECSIS=${CLAS_PACK}
export COBRASYS=${CLAS_PACK}/utilities/cobra
export CLAS_SLIB=${CLAS_ROOT}/slib/${OS_NAME}

export CLAS_PARMS=/group/clas/parms
export CLAS_CALDB_HOST=clasdb.jlab.org
export CLAS_CALDB_USER=clasuser
export CLAS_CALDB_DBNAME=calib
export CLAS_CALDB_RUNINDEX="calib.RunIndex"
export RECSIS_RUNTIME=/group/clas/clsrc/recsis/runtime

echo "CVS_RSH              is set to ${CVS_RSH}"
echo "CVSROOT              is set to ${CVSROOT}"
echo "CLAS_ROOT            is set to ${CLAS_ROOT}"
echo "TOP_DIR              is set to ${TOP_DIR}"
echo "CLAS_BUILD           is set to ${CLAS_BUILD}"
echo "CLAS_PACK            is set to ${CLAS_PACK}"
echo "CLAS_CMS             is set to ${CLAS_CMS}"
echo "CLASTOOL             is set to ${CLASTOOL}"
echo "CLAS_TOOL            is set to ${CLAS_TOOL}"
echo "CLAS_SCRIPTS         is set to ${CLAS_SCRIPTS}"
echo "OSNAME               is set to ${OSNAME}"
echo "OS_NAME              is set to ${OS_NAME}"
echo "CLAS_LIB             is set to ${CLAS_LIB}"
echo "CLAS_BIN             is set to ${CLAS_BIN}"
echo "HV_LOCATION          is set to ${HV_LOCATION}"
echo "RECSIS               is set to ${RECSIS}"
echo "COBRASYS             is set to ${COBRASYS}"
echo "CLAS_SLIB            is set to ${CLAS_SLIB}"
echo "CLAS_PARMS           is set to ${CLAS_PARMS}"
echo "CLAS_CALDB_HOST      is set to ${CLAS_CALDB_HOST}"
echo "CLAS_CALDB_USER      is set to ${CLAS_CALDB_USER}"
echo "CLAS_CALDB_DBNAME    is set to ${CLAS_CALDB_DBNAME}"
echo "CLAS_CALDB_RUNINDEX  is set to ${CLAS_CALDB_RUNINDEX}"
echo "RECSIS_RUNTIME       is set to ${RECSIS_RUNTIME}"

if [ -z "${PATH}" ]; then
  PATH=${CLAS_BIN}:${CLAS_TOOL_BIN}:${CLAS_SCRIPTS}:${COBRASYS}/bin
else
  PATH=${CLAS_BIN}:${CLAS_TOOL_BIN}:${CLAS_SCRIPTS}:${COBRASYS}/bin:${PATH}
fi

if [ -z "$LIBRARY_PATH" ]; then
  export LIBRARY_PATH=${CLAS_LIB}:${COBRASYS}/lib:${CLAS_SLIB}
else
  export LIBRARY_PATH=${CLAS_LIB}:${COBRASYS}/lib:${CLAS_SLIB}:$LIBRARY_PATH
fi

if [ -z "${LD_LIBRARY_PATH}" ]; then
  LD_LIBRARY_PATH=${CLAS_LIB}:${COBRASYS}/lib:${CLAS_SLIB}
else
  LD_LIBRARY_PATH=${CLAS_LIB}:${COBRASYS}/lib:${CLAS_SLIB}:${LD_LIBRARY_PATH}
fi
