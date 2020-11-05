OBJDIR    = obj/
BINDIR    = bin/
EXE      = $(BINDIR)/txt2part.exe

#HHINCDIR  = include/
#INC_HH    = $(notdir $(wildcard $(HHINCDIR)*.hh))
#HH_INC    = $(addprefix $(HHINCDIR),$(INC_HH))

CCSRCDIR  = ./
SRC_CC    = $(notdir $(wildcard $(CCSRCDIR)*.c))
CC_SRC    = $(addprefix $(CCSRCDIR),$(SRC_CC))
OBJ_CC    = $(SRC_CC:.c=.o)
CC_OBJ    = $(addprefix $(OBJDIR),$(OBJ_CC))

#FINCDIR   = include_f/
#INC_F     = $(notdir $(wildcard $(FINCDIR)*.inc))
#F_INC     = $(addprefix $(FINCDIR),$(INC_F))

#FSRCDIR   = src_f/
#SRC_F     = $(notdir $(wildcard $(FSRCDIR)*.f))
#F_SRC     = $(addprefix $(FSRCDIR),$(SRC_F))
#OBJ_F     = $(SRC_F:.f=.o)
#F_OBJ     = $(addprefix $(OBJDIR),$(OBJ_F))

CXX       = gcc
#FF        = gfortran
LD        = gcc

#CXXFLAGS := -m64 -Wall -fPIC -g -O2 $(shell root-config --cflags) -Woverloaded-virtual -DgFortran
CXXFLAGS = -m64 -Wall -fPIC -Wno-deprecated -g -O2
#FFLAGS   = -m64 -g -O2 -fno-automatic -fno-second-underscore -ffixed-line-length-none \
#           -funroll-loops -Wall -Wunused -ffast-math -finit-local-zero -fno-align-commons
LDFLAGS  = -m64 -g -O2

MKDIR_P  = mkdir -p

MY_INCLUDES     = -I./
CERN_INCLUDES   = -I$(CERN_INC)
#ROOT_INCLUDES   = -I$(shell root-config --incdir)
CLAS_INCLUDES   = -I$(CLAS_PACK)/include
#CLASTOOL_INCLUDES   := -I$(CLAS_TOOL)/include
LHAPDF_INCLUDES = 

MY_LIBRARIES     = 
CERN_LIBRARIES   = -L$(CERN_LIB) -lpacklib -lkernlib -lnsl -lcrypt -ldl -lm
#ROOT_LIBRARIES   = $(shell root-config --glibs)
CLAS_LIBRARIES   = -L$(CLAS_LIB) -lscaler -lclasutil -lc_bos_io -lmapmanager -lfputil -lbos -lfpack -lc_cern
#CLASTOOL_LIBRARIES  := -L$(CLAS_ROOT)/slib/$(OSNAME) -lClasTool -lClasBanks -lDSTReader
LHAPDF_LIBRARIES = 
FORT_LIBRARIES   = -lgfortran

INCLUDES := $(MY_INCLUDES) \
            $(CERN_INCLUDES) \
            $(ROOT_INCLUDES) \
            $(CLAS_INCLUDES) \
    	    $(CLASTOOL_INCLUDES) \
            $(LHAPDF_INCLUDES)

LIBRARIES := $(MY_LIBRARIES) \
             $(ROOT_LIBRARIES) \
             $(CLAS_LIBRARIES) \
             $(CLASTOOL_LIBRARIES) \
             $(PDF_LIBRARIES) \
             $(CERN_LIBRARIES) \
             $(FORT_LIBRARIES) 

all: $(EXE)

$(EXE): $(OBJDIR) $(CC_OBJ)
	@echo
	@echo 'Start compilation at : '
	@date 
	$(MKDIR_P) $(BINDIR)
	$(LD) $(LDFLAGS) $(INCLUDES) -o $@ $(CC_OBJ) $(LIBRARIES)
	@echo ' '
	@echo 'done.'

$(OBJDIR):
	$(MKDIR_P) $(OBJDIR)

$(OBJDIR)%.o: $(CCSRCDIR)%.c
	@echo
	@echo compiling $@
	$(CXX) $(CXXFLAGS) $(INCLUDES) -c $< -o $@

#$(OBJDIR)%.o: $(FSRCDIR)%.f
#	@echo
#	@echo compiling $@
#	$(FF) $(FFLAGS) $(INCLUDES) -c $< -o $@

clean: cleanccobj

cleanccobj:
	/bin/rm -f $(CC_OBJ)

#cleanff:
#	/bin/rm -f $(F_OBJ) $(EXEPDF) $(EXELHA)

cleanall:
	/bin/rm -rf $(BINDIR) $(OBJDIR)

help: 
	@echo $(HHINCDIR)
	@echo $(INC_HH)
	@echo $(HH_INC)
	@echo $(CCSRCDIR)
	@echo $(SRC_CC)
	@echo $(CC_SRC)
	@echo $(OBJ_CC)
	@echo $(CC_OBJ)
	@echo $(FINCDIR)
	@echo $(INC_F)
	@echo $(F_INC)
	@echo $(FSRCDIR)
	@echo $(SRC_F)
	@echo $(F_SRC)
	@echo $(OBJ_F)
	@echo $(F_OBJ)
