#Makefile to build application using lepto
.PHONY: all clean

arch := -m64

OBJDIR:=obj
BINDIR:=bin
SRCDIRC:=src
SRCDIRF:=src_f

APPLICATION := $(BINDIR)/lepto.exe
#APPLICATION := $(BINDIR)/lepto_bkg.exe
vpath %.f $(SRCDIRF):.
vpath %.cc $(SRCDIRC):.
vpath %.o $(OBJDIR)

FSRC := qp1.f lepto-6.5.1.f pythia5721.f
#FSRC := qp1_bkg.f lepto-6.5.1.f pythia5721.f
CSRC := csparam.cc
FOBJS := $(FSRC:.f=.o)
FOBJS := $(addprefix $(OBJDIR)/, $(FOBJS))
COBJS =$(CSRC:.cc=.o)
COBJS := $(addprefix $(OBJDIR)/, $(COBJS))
FC := gfortran

ROOTLIB := $(shell root-config --glibs)
CERNLIB	:= $(shell cernlib packlib mathlib kernlib)
CLASLIB := -L$(CLAS_LIB) -lbosio
FFLAGS := $(arch) -g -Wall -v -O2 -ffixed-line-length-none -fno-second-underscore -fno-automatic -funroll-loops -Wunused -I$(CLAS_PACK)/include
CXXFLAGS := $(arch) -g -c -O -Wall -fPIC  -Wno-deprecated $(shell root-config --cflags ) -I. -I./include -I$(CLAS_PACK)/include
LDFLAGS := $(shell root-config --ldflags) -D_REENTRANT -Wall
LIBS :=  $(ROOTLIB) -lgraflib -lpacklib -lnsl -lkernlib -lnsl -lgfortran -lXm -lX11 -lm -lz -lGui \
         $(CLASLIB) -L$(CERN_LIB) -lmathlib\
         -lscaler -lc_cern -lpdflib804 -ljetset74 -lmathlib -lkernlib -ltcl8.5 -lnsl

check_dir = @test -d $1 || (mkdir $1 && echo "Creating directory: $1")

all : $(APPLICATION)

$(APPLICATION):$(FOBJS) $(COBJS)
	$(call check_dir, $(@D))
	@echo $(if $(CERN_LIB), CERN_LIB:= $(CERN_LIB),You must to supply cernlib directory)
	@echo $(if $(CLAS_LIB), CLAS_LIB:= $(CLAS_LIB),You must to supply claslib directory)
	$(CXX) -o $@ $(LDFLAGS) $^ $(LIBS)

$(TEST):$(FOBJS_TEST) $(COBJS)
	$(call check_dir, $(@D))
	@echo $(if $(CERN_LIB), CERN_LIB:= $(CERN_LIB),You must to supply cernlib directory)
	@echo $(if $(CLAS_LIB), CLAS_LIB:= $(CLAS_LIB),You must to supply claslib directory)
	$(CXX) -o $@ $(LDFLAGS) $^ $(LIBS)

$(APP2):$(FOBJS_APP2) $(COBJS)
	$(call check_dir, $(@D))
	@echo $(if $(CERN_LIB), CERN_LIB:= $(CERN_LIB),You must to supply cernlib directory)
	@echo $(if $(CLAS_LIB), CLAS_LIB:= $(CLAS_LIB),You must to supply claslib directory)
	$(CXX) -o $@ $(LDFLAGS) $^ $(LIBS)

$(OBJDIR)/%.o : %.f
	$(call check_dir, $(@D))
	$(FC) -c $(FFLAGS) -o $@ $< 

$(OBJDIR)/%.o : %.cc
	$(call check_dir, $(@D))
	$(CXX) -c $(CXXFLAGS) -o $@ $< 

clean:
	rm -f *~
