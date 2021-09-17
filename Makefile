#Compiler and Linker
FC           := gfortran

#The Target Binary Program
TARGET1      := optimize_fit_on_RF_field
TARGETS      := $(TARGET1)

#The Directories, Source, Includes, Objects, Binary and Resources
SRCDIR      := src
INCDIR      := inc
BUILDDIR    := obj
TARGETDIR   := bin
SRCEXT      := f90
OBJEXT      := o
MODEXT      := mod

#Flags, Libraries and Includes
#FLAGS       := -O3
FLAGS       := -O3 -fcheck=all -g -ffpe-trap=zero,invalid,overflow,underflow
MODFLAGS    := -I$(BUILDDIR) -J$(BUILDDIR) --openmp

#---------------------------------------------------------------------------------
#DO NOT EDIT BELOW THIS LINE
#---------------------------------------------------------------------------------
SOURCES        := $(shell find $(SRCDIR) -type f -name *.$(SRCEXT))
OBJECTS        := $(patsubst $(SRCDIR)/%,$(BUILDDIR)/%,$(SOURCES:.$(SRCEXT)=.$(OBJEXT)))
FOREIGNOBJS    := $(shell find $(INCDIR) -type f -name *.$(OBJEXT))
FOREIGNMODS    := $(shell find $(INCDIR) -type f -name *.$(MODEXT))
OPTIMIZEDEP    := $(BUILDDIR)/mean_squared_error.o $(BUILDDIR)/interp.o $(BUILDDIR)/io.o $(BUILDDIR)/utils.o 
MSEDEP         := $(BUILDDIR)/interp.o $(BUILDDIR)/utils.o

#Defauilt Make
all: directories $(TARGETS)

#Remake
remake: cleaner all

#Make the Directories
directories:
	@mkdir -p $(TARGETDIR)
	@mkdir -p $(BUILDDIR)

#Clean only Objecst
clean:
	@$(RM) -rf $(BUILDDIR)

#Full Clean, Objects and Binaries
cleaner: clean
	@$(RM) -rf $(TARGETDIR)

#Link
$(TARGET1): $(BUILDDIR)/$(TARGET1)_main.o $(OPTIMIZEDEP)
	$(FC) -o $(TARGETDIR)/$(TARGET1) $^ $(FOREIGNOBJS) $(MODFLAGS)

#Compile
$(BUILDDIR)/$(TARGET1)_main.o: $(SRCDIR)/$(TARGET1)_main.f90 $(OPTIMIZEDEP)
	@mkdir -p $(dir $@)
	@cp -f $(FOREIGNMODS) $(BUILDDIR)
	$(FC) $(FLAGS) -c -o $@ $< $(MODFLAGS)

$(BUILDDIR)/mean_squared_error.o: $(SRCDIR)/mean_squared_error.f90 $(MSEDEP)
	@mkdir -p $(dir $@)
	@cp -f $(FOREIGNMODS) $(BUILDDIR)
	$(FC) $(FLAGS) -c -o $@ $< $(MODFLAGS)

$(BUILDDIR)/%.$(OBJEXT): $(SRCDIR)/%.$(SRCEXT)
	@mkdir -p $(dir $@)
	@cp -f $(FOREIGNMODS) $(BUILDDIR)
	$(FC) $(FLAGS) -c -o $@ $< $(MODFLAGS)

print-%  : ; @echo $* = $($*)
#Non-File Targets
.PHONY: all remake clean cleaner print-
