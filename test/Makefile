#Compiler and Linker
FC           := gfortran

#The Directories, Source, Includes, Objects, Binary and Resources
SRCDIR      := ../src
INCDIR      := ../inc
BUILDDIR    := obj
TESTDIR     := .
TARGETDIR   := bin
SRCEXT      := f90
OBJEXT      := o
MODEXT      := mod

#Flags
FLAGS       := -O3
MODFLAGS    := -I$(BUILDDIR) -J$(BUILDDIR)

#Objects and dependencies
SOURCES        := $(shell find $(SRCDIR) -type f -name *.$(SRCEXT))
OBJECTS        := $(patsubst $(SRCDIR)/%,$(BUILDDIR)/%,$(SOURCES:.$(SRCEXT)=.$(OBJEXT)))
FOREIGNOBJS    := $(shell find $(INCDIR) -type f -name *.$(OBJEXT))
FOREIGNMODS    := $(shell find $(INCDIR) -type f -name *.$(MODEXT))
TEST_INTERP_DEP  := $(BUILDDIR)/interp.o

#Link
test_interp: $(BUILDDIR)/test_interp.o $(TEST_INTERP_DEP)
	$(FC) -o $(TARGETDIR)/test_interp $^ $(FOREIGNOBJS) $(MODFLAGS)

#Compile
$(BUILDDIR)/test_interp.o: test_interp.f90 $(TEST_INTERP_DEP)
	@mkdir -p $(dir $@)
	@cp -f $(FOREIGNMODS) $(BUILDDIR)
	$(FC) $(FLAGS) -c -o $@ $< $(MODFLAGS)

$(BUILDDIR)/%.$(OBJEXT): $(SRCDIR)/%.$(SRCEXT)
	@mkdir -p $(dir $@)
	@cp -f $(FOREIGNMODS) $(BUILDDIR)
	$(FC) $(FLAGS) -c -o $@ $< $(MODFLAGS)

#Default make
all: directories test_interp
#all: $(info    BUILDDIR is $(BUILDDIR)); directories test_interp

#Remake
remake: cleaner all

directories:
	@mkdir -p $(TARGETDIR)
	@mkdir -p $(BUILDDIR)

#Clean only Objecst
clean:
	@$(RM) -rf $(BUILDDIR)

#Full Clean, Objects and Binaries
cleaner: clean
	@$(RM) -rf $(TARGETDIR)

print-%  : ; @echo $* = $($*)
#Non-File Targets
.PHONY: all remake clean cleaner print-
