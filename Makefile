# Makefile for compiling Fortran modules and main program

# Directories
SRCDIR = src
BUILDDIR = build
BINDIR = bin

# Source files
MODULES = mod_kinds.f95 mod_utility.f95 mod_setup.f95 mod_agent_class.f95 mod_agent_matrix_merge.f95 mod_rnorm.f95 mod_birth_death.f95 mod_functions.f95 omp_lib.f95 mod_matrix_calculations.f95 mod_setup_agents.f95 mod_debug_agents.f95 mod_export_agents.f95
MAIN = main.f95

# Object files
MODULE_OBJS = $(addprefix $(BUILDDIR)/, $(MODULES:.f95=.o))
MAIN_OBJ = $(BUILDDIR)/$(MAIN:.f95=.o)
EXECUTABLE = $(BINDIR)/main_program
HES_PRT = $(BINDIR)/hes.prt 

# Compiler flags (adjust as needed)
FFLAGS = #-Wall -Wextra -fopenmp # example flags

# Create directories if they don't exist
$(BUILDDIR) $(BINDIR):
	mkdir -p $@

# Standardziel: Ruft die Regeln zum Kompilieren und Erstellen der ausführbaren Datei auf
all: $(EXECUTABLE) $(HES_PRT)

# Rule for creating the executable
$(EXECUTABLE): $(MODULE_OBJS) $(MAIN_OBJ) | $(BINDIR)
	gfortran -I/usr/include $(FFLAGS) $(MODULE_OBJS) $(MAIN_OBJ) -o $@ -lnetcdf -lnetcdff

# Rule for compiling module object files
$(BUILDDIR)/%.o: $(SRCDIR)/%.f95 | $(BUILDDIR)
	gfortran -I/usr/include $(FFLAGS) -c $< -o $@ -J $(BUILDDIR)/

# Rule for compiling main object file
$(BUILDDIR)/main.o: \
    $(SRCDIR)/main.f95 \
    $(BUILDDIR)/mod_matrix_calculations.o \
    $(BUILDDIR)/mod_agent_class.o \
    $(BUILDDIR)/mod_setup_agents.o \
    $(BUILDDIR)/mod_debug_agents.o \
    $(BUILDDIR)/mod_export_agents.o | $(BUILDDIR)
	gfortran -I/usr/include $(FFLAGS) -c $< -o $@ -J $(BUILDDIR)/

$(HES_PRT): hes.prt | $(BINDIR)
	cp hes.prt $@

# Clean rule
clean:
	rm -rf $(BUILDDIR)/* $(BINDIR)/*

.PHONY: clean
