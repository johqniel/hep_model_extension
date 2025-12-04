# Makefile for compiling Fortran modules and main program

# Directories
SRCDIR = src
BUILDDIR = build
BINDIR = bin

# Compiler
FC = gfortran
FFLAGS = #-Wall -Wextra -Wunused -Wuninitialized 
FFLAGS = -Wall -Wextra -pedantic -fcheck=all -fbacktrace -g -O0 -fopenmp
#FFLAGS = #-Wall -Wextra -fopenmp

# Source files
MODULES = \
	globals/mod_globals.f95 \
	globals/mod_config.f95 \
	setup/mod_setup.f95 \
	utilities/mod_calculations.f95 \
	data_structures/mod_hashmap.f95 \
	data_management/mod_export_hep.f95 \
	data_structures/mod_grid_id.f95 \
	data_structures/mod_agent_world.f95 \
	data_management/mod_export_agents_hash.f95 \
	old_program/mod_kinds.f95 \
	old_program/mod_rnorm.f95 \
	old_program/omp_lib.f95 \
	old_program/mod_utility.f95 \
	old_program/mod_functions.f95 \
	old_program/mod_setup_hep.f95 \
	old_program/mod_matrix_calculations.f95 \
	agent_management/mod_agent_core.f95 \
	simulation_modules/mod_modules_hash.f95 # Makefile for compiling Fortran modules and main program

# Directories
SRCDIR = src
BUILDDIR = build
BINDIR = bin

# Compiler
FC = gfortran
FFLAGS = #-Wall -Wextra -Wunused -Wuninitialized 
FFLAGS = -Wall -Wextra -pedantic -fcheck=all -fbacktrace -g -O0 -fopenmp
#FFLAGS = #-Wall -Wextra -fopenmp


#MAIN = main.f95 

#MAIN_SRCS = main_new.f95 main_runtime_test.f95 main_agb.f95 main_args.f95 main_demo.f95 
MAIN_SRCS =  main.f95
MAIN_OBJS = $(patsubst %.f95, $(BUILDDIR)/%.o, $(MAIN_SRCS))
EXECUTABLES = $(patsubst %.f95, $(BINDIR)/%, $(MAIN_SRCS))

# Object files
MODULE_OBJS = $(patsubst %.f95, $(BUILDDIR)/%.o, $(MODULES))
MAIN_OBJ = $(BUILDDIR)/$(MAIN:.f95=.o)
#EXECUTABLE = $(BINDIR)/main_program
HES_PRT = $(BINDIR)/hes.prt

# Targets
all: $(MODULE_OBJS) $(EXECUTABLES) $(HES_PRT)

$(BINDIR)/%: $(BUILDDIR)/%.o $(MODULE_OBJS) | $(BINDIR)
	$(FC) -I/usr/include $(FFLAGS) $(MODULE_OBJS) $< -o $@ -lnetcdf -lnetcdff



#$(EXECUTABLE): $(MODULE_OBJS) $(MAIN_OBJ) | $(BINDIR)
#	$(FC) -I/usr/include $(FFLAGS) $(MODULE_OBJS) $(MAIN_OBJ) -o $@ -lnetcdf -lnetcdff

# Generic compile rule
$(BUILDDIR)/%.o: $(SRCDIR)/%.f95 | $(BUILDDIR)
	mkdir -p $(dir $@)
	$(FC) -I/usr/include $(FFLAGS) -c $< -o $@ -J $(BUILDDIR)/

# Ensure build/bin directories exist
$(BUILDDIR) $(BINDIR):
	mkdir -p $@

# Dummy file copy
$(HES_PRT): hes.prt | $(BINDIR)
	cp hes.prt $@

# Clean rule
clean:
	rm -rf $(BUILDDIR)/* $(BINDIR)/*

.PHONY: all clean
