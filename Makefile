# Makefile for compiling Fortran modules and main program

# Directories
SRCDIR = src
BUILDDIR = build
BINDIR = bin

# Compiler
FC = gfortran
FFLAGS = #-Wall -Wextra -fopenmp

# Source files
MODULES = \
	globals/mod_constants.f95 \
	globals/mod_parameters.f95 \
	globals/mod_common_variables.f95 \
	globals/mod_paths_and_strings.f95 \
	utilities/mod_kinds.f95 \
	utilities/mod_utility.f95 \
	setup/mod_setup_hep.f95 \
	utilities/mod_calculations.f95 \
	agent_management/mod_agent_class.f95 \
	utilities/mod_rnorm.f95 \
	utilities/mod_functions.f95 \
	omp/omp_lib.f95 \
	data_management/mod_export_agents.f95 \
	data_management/mod_export_hep.f95 \
	grid_management/mod_grid_utilities.f95 \
	grid_management/mod_grid.f95 \
	agent_management/mod_agent_tracking.f95 \
	matrix_calculations/mod_matrix_calculations.f95 \
	test_and_debug/mod_debug_agents.f95 \
	setup/mod_setup_agents.f95 \
	simulation_modules/mod_movement.f95 \
	simulation_modules/mod_birth_death_example.f95 \
	test_and_debug/mod_debug_grid.f95

MAIN = main.f95

MAIN_SRCS =  main_agb.f95
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
