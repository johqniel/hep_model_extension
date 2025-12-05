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
	globals/mod_basic_config.f95 \
	globals/mod_config.f95 \
	setup/mod_read_inputs.f95 \
	utilities/mod_calculations.f95 \
	data_structures/mod_hashmap.f95 \
	data_management/mod_export_hep.f95 \
	data_structures/mod_grid_id.f95 \
	data_structures/mod_agent_world.f95 \
	data_management/mod_export_agents_hash.f95 \
	old_program/mod_kinds.f95 \
	old_program/mod_rnorm.f95 \
	old_program/mod_functions.f95 \
	agent_management/mod_agent_core.f95 \
	simulation_modules/mod_modules_hash.f95 \
	setup/mod_setup.f95 # Makefile for compiling Fortran modules and main program

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

# Test files
TEST_SRCS = $(wildcard $(SRCDIR)/tests/*.f95)
TEST_OBJS = $(patsubst $(SRCDIR)/%.f95, $(BUILDDIR)/%.o, $(TEST_SRCS))
TEST_EXECUTABLES = $(patsubst $(SRCDIR)/tests/%.f95, $(BINDIR)/%, $(TEST_SRCS))

# Targets
all: $(MODULE_OBJS) $(EXECUTABLES) $(TEST_EXECUTABLES) $(HES_PRT) run_tests

# Rule for test executables
$(BINDIR)/%: $(BUILDDIR)/tests/%.o $(MODULE_OBJS) | $(BINDIR)
	$(FC) -I/usr/include $(FFLAGS) $(MODULE_OBJS) $< -o $@ -lnetcdf -lnetcdff

# Run tests
run_tests: $(TEST_EXECUTABLES)
	@for test in $(TEST_EXECUTABLES); do \
		echo "Running $$test..."; \
		./$$test || exit 1; \
	done

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

# Explicit dependencies
$(BUILDDIR)/data_structures/mod_agent_world.o: $(BUILDDIR)/setup/mod_read_inputs.o
$(BUILDDIR)/setup/mod_read_inputs.o: $(BUILDDIR)/globals/mod_basic_config.o
$(BUILDDIR)/setup/mod_setup.o: $(BUILDDIR)/data_structures/mod_agent_world.o $(BUILDDIR)/old_program/mod_functions.o
