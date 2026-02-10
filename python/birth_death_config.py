# =============================================================================
# File: birth_death_config.py
#
# PURPOSE: Reference Python code for integrating the birth-death module
#          into the Python GUI (spawn_editor.py / application.py).
#
# This file is importable and provides the module ID constant plus
# helper functions that can be called from the simulation loop.
# =============================================================================

import numpy as np

# -------------------------------------------------------------------------
# Module ID (must match MODULE_BIRTH_DEATH in python_interface.f95)
# -------------------------------------------------------------------------
MODULE_BIRTH_DEATH = 9

# -------------------------------------------------------------------------
# Default parameters (must match DEFAULT_* in mod_birth_death_target.f95)
# -------------------------------------------------------------------------
DEFAULT_GROWTH_RATE = 0.05
DEFAULT_CC_SCALE = 10.0


def get_module_entry():
    """
    Returns the dict entry to add to spawn_editor.py's available_modules.

    Integration:
        In spawn_editor.py, add to the `available_modules` dict:

            self.available_modules = {
                "Natural Deaths": 1,
                "Births": 2,
                "Move": 3,
                "Update Age": 4,
                "Find Mate": 5,
                "Distribute Ressources": 6,
                "Resource Mortality": 7,
                "Langevin Move": 8,
                "Birth Death": 9,        # <-- ADD THIS LINE
            }
    """
    return {"Birth Death": MODULE_BIRTH_DEATH}


def call_birth_death_step(mod_python_interface, cc_scale=None, growth_rate=None):
    """
    Calls the birth-death step directly from Python (optional wrapper).

    Only works if you added the `step_birth_death` subroutine to
    python_interface.f95 (see CHANGE 5 in the patch file).

    Args:
        mod_python_interface: The imported Fortran interface module.
        cc_scale: Carrying capacity scale (HEP -> max agents). None = use default.
        growth_rate: Logistic growth rate. None = use default.
    """
    if cc_scale is None:
        cc_scale = DEFAULT_CC_SCALE
    if growth_rate is None:
        growth_rate = DEFAULT_GROWTH_RATE

    mod_python_interface.step_birth_death(cc_scale, growth_rate)
