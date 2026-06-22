"""
utils.py — Shared helpers for the HEP simulation Python wrapper.

Centralises things that were previously duplicated across application.py,
simulation.py, headless_simulation.py, export_simulation.py, and spawn_editor.py.
"""

import sys
import os


# ---------------------------------------------------------------------------
# Fortran module loader
# ---------------------------------------------------------------------------

def load_mpi(exit_on_failure=False):
    """Load and return the mod_python_interface Fortran module.

    Parameters
    ----------
    exit_on_failure : bool
        If True, call sys.exit(1) when the import fails (suitable for the
        main entry point). If False, return None so callers can handle it.
    """
    sys.path.append(os.path.abspath(os.path.join(os.path.dirname(__file__), '..')))
    try:
        import mod_python_interface
        # Fix for f2py module nesting
        if hasattr(mod_python_interface, 'mod_python_interface'):
            return mod_python_interface.mod_python_interface
        return mod_python_interface
    except ImportError as e:
        print(f"Error importing mod_python_interface: {e}")
        if exit_on_failure:
            sys.exit(1)
        return None


# ---------------------------------------------------------------------------
# Multiprocessing
# ---------------------------------------------------------------------------

def multiprocessing_safe_spawn():
    """Set multiprocessing start method to 'spawn' (idempotent)."""
    import multiprocessing
    try:
        multiprocessing.set_start_method('spawn')
    except RuntimeError:
        pass


# ---------------------------------------------------------------------------
# Qt helpers (imported lazily to avoid forcing PyQt5 at module import time)
# ---------------------------------------------------------------------------

def show_selectable_error(parent, title, text):
    """Show a QMessageBox with selectable text for copy-pasting error details."""
    from PyQt5 import QtWidgets, QtCore
    msg_box = QtWidgets.QMessageBox(parent)
    msg_box.setIcon(QtWidgets.QMessageBox.Critical)
    msg_box.setWindowTitle(title)
    msg_box.setText(text)
    msg_box.setTextInteractionFlags(QtCore.Qt.TextSelectableByMouse)
    msg_box.exec_()


def update_button_progress(button, percent, text, color="green"):
    """Update a QPushButton to show a linear-gradient progress bar.

    Parameters
    ----------
    button  : QPushButton
    percent : float   0–100
    text    : str     label shown on the button
    color   : str     "green" or "red"
    """
    from PyQt5 import QtWidgets
    if percent >= 100:
        button.setStyleSheet("background-color: red; color: white; font-weight: bold;")
        button.setText(text)
        return

    c_code = "#90EE90" if color == "green" else "#ff6666"
    style = f"""
        QPushButton {{
            background-color: qlineargradient(spread:pad, x1:0, y1:0, x2:1, y2:0,
                                              stop:0 {c_code}, stop:{percent / 100.0} {c_code},
                                              stop:{percent / 100.0 + 0.001} #e1e1e1, stop:1 #e1e1e1);
            border: 1px solid #777;
            border-radius: 4px;
            padding: 4px;
            color: black;
        }}
    """
    button.setStyleSheet(style)
    button.setText(f"{text} ({int(percent)}%)")
    QtWidgets.QApplication.processEvents()


# ---------------------------------------------------------------------------
# Module registry
# ---------------------------------------------------------------------------

# Single source of truth for simulation modules.
# To add a new module, append a dict here — all files pick it up automatically.
MODULE_REGISTRY = [
    {"id": 12, "name": "Reviewed Death",              "group": "ReviewedModules", "author": "Daniel & Sandesh",    "file": "mod_reviewed_modules.f95"},
    {"id": 13, "name": "Reviewed Birth",              "group": "ReviewedModules", "author": "Daniel & Sandesh",    "file": "mod_reviewed_modules.f95"},
    {"id": 14, "name": "Move Children to Mothers",    "group": "ReviewedModules", "author": "Daniel & Sandesh",    "file": "mod_reviewed_modules.f95"},
    {"id": 21, "name": "Reviewed Agent Motion",       "group": "ReviewedModules", "author": "Reviewed",            "file": "mod_reviewed_modules.f95"},
    {"id": 22, "name": "Cluster Death (No Interaction)", "group": "ReviewedModules", "author": "Daniel",           "file": "mod_birth_death_new.f95"},
    {"id": 23, "name": "Cluster Birth (No Interaction)", "group": "ReviewedModules", "author": "Daniel",           "file": "mod_birth_death_new.f95"},
    {"id": 24, "name": "Creativity (C3)",             "group": "ReviewedModules", "author": "Y. Shao",             "file": "mod_creativity.f95"},
    {"id": 25, "name": "Cluster Creativity (C3)",     "group": "ReviewedModules", "author": "Y. Shao",             "file": "mod_creativity.f95"},
    {"id": 26, "name": "Creativity Simple (C3)",      "group": "ReviewedModules", "author": "Y. Shao / D. Nogues", "file": "mod_creativity_simple.f95"},
    {"id": 27, "name": "Creativity Fast (C3)",        "group": "ReviewedModules", "author": "Y. Shao / D. Nogues", "file": "mod_creativity_fast.f95"},
    {"id": 28, "name": "Cluster Death (Shared MC)",   "group": "ReviewedModules", "author": "Daniel",              "file": "mod_birth_death_new.f95"},
    {"id": 29, "name": "Cluster Birth (Shared MC)",   "group": "ReviewedModules", "author": "Daniel",              "file": "mod_birth_death_new.f95"},
]

# Derived lookups — built once at import time.
MODULE_NAMES_MAP = {m["id"]: m["name"] for m in MODULE_REGISTRY}   # {12: "Reviewed Death", ...}
MOD_NAME_TO_ID   = {m["name"]: m["id"] for m in MODULE_REGISTRY}   # {"Reviewed Death": 12, ...}
MOD_ID_TO_NAME   = MODULE_NAMES_MAP                                  # alias
