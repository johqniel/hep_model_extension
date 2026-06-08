"""
Test Simulation — Combinatorial Parameter Sweep

Provides:
  - TestSimulationConfigDialog: configure sweep parameters
  - TestSimulationSuiteWindow: run and monitor the sweep (reuses run_simulation_process)
  - patch_namelist_file: generate temp .nml files with overridden values
"""
import sys, os, re, tempfile, shutil, itertools, multiprocessing, time, json
try:
    multiprocessing.set_start_method('spawn')
except RuntimeError:
    pass
import numpy as np
import pyqtgraph as pg
from PyQt5 import QtCore, QtWidgets, QtGui

from full_simulation import run_simulation_process, show_selectable_error

# ---------------------------------------------------------------------------
# Module registry (mirrors spawn_editor.py)
# ---------------------------------------------------------------------------
MODULE_REGISTRY = [
    {"id": 12, "name": "Reviewed Death"},
    {"id": 13, "name": "Reviewed Birth"},
    {"id": 14, "name": "Move Children to Mothers"},
    {"id": 21, "name": "Reviewed Agent Motion"},
    {"id": 22, "name": "Cluster Death (New)"},
    {"id": 23, "name": "Cluster Birth (New)"},
    {"id": 24, "name": "Creativity (C3)"},
    {"id": 25, "name": "Cluster Creativity (C3)"},
    {"id": 26, "name": "Creativity Simple (C3)"},
    {"id": 27, "name": "Creativity Fast (C3)"},
    {"id": 1,  "name": "Natural Deaths"},
    {"id": 5,  "name": "Find Mate"},
    {"id": 6,  "name": "Distribute Ressources"},
    {"id": 7,  "name": "Resource Mortality"},
    {"id": 9,  "name": "Birth Death"},
    {"id": 10, "name": "Verhulst Pressure"},
    {"id": 17, "name": "Yaping Move"},
    {"id": 18, "name": "Yaping Birth Grid"},
    {"id": 19, "name": "Yaping Death AGB"},
    {"id": 20, "name": "Yaping Death Grid"},
]
_MOD_NAME_TO_ID = {m["name"]: m["id"] for m in MODULE_REGISTRY}
_MOD_ID_TO_NAME = {m["id"]: m["name"] for m in MODULE_REGISTRY}

# ---------------------------------------------------------------------------
# Config variable suggestions (common ones from world_config)
# ---------------------------------------------------------------------------
SUGGESTED_CONFIG_VARS = [
    "cb1", "cb2", "cb3", "r", "NC", "Kmin", "Kmax", "NC_per_hep",
    "c3_Pmax1", "c3_Alpha1", "c3_Phi_l1", "c3_tau1",
    "c3_Pmax2", "c3_k2", "c3_Phi_l2", "c3_tau2",
    "c3_l", "c3_R", "c3_search_r_cap",
    "c3_min_creativity", "c3_max_creativity", "max_high_creativity_fast",
    "dt", "watershed_threshold", "cluster_update_interval",
    "c3_individual_update_interval", "c3_cluster_update_interval",
]


# ---------------------------------------------------------------------------
# Namelist patching
# ---------------------------------------------------------------------------
def patch_namelist_file(base_path, overrides, output_path):
    """Copy base .nml and replace values for keys in overrides dict."""
    with open(base_path, 'r') as f:
        lines = f.readlines()

    for key, val in overrides.items():
        pattern = re.compile(
            r'^(\s*' + re.escape(key) + r'\s*=\s*)(.+?)(\s*[,!].*|$)',
            re.IGNORECASE
        )
        found = False
        for i, line in enumerate(lines):
            m = pattern.match(line)
            if m:
                # Preserve trailing comment if any
                trail = m.group(3)
                lines[i] = f"{m.group(1)}{val}{trail}\n"
                found = True
                break
        if not found:
            # Insert before the closing '/' of &config
            for i in range(len(lines) - 1, -1, -1):
                if lines[i].strip() == '/':
                    lines.insert(i, f"    {key} = {val}\n")
                    break

    with open(output_path, 'w') as f:
        f.writelines(lines)


# ---------------------------------------------------------------------------
# Dual-list widget helper
# ---------------------------------------------------------------------------
class DualListWidget(QtWidgets.QWidget):
    """Two list widgets with Add/Remove buttons between them."""
    changed = QtCore.pyqtSignal()

    def __init__(self, available_items, label_left="Available", label_right="Selected", parent=None):
        super().__init__(parent)
        layout = QtWidgets.QHBoxLayout(self)
        layout.setContentsMargins(0, 0, 0, 0)

        # Left list
        left_col = QtWidgets.QVBoxLayout()
        left_col.addWidget(QtWidgets.QLabel(label_left))
        self.list_available = QtWidgets.QListWidget()
        self.list_available.setSelectionMode(QtWidgets.QAbstractItemView.ExtendedSelection)
        for name in available_items:
            self.list_available.addItem(name)
        left_col.addWidget(self.list_available)
        layout.addLayout(left_col)

        # Buttons
        btn_col = QtWidgets.QVBoxLayout()
        btn_col.addStretch()
        btn_add = QtWidgets.QPushButton("→")
        btn_add.setFixedWidth(40)
        btn_add.clicked.connect(self._add)
        btn_col.addWidget(btn_add)
        btn_rem = QtWidgets.QPushButton("←")
        btn_rem.setFixedWidth(40)
        btn_rem.clicked.connect(self._remove)
        btn_col.addWidget(btn_rem)
        btn_col.addStretch()
        layout.addLayout(btn_col)

        # Right list
        right_col = QtWidgets.QVBoxLayout()
        right_col.addWidget(QtWidgets.QLabel(label_right))
        self.list_selected = QtWidgets.QListWidget()
        self.list_selected.setSelectionMode(QtWidgets.QAbstractItemView.ExtendedSelection)
        right_col.addWidget(self.list_selected)
        layout.addLayout(right_col)

    def _add(self):
        for item in self.list_available.selectedItems():
            name = item.text()
            # Don't add duplicates
            if not self.list_selected.findItems(name, QtCore.Qt.MatchExactly):
                self.list_selected.addItem(name)
        self.changed.emit()

    def _remove(self):
        for item in self.list_selected.selectedItems():
            self.list_selected.takeItem(self.list_selected.row(item))
        self.changed.emit()

    def get_selected_names(self):
        return [self.list_selected.item(i).text() for i in range(self.list_selected.count())]

    def set_selected_names(self, names):
        self.list_selected.clear()
        for name in names:
            self.list_selected.addItem(name)
        self.changed.emit()


# ---------------------------------------------------------------------------
# Module Binder Widget & Canvas
# ---------------------------------------------------------------------------
class ModuleBinderCanvas(QtWidgets.QWidget):
    changed = QtCore.pyqtSignal()

    def __init__(self, parent=None):
        super().__init__(parent)
        self.setMinimumHeight(200)
        self.setMouseTracking(True)
        self.setFocusPolicy(QtCore.Qt.StrongFocus)
        self.items = []
        self.links = set()  # set of (idx1, idx2) with idx1 < idx2
        
        self.drag_start_idx = None
        self.drag_current_pos = None
        self.hovered_dot_idx = None
        self.selected_item_idx = None
        
        self.dot_radius = 8
        self.card_height = 36
        self.card_width = 220
        self.spacing = 15

    def add_item(self, name):
        if name not in self.items:
            self.items.append(name)
            self.setMinimumHeight(max(200, 30 + len(self.items) * (self.card_height + self.spacing)))
            self.update()
            self.changed.emit()

    def remove_item(self, name):
        if name in self.items:
            idx = self.items.index(name)
            self.items.remove(name)
            
            # Rebuild links with updated indices
            new_links = set()
            for i1, i2 in self.links:
                if i1 == idx or i2 == idx:
                    continue
                new_i1 = i1 - 1 if i1 > idx else i1
                new_i2 = i2 - 1 if i2 > idx else i2
                new_links.add((new_i1, new_i2))
            self.links = new_links
            
            # Adjust selection index
            if self.selected_item_idx == idx:
                self.selected_item_idx = None
            elif self.selected_item_idx is not None and self.selected_item_idx > idx:
                self.selected_item_idx -= 1
            
            self.setMinimumHeight(max(200, 30 + len(self.items) * (self.card_height + self.spacing)))
            self.update()
            self.changed.emit()

    def get_items(self):
        return list(self.items)

    def set_state(self, items, links):
        self.items = list(items)
        self.links = set(tuple(link) for link in links)
        self.selected_item_idx = None
        self.setMinimumHeight(max(200, 30 + len(self.items) * (self.card_height + self.spacing)))
        self.update()
        self.changed.emit()

    def _get_item_rect(self, index):
        y = 10 + index * (self.card_height + self.spacing)
        x = 10
        return QtCore.QRect(x, y, self.card_width, self.card_height)

    def _get_dot_pos(self, index):
        rect = self._get_item_rect(index)
        return QtCore.QPoint(rect.right(), rect.top() + rect.height() // 2)

    def _get_dot_under_mouse(self, pos):
        for i in range(len(self.items)):
            dot_pos = self._get_dot_pos(i)
            dx = pos.x() - dot_pos.x()
            dy = pos.y() - dot_pos.y()
            if dx*dx + dy*dy <= (self.dot_radius + 4)**2:
                return i
        return None

    def paintEvent(self, event):
        painter = QtGui.QPainter(self)
        painter.setRenderHint(QtGui.QPainter.Antialiasing)

        # Background
        bg_color = self.palette().color(QtGui.QPalette.Window)
        painter.fillRect(self.rect(), bg_color)

        # Draw links
        pen = QtGui.QPen(QtGui.QColor("#FF9800"), 3)
        painter.setPen(pen)
        for i1, i2 in self.links:
            p1 = self._get_dot_pos(i1)
            p2 = self._get_dot_pos(i2)
            painter.drawLine(p1, p2)

        # Draw drag line
        if self.drag_start_idx is not None and self.drag_current_pos is not None:
            painter.setPen(QtGui.QPen(QtGui.QColor("#FF9800"), 2, QtCore.Qt.DashLine))
            p1 = self._get_dot_pos(self.drag_start_idx)
            painter.drawLine(p1, self.drag_current_pos)

        # Draw cards
        card_bg = self.palette().color(QtGui.QPalette.Button)
        border_color = self.palette().color(QtGui.QPalette.Mid)
        text_color = self.palette().color(QtGui.QPalette.ButtonText)
        selected_border = QtGui.QColor("#EF5350")  # red highlight for selected

        for i, name in enumerate(self.items):
            rect = self._get_item_rect(i)
            
            # Card background and border
            is_selected = (self.selected_item_idx == i)
            if is_selected:
                painter.setPen(QtGui.QPen(selected_border, 2))
                painter.setBrush(QtGui.QColor(selected_border.red(), selected_border.green(), selected_border.blue(), 40))
            else:
                painter.setPen(QtGui.QPen(border_color, 1))
                painter.setBrush(card_bg)
            painter.drawRoundedRect(rect, 5, 5)

            # Card text
            painter.setPen(text_color)
            metrics = painter.fontMetrics()
            elided_name = metrics.elidedText(name, QtCore.Qt.ElideRight, self.card_width - 20)
            painter.drawText(rect, QtCore.Qt.AlignCenter, elided_name)

            # Dot
            dot_pos = self._get_dot_pos(i)
            is_hovered = (self.hovered_dot_idx == i)
            
            painter.setPen(QtGui.QPen(QtGui.QColor("#FF9800"), 2))
            if is_hovered or self.drag_start_idx == i:
                painter.setBrush(QtGui.QColor("#FFb74d"))
            else:
                painter.setBrush(bg_color)
            painter.drawEllipse(dot_pos, self.dot_radius, self.dot_radius)

    def mousePressEvent(self, event):
        if event.button() == QtCore.Qt.LeftButton:
            dot_idx = self._get_dot_under_mouse(event.pos())
            if dot_idx is not None:
                # Start a link drag from the dot
                self.drag_start_idx = dot_idx
                self.drag_current_pos = event.pos()
                self.update()
            else:
                # Check if clicking on a card body — select it
                card_clicked = None
                for i in range(len(self.items)):
                    if self._get_item_rect(i).contains(event.pos()):
                        card_clicked = i
                        break
                self.selected_item_idx = card_clicked
                self.setFocus()
                self.update()

    def mouseMoveEvent(self, event):
        self.hovered_dot_idx = self._get_dot_under_mouse(event.pos())
        if self.drag_start_idx is not None:
            self.drag_current_pos = event.pos()
        self.update()

    def leaveEvent(self, event):
        self.hovered_dot_idx = None
        self.update()

    def mouseReleaseEvent(self, event):
        if event.button() == QtCore.Qt.LeftButton and self.drag_start_idx is not None:
            end_idx = self._get_dot_under_mouse(event.pos())
            if end_idx is not None and end_idx != self.drag_start_idx:
                i1, i2 = min(self.drag_start_idx, end_idx), max(self.drag_start_idx, end_idx)
                if (i1, i2) in self.links:
                    self.links.remove((i1, i2))
                else:
                    self.links.add((i1, i2))
                self.changed.emit()
            self.drag_start_idx = None
            self.drag_current_pos = None
            self.hovered_dot_idx = self._get_dot_under_mouse(event.pos())
            self.update()

    def mouseDoubleClickEvent(self, event):
        for i in range(len(self.items)):
            rect = self._get_item_rect(i)
            if rect.contains(event.pos()):
                self.remove_item(self.items[i])
                break

    def keyPressEvent(self, event):
        if event.key() in (QtCore.Qt.Key_Delete, QtCore.Qt.Key_Backspace):
            if self.selected_item_idx is not None and 0 <= self.selected_item_idx < len(self.items):
                self.remove_item(self.items[self.selected_item_idx])
        else:
            super().keyPressEvent(event)

    def get_groups(self):
        n = len(self.items)
        parent = list(range(n))
        
        def find(i):
            path = []
            while parent[i] != i:
                path.append(i)
                i = parent[i]
            for node in path:
                parent[node] = i
            return i
            
        def union(i, j):
            root_i = find(i)
            root_j = find(j)
            if root_i != root_j:
                parent[root_i] = root_j
                
        for i1, i2 in self.links:
            union(i1, i2)
            
        groups = {}
        for i in range(n):
            root = find(i)
            if root not in groups:
                groups[root] = []
            groups[root].append(self.items[i])
            
        return list(groups.values())


class ModuleBinderWidget(QtWidgets.QWidget):
    changed = QtCore.pyqtSignal()

    def __init__(self, available_items, label_left="Available", label_right="Optional (Drag dots to group, double-click card to remove)", parent=None):
        super().__init__(parent)
        layout = QtWidgets.QHBoxLayout(self)
        layout.setContentsMargins(0, 0, 0, 0)

        # Left list
        left_col = QtWidgets.QVBoxLayout()
        left_col.addWidget(QtWidgets.QLabel(label_left))
        self.list_available = QtWidgets.QListWidget()
        self.list_available.setSelectionMode(QtWidgets.QAbstractItemView.ExtendedSelection)
        for name in available_items:
            self.list_available.addItem(name)
        # Double-click helper
        self.list_available.itemDoubleClicked.connect(lambda item: self.canvas.add_item(item.text()))
        left_col.addWidget(self.list_available)
        layout.addLayout(left_col)

        # Buttons
        btn_col = QtWidgets.QVBoxLayout()
        btn_col.addStretch()
        btn_add = QtWidgets.QPushButton("→")
        btn_add.setFixedWidth(40)
        btn_add.setToolTip("Add selected module to optional list")
        btn_add.clicked.connect(self._add)
        btn_col.addWidget(btn_add)
        btn_rem = QtWidgets.QPushButton("←")
        btn_rem.setFixedWidth(40)
        btn_rem.setToolTip("Remove selected module (or use Delete key)")
        btn_rem.clicked.connect(self._remove_selected)
        btn_col.addWidget(btn_rem)
        btn_col.addStretch()
        layout.addLayout(btn_col)

        # Right canvas inside scroll area
        right_col = QtWidgets.QVBoxLayout()
        right_col.addWidget(QtWidgets.QLabel(label_right))
        
        self.scroll_area = QtWidgets.QScrollArea()
        self.scroll_area.setWidgetResizable(True)
        self.canvas = ModuleBinderCanvas()
        self.canvas.changed.connect(self.changed.emit)
        self.scroll_area.setWidget(self.canvas)
        
        right_col.addWidget(self.scroll_area)
        layout.addLayout(right_col)

    def _add(self):
        for item in self.list_available.selectedItems():
            self.canvas.add_item(item.text())

    def _remove_selected(self):
        idx = self.canvas.selected_item_idx
        if idx is not None and 0 <= idx < len(self.canvas.items):
            self.canvas.remove_item(self.canvas.items[idx])

    def get_selected_groups(self):
        return self.canvas.get_groups()

    def set_state(self, items, links):
        self.canvas.set_state(items, links)


# ---------------------------------------------------------------------------
# Config Dialog
# ---------------------------------------------------------------------------
class TestSimulationConfigDialog(QtWidgets.QDialog):
    def __init__(self, start_year=-43000, end_year=-38000, save_interval=100, store_dead_agents=False, store_grid_data=True, parent=None):
        super().__init__(parent)
        self.setWindowTitle("Test Simulation Configuration")
        self.resize(750, 800)
        self.result_configs = None

        # Get saved state from parent if available
        test_sim = {}
        if parent and hasattr(parent, 'test_sim_state') and parent.test_sim_state:
            test_sim = parent.test_sim_state

        main_layout = QtWidgets.QVBoxLayout(self)

        # --- Output folder ---
        folder_layout = QtWidgets.QHBoxLayout()
        folder_layout.addWidget(QtWidgets.QLabel("Output Folder:"))
        self.le_output_folder = QtWidgets.QLineEdit(test_sim.get("output_folder", "output/test_suite"))
        folder_layout.addWidget(self.le_output_folder)
        btn_browse = QtWidgets.QPushButton("Browse...")
        btn_browse.setFixedWidth(80)
        btn_browse.clicked.connect(self._browse_folder)
        folder_layout.addWidget(btn_browse)
        main_layout.addLayout(folder_layout)

        # --- Simulation Time Scale ---
        time_layout = QtWidgets.QHBoxLayout()
        
        time_layout.addWidget(QtWidgets.QLabel("Start Time (Years):"))
        self.spin_start_year = QtWidgets.QSpinBox()
        self.spin_start_year.setRange(-100000, 100000)
        self.spin_start_year.setValue(test_sim.get("start_year", start_year))
        time_layout.addWidget(self.spin_start_year)
        
        time_layout.addWidget(QtWidgets.QLabel("End Time (Years):"))
        self.spin_end_year = QtWidgets.QSpinBox()
        self.spin_end_year.setRange(-100000, 100000)
        self.spin_end_year.setValue(test_sim.get("end_year", end_year))
        time_layout.addWidget(self.spin_end_year)
        
        time_layout.addWidget(QtWidgets.QLabel("Save Interval (Ticks):"))
        self.spin_save_interval = QtWidgets.QSpinBox()
        self.spin_save_interval.setRange(0, 9999999)
        self.spin_save_interval.setValue(test_sim.get("save_interval", save_interval))
        time_layout.addWidget(self.spin_save_interval)

        time_layout.addWidget(QtWidgets.QLabel("IPC Interval (Ticks):"))
        self.spin_ipc_interval = QtWidgets.QSpinBox()
        self.spin_ipc_interval.setRange(1, 10000)
        self.spin_ipc_interval.setValue(test_sim.get("ipc_interval", 10))
        time_layout.addWidget(self.spin_ipc_interval)

        main_layout.addLayout(time_layout)

        # --- Repetitions & Data Export Options ---
        rep_layout = QtWidgets.QHBoxLayout()
        rep_layout.addWidget(QtWidgets.QLabel("Repetitions per configuration (nsi):"))
        self.spin_nsi = QtWidgets.QSpinBox()
        self.spin_nsi.setRange(1, 1000)
        self.spin_nsi.setValue(test_sim.get("nsi", 1))
        self.spin_nsi.valueChanged.connect(self._update_summary)
        rep_layout.addWidget(self.spin_nsi)
        
        rep_layout.addSpacing(30)
        self.chk_store_grid_data = QtWidgets.QCheckBox("Store Grid Data (NetCDF)")
        self.chk_store_grid_data.setChecked(test_sim.get("store_grid_data", store_grid_data))
        rep_layout.addWidget(self.chk_store_grid_data)
        
        self.chk_store_dead_agents = QtWidgets.QCheckBox("Store Dead Agents")
        self.chk_store_dead_agents.setChecked(test_sim.get("store_dead_agents", store_dead_agents))
        rep_layout.addWidget(self.chk_store_dead_agents)
        
        rep_layout.addWidget(QtWidgets.QLabel("Dead Export (Ticks):"))
        self.spin_dead_export_interval = QtWidgets.QSpinBox()
        self.spin_dead_export_interval.setRange(1, 10000)
        self.spin_dead_export_interval.setValue(test_sim.get("dead_export_interval", 500))
        rep_layout.addWidget(self.spin_dead_export_interval)

        rep_layout.addWidget(QtWidgets.QLabel("Min Dead to Export:"))
        self.spin_dead_export_threshold = QtWidgets.QSpinBox()
        self.spin_dead_export_threshold.setRange(1, 100000)
        self.spin_dead_export_threshold.setValue(test_sim.get("dead_export_threshold", 1000))
        rep_layout.addWidget(self.spin_dead_export_threshold)

        rep_layout.addStretch()
        main_layout.addLayout(rep_layout)

        # --- Always-on modules ---
        main_layout.addWidget(QtWidgets.QLabel("<b>Always-On Modules</b>"))
        all_names = [m["name"] for m in MODULE_REGISTRY]
        self.always_on = DualListWidget(all_names, "Available", "Always On")
        self.always_on.setFixedHeight(160)
        self.always_on.changed.connect(self._update_summary)
        main_layout.addWidget(self.always_on)

        # --- Optional modules ---
        main_layout.addWidget(QtWidgets.QLabel("<b>Optional Modules</b> (Drag dots to link/group, double-click card to remove)"))
        self.optional = ModuleBinderWidget(all_names, "Available", "Optional (Drag dots to group)")
        self.optional.setFixedHeight(220)
        self.optional.changed.connect(self._update_summary)
        main_layout.addWidget(self.optional)

        # --- Variable config vars ---
        main_layout.addWidget(QtWidgets.QLabel("<b>Variable Config Parameters</b>"))
        self.tbl_vars = QtWidgets.QTableWidget(0, 2)
        self.tbl_vars.setHorizontalHeaderLabels(["Variable Name", "Values (comma-separated)"])
        self.tbl_vars.horizontalHeader().setStretchLastSection(True)
        self.tbl_vars.setMinimumHeight(120)
        self.tbl_vars.itemChanged.connect(self._update_summary)
        main_layout.addWidget(self.tbl_vars)

        var_btn_layout = QtWidgets.QHBoxLayout()
        btn_add_var = QtWidgets.QPushButton("+ Add Variable")
        btn_add_var.clicked.connect(self._add_variable_row)
        var_btn_layout.addWidget(btn_add_var)
        btn_rem_var = QtWidgets.QPushButton("- Remove Selected")
        btn_rem_var.clicked.connect(self._remove_variable_row)
        var_btn_layout.addWidget(btn_rem_var)
        var_btn_layout.addStretch()
        main_layout.addLayout(var_btn_layout)

        # --- Summary ---
        self.lbl_summary = QtWidgets.QLabel("")
        self.lbl_summary.setStyleSheet("font-size: 13px; font-weight: bold; color: #00CCAA; padding: 6px;")
        main_layout.addWidget(self.lbl_summary)

        # Restore module configurations and variable config table
        if "always_on" in test_sim:
            self.always_on.set_selected_names(test_sim["always_on"])
        if "optional" in test_sim:
            self.optional.set_state(test_sim["optional"], test_sim.get("optional_links", []))
        if "variables" in test_sim:
            for name, vals_str in test_sim["variables"]:
                self._add_variable_row_with_values(name, vals_str)

        # --- Buttons ---
        btn_layout = QtWidgets.QHBoxLayout()
        btn_ok = QtWidgets.QPushButton("Run Test Suite")
        btn_ok.setStyleSheet("background-color: #FF9800; color: white; font-weight: bold; height: 35px;")
        btn_ok.clicked.connect(self._accept)
        btn_layout.addWidget(btn_ok)
        btn_cancel = QtWidgets.QPushButton("Cancel")
        btn_cancel.clicked.connect(self.reject)
        btn_layout.addWidget(btn_cancel)
        main_layout.addLayout(btn_layout)

        self._update_summary()

    def _browse_folder(self):
        folder = QtWidgets.QFileDialog.getExistingDirectory(self, "Select Output Folder")
        if folder:
            self.le_output_folder.setText(folder)

    def _add_variable_row(self):
        row = self.tbl_vars.rowCount()
        self.tbl_vars.insertRow(row)
        combo = QtWidgets.QComboBox()
        combo.setEditable(True)
        for v in SUGGESTED_CONFIG_VARS:
            combo.addItem(v)
        combo.currentTextChanged.connect(self._update_summary)
        self.tbl_vars.setCellWidget(row, 0, combo)
        self.tbl_vars.setItem(row, 1, QtWidgets.QTableWidgetItem(""))
        self._update_summary()

    def _remove_variable_row(self):
        rows = set(idx.row() for idx in self.tbl_vars.selectedIndexes())
        for row in sorted(rows, reverse=True):
            self.tbl_vars.removeRow(row)
        self._update_summary()

    def _add_variable_row_with_values(self, name, value_str):
        row = self.tbl_vars.rowCount()
        self.tbl_vars.insertRow(row)
        combo = QtWidgets.QComboBox()
        combo.setEditable(True)
        for v in SUGGESTED_CONFIG_VARS:
            combo.addItem(v)
        idx = combo.findText(name)
        if idx >= 0:
            combo.setCurrentIndex(idx)
        else:
            combo.setEditText(name)
        combo.currentTextChanged.connect(self._update_summary)
        self.tbl_vars.setCellWidget(row, 0, combo)
        self.tbl_vars.setItem(row, 1, QtWidgets.QTableWidgetItem(value_str))

    def get_state(self):
        variables = []
        for row in range(self.tbl_vars.rowCount()):
            combo = self.tbl_vars.cellWidget(row, 0)
            name = combo.currentText().strip() if combo else ""
            val_item = self.tbl_vars.item(row, 1)
            val_text = val_item.text().strip() if val_item else ""
            if name or val_text:
                variables.append([name, val_text])

        return {
            "output_folder": self.le_output_folder.text(),
            "start_year": self.spin_start_year.value(),
            "end_year": self.spin_end_year.value(),
            "save_interval": self.spin_save_interval.value(),
            "ipc_interval": self.spin_ipc_interval.value(),
            "dead_export_interval": self.spin_dead_export_interval.value(),
            "dead_export_threshold": self.spin_dead_export_threshold.value(),
            "nsi": self.spin_nsi.value(),
            "store_grid_data": self.chk_store_grid_data.isChecked(),
            "store_dead_agents": self.chk_store_dead_agents.isChecked(),
            "always_on": self.always_on.get_selected_names(),
            "optional": self.optional.canvas.get_items(),
            "optional_links": [list(link) for link in self.optional.canvas.links],
            "variables": variables
        }

    def _get_variable_configs(self):
        """Returns list of (var_name, [values])."""
        result = []
        for row in range(self.tbl_vars.rowCount()):
            combo = self.tbl_vars.cellWidget(row, 0)
            name = combo.currentText().strip() if combo else ""
            val_item = self.tbl_vars.item(row, 1)
            val_text = val_item.text().strip() if val_item else ""
            if name and val_text:
                try:
                    vals = [v.strip() for v in val_text.split(",") if v.strip()]
                    result.append((name, vals))
                except Exception:
                    pass
        return result

    def _update_summary(self):
        nsi = self.spin_nsi.value()
        groups = self.optional.get_selected_groups()
        nom = 1 + len(groups)
        var_configs = self._get_variable_configs()
        var_product = 1
        var_parts = []
        for name, vals in var_configs:
            var_product *= len(vals)
            var_parts.append(f"|{name}|={len(vals)}")
        total = nsi * nom * var_product
        var_str = " × ".join(var_parts) if var_parts else "1"
        self.lbl_summary.setText(
            f"Total simulations: {nsi} (reps) × {nom} (module variants) × {var_str} = {total}"
        )

    def _accept(self):
        output_folder = self.le_output_folder.text().strip()
        if not output_folder:
            QtWidgets.QMessageBox.warning(self, "Warning", "Please specify an output folder.")
            return

        start_year = self.spin_start_year.value()
        end_year = self.spin_end_year.value()
        save_interval = self.spin_save_interval.value()
        if end_year <= start_year:
            QtWidgets.QMessageBox.warning(self, "Warning", "End time must be greater than start time.")
            return

        always_names = self.always_on.get_selected_names()
        groups = self.optional.get_selected_groups()
        nsi = self.spin_nsi.value()
        var_configs = self._get_variable_configs()

        always_ids = [_MOD_NAME_TO_ID[n] for n in always_names if n in _MOD_NAME_TO_ID]

        # Build module variants: always include the baseline (no optional modules)
        module_variants = [("baseline", always_ids)]
        if groups:
            for grp in groups:
                grp_ids = [_MOD_NAME_TO_ID[n] for n in grp if n in _MOD_NAME_TO_ID]
                label = "+".join(grp)
                module_variants.append((label, always_ids + grp_ids))

        # Build variable value combinations
        if var_configs:
            var_names = [vc[0] for vc in var_configs]
            var_val_lists = [vc[1] for vc in var_configs]
            var_combos = list(itertools.product(*var_val_lists))
        else:
            var_names = []
            var_combos = [()]

        # Generate all run configs with descriptive file-safe labels
        configs = []
        run_idx = 1
        for mod_label, mod_ids in module_variants:
            # Create a short module tag: sanitize name for filesystem
            mod_ids_str = '-'.join(str(m) for m in sorted(mod_ids))
            for var_vals in var_combos:
                overrides = dict(zip(var_names, var_vals))
                # Build descriptive label
                var_tag = '_'.join(f"{k}-{v}" for k, v in overrides.items()) if overrides else 'default'
                for rep in range(1, nsi + 1):
                    # Human-readable label for the UI
                    label_parts = [f"mod={mod_label}"]
                    for k, v in overrides.items():
                        label_parts.append(f"{k}={v}")
                    label_parts.append(f"rep={rep}")
                    run_label = "_".join(label_parts)

                    # File-safe name encoding the full setup
                    file_tag = f"mods_{mod_ids_str}_vars_{var_tag}_rep_{rep}"

                    configs.append({
                        "run_idx": run_idx,
                        "run_label": run_label,
                        "file_tag": file_tag,
                        "modules": list(mod_ids),
                        "config_overrides": dict(overrides),
                    })
                    run_idx += 1

        if not configs:
            QtWidgets.QMessageBox.warning(self, "Warning", "No simulation runs configured.")
            return

        self.result_configs = configs
        self.output_folder = output_folder
        self.start_year = start_year
        self.end_year = end_year
        self.save_interval = save_interval
        self.ipc_interval = self.spin_ipc_interval.value()
        self.dead_export_interval = self.spin_dead_export_interval.value()
        self.dead_export_threshold = self.spin_dead_export_threshold.value()
        self.store_grid_data = self.chk_store_grid_data.isChecked()
        self.store_dead_agents = self.chk_store_dead_agents.isChecked()

        # Save state back to parent
        if self.parent() and hasattr(self.parent(), 'test_sim_state'):
            self.parent().test_sim_state = self.get_state()

        self.accept()


# ---------------------------------------------------------------------------
# Suite Window (runs all simulations)
# ---------------------------------------------------------------------------
class TestSimulationSuiteWindow(QtWidgets.QMainWindow):
    def __init__(self, run_configs, start_year, end_year, save_interval,
                 output_folder, store_dead_agents, store_grid_data,
                 config_path, hep_paths, spawn_points, age_dist,
                 clustering_alg, kmeans_k, dbscan_eps, dbscan_minpts, current_npops,
                 ipc_interval=10, dead_export_interval=500, dead_export_threshold=1000):
        super().__init__()
        self.setWindowTitle("Test Simulation Suite")
        self.resize(1100, 600)

        self.run_configs = run_configs
        self.num_sims = len(run_configs)
        self.start_year = start_year
        self.end_year = end_year
        self.save_interval = save_interval
        self.ipc_interval = ipc_interval
        self.dead_export_interval = dead_export_interval
        self.dead_export_threshold = dead_export_threshold
        self.output_folder = output_folder
        self.store_dead_agents = store_dead_agents
        self.store_grid_data = store_grid_data
        self.config_path = config_path
        self.hep_paths = hep_paths
        self.spawn_points = spawn_points
        self.age_dist = age_dist
        self.clustering_alg = clustering_alg
        self.kmeans_k = kmeans_k
        self.dbscan_eps = dbscan_eps
        self.dbscan_minpts = dbscan_minpts
        self.current_npops = current_npops

        # Ensure output folder exists
        os.makedirs(self.output_folder, exist_ok=True)

        # Write metadata.json to output folder for complete reconstruction
        try:
            def make_serializable(obj):
                if isinstance(obj, dict):
                    return {str(k): make_serializable(v) for k, v in obj.items()}
                elif isinstance(obj, (list, tuple)):
                    return [make_serializable(x) for x in obj]
                elif hasattr(obj, 'tolist'):
                    return obj.tolist()
                elif isinstance(obj, (np.integer, np.floating)):
                    return obj.item()
                return obj

            metadata = {
                "start_year": self.start_year,
                "end_year": self.end_year,
                "save_interval": self.save_interval,
                "config_path": self.config_path,
                "hep_paths": self.hep_paths,
                "spawn_points": make_serializable(self.spawn_points),
                "age_distribution": make_serializable(self.age_dist),
                "clustering_algorithm_id": self.clustering_alg,
                "kmeans_k": self.kmeans_k,
                "dbscan_eps": self.dbscan_eps,
                "dbscan_minpts": self.dbscan_minpts,
                "store_dead_agents": self.store_dead_agents,
                "store_grid_data": self.store_grid_data,
                "runs": [
                    {
                        "run_idx": rc["run_idx"],
                        "run_label": rc["run_label"],
                        "file_tag": rc["file_tag"],
                        "modules": rc["modules"],
                        "config_overrides": rc["config_overrides"]
                    }
                    for rc in self.run_configs
                ]
            }
            with open(os.path.join(self.output_folder, "metadata.json"), "w") as f:
                json.dump(metadata, f, indent=4)
        except Exception as e:
            print(f"Warning: Failed to write metadata.json: {e}")

        # Temp directory for patched configs
        self.tmp_dir = tempfile.mkdtemp(prefix="hep_test_sim_")

        # Scheduler state
        self.max_parallel = max(1, os.cpu_count() - 1)
        self.active_processes = {}
        self.run_statuses = {rc["run_idx"]: "queued" for rc in run_configs}
        self.last_update_time = {rc["run_idx"]: time.time() for rc in run_configs}
        self.last_update_tick = {rc["run_idx"]: 0 for rc in run_configs}
        self.run_start_time = {rc["run_idx"]: time.time() for rc in run_configs}
        self.last_child_elapsed = {rc["run_idx"]: 0.0 for rc in run_configs}
        self.run_log_paths = {}  # run_idx -> log file path for crash diagnostics

        # --- UI ---
        self.central_widget = QtWidgets.QWidget()
        self.setCentralWidget(self.central_widget)
        self.main_layout = QtWidgets.QVBoxLayout(self.central_widget)

        self.lbl_title = QtWidgets.QLabel(
            f"Test Suite: {self.num_sims} runs (max {self.max_parallel} parallel)"
        )
        self.lbl_title.setStyleSheet("font-size: 14px; font-weight: bold; color: #FF9800; margin-bottom: 5px;")
        self.main_layout.addWidget(self.lbl_title)

        # Horizontal Splitter to divide left runs list and right performance plot
        self.splitter = QtWidgets.QSplitter(QtCore.Qt.Horizontal)

        # Scroll area on the left
        self.scroll_area = QtWidgets.QScrollArea()
        self.scroll_area.setWidgetResizable(True)
        self.scroll_widget = QtWidgets.QWidget()
        self.scroll_layout = QtWidgets.QVBoxLayout(self.scroll_widget)
        self.scroll_layout.setSpacing(6)

        self.progress_bars = {}
        self.status_labels = {}

        # Create tab widget on the right
        self.right_tabs = QtWidgets.QTabWidget()
        self.right_tabs.setStyleSheet(
            "QTabWidget::pane { border: 1px solid #444; background: #111c1c; } "
            "QTabBar::tab { background: #223333; color: #889999; padding: 8px 12px; border-top-left-radius: 4px; border-top-right-radius: 4px; } "
            "QTabBar::tab:selected { background: #111c1c; color: #FF9800; font-weight: bold; border: 1px solid #444; border-bottom-color: #111c1c; }"
        )
        
        # Plot widget on the right (using GraphicsLayoutWidget for multiple vertically-stacked subplots)
        self.plot_widget = pg.GraphicsLayoutWidget()
        self.plot_widget.setBackground('#111c1c') # Dark theme matching application
        
        # Subplot 1: Timing Components (ms/tick)
        self.p1 = self.plot_widget.addPlot(row=0, col=0)
        self.p1.setLabel('left', "Time", units='ms/tick', color='#00CCAA')
        self.p1.showGrid(x=True, y=True, alpha=0.3)
        self.p1.addLegend()

        # Subplot 2: Population & Disk Writes (Count)
        self.p2 = self.plot_widget.addPlot(row=1, col=0)
        self.p2.setLabel('left', "Count", color='#00CCAA')
        self.p2.showGrid(x=True, y=True, alpha=0.3)
        self.p2.addLegend()
        self.p2.setXLink(self.p1) # Synchronize zooming and panning with p1

        # Subplot 3: Storage & Elapsed Time (MB & seconds)
        self.p3 = self.plot_widget.addPlot(row=2, col=0)
        self.p3.setLabel('left', "Value", color='#00CCAA')
        self.p3.setLabel('bottom', "Time", units='ticks', color='#00CCAA')
        self.p3.showGrid(x=True, y=True, alpha=0.3)
        self.p3.addLegend()
        self.p3.setXLink(self.p1) # Synchronize zooming and panning with p1

        self.right_tabs.addTab(self.plot_widget, "Performance Plot")

        # Config settings text box
        self.txt_config = QtWidgets.QPlainTextEdit()
        self.txt_config.setReadOnly(True)
        self.txt_config.setFont(QtGui.QFont("Courier New", 10))
        self.txt_config.setStyleSheet(
            "background-color: #112222; color: #00FFCC; border: none; font-family: monospace;"
        )
        self.right_tabs.addTab(self.txt_config, "Suite Configuration")

        config_text = []
        config_text.append("=========================================================")
        config_text.append("             SIMULATION SWEEP CONFIGURATION              ")
        config_text.append("=========================================================")
        config_text.append(f"Output Folder:          {self.output_folder}")
        config_text.append(f"Start Year:             {self.start_year}")
        config_text.append(f"End Year:               {self.end_year}")
        config_text.append(f"Total Ticks:            {abs(self.end_year - self.start_year) * 100}")
        config_text.append(f"Save Interval:          {self.save_interval} ticks")
        config_text.append(f"IPC Interval:           {self.ipc_interval} ticks")
        config_text.append(f"Dead Export (Ticks):    {self.dead_export_interval}")
        config_text.append(f"Min Dead to Export:     {self.dead_export_threshold}")
        config_text.append(f"Store Grid Data:        {self.store_grid_data}")
        config_text.append(f"Store Dead Agents:      {self.store_dead_agents}")
        config_text.append(f"Clustering Algorithm:   {self.clustering_alg}")
        if self.clustering_alg == 1:
            config_text.append("  - Type:               Watershed")
        elif self.clustering_alg == 2:
            config_text.append("  - Type:               K-Means")
            config_text.append(f"  - K:                  {self.kmeans_k}")
        elif self.clustering_alg == 3:
            config_text.append("  - Type:               DBSCAN")
            config_text.append(f"  - EPS:                {self.dbscan_eps}")
            config_text.append(f"  - MinPts:             {self.dbscan_minpts}")
        elif self.clustering_alg == 4:
            config_text.append("  - Type:               Auto K-Means")
        config_text.append(f"Number of Populations:  {self.current_npops}")
        config_text.append("=========================================================")
        config_text.append("                     SIMULATION RUNS                     ")
        config_text.append("=========================================================")
        
        for rc in run_configs:
            idx = rc["run_idx"]
            config_text.append(f"Run #{idx}:")
            config_text.append(f"  Label:       {rc['run_label']}")
            config_text.append(f"  File Tag:    {rc['file_tag']}")
            
            # Module names
            mods = rc.get("modules", [])
            mod_names = []
            for m_id in mods:
                name = next((m["name"] for m in MODULE_REGISTRY if m["id"] == m_id), f"Module {m_id}")
                mod_names.append(name)
            config_text.append(f"  Modules:     {', '.join(mod_names) if mod_names else 'None'}")
            
            # Overrides
            ovr = rc.get("config_overrides", {})
            if ovr:
                config_text.append("  Overrides:")
                for k, v in ovr.items():
                    config_text.append(f"    {k}: {v}")
            else:
                config_text.append("  Overrides:   None")
            config_text.append("---------------------------------------------------------")
            
        self.txt_config.setPlainText("\n".join(config_text))

        # Graph Data Initialization
        self.plot_x_data = {rc["run_idx"]: [] for rc in run_configs}
        self.plot_timings_fortran = {rc["run_idx"]: [] for rc in run_configs}
        self.plot_timings_grid = {rc["run_idx"]: [] for rc in run_configs}
        self.plot_timings_dead = {rc["run_idx"]: [] for rc in run_configs}
        self.plot_timings_overhead = {rc["run_idx"]: [] for rc in run_configs}
        self.plot_agents_alive = {rc["run_idx"]: [] for rc in run_configs}
        self.plot_agents_dead = {rc["run_idx"]: [] for rc in run_configs}
        self.plot_grid_mb = {rc["run_idx"]: [] for rc in run_configs}
        self.plot_python_used = {rc["run_idx"]: [] for rc in run_configs}

        # Instantiate a single set of curves on the subplots
        self.curve_fortran = self.p1.plot(pen=pg.mkPen('#FF9800', width=2), name="Fortran Solver")
        self.curve_grid_time = self.p1.plot(pen=pg.mkPen('#00CCAA', width=2), name="Grid Write")
        self.curve_dead_time = self.p1.plot(pen=pg.mkPen('#FF5722', width=2), name="Dead Write")
        self.curve_overhead = self.p1.plot(pen=pg.mkPen('#E91E63', width=2), name="Python Overhead")
        
        self.curve_alive = self.p2.plot(pen=pg.mkPen('#4CAF50', width=2), name="Alive Agents")
        self.curve_dead_count = self.p2.plot(pen=pg.mkPen('#9C27B0', width=2), name="Dead Exported")
        
        self.curve_grid_mb = self.p3.plot(pen=pg.mkPen('#00BCD4', width=2), name="Grid Written (MB)")
        self.curve_python_used = self.p3.plot(pen=pg.mkPen('#3F51B5', width=2), name="Python Used Time (s)")

        # Set initial titles
        initial_label = run_configs[0]["run_label"]
        self.p1.setTitle(f"Timing Components: {initial_label}", color='#FF9800', size='10pt')
        self.p2.setTitle(f"Population & Disk Writes: {initial_label}", color='#FF9800', size='10pt')
        self.p3.setTitle(f"Storage & Python Used Time: {initial_label}", color='#FF9800', size='10pt')

        for i, rc in enumerate(run_configs):
            idx = rc["run_idx"]
            group_box = QtWidgets.QGroupBox(f"#{idx}: {rc['run_label']}")
            group_box.setStyleSheet(
                "QGroupBox { font-weight: bold; border: 1px solid #444; border-radius: 4px; "
                "margin-top: 8px; padding: 8px; } "
                "QGroupBox::title { subcontrol-origin: margin; left: 10px; padding: 0 3px; color: #FF9800; }"
            )
            gl = QtWidgets.QVBoxLayout(group_box)

            # Selection row
            sel_layout = QtWidgets.QHBoxLayout()
            rb_select = QtWidgets.QRadioButton("Active Plot Display")
            rb_select.setStyleSheet("font-size: 10px; color: #00FFCC; font-weight: normal;")
            if i == 0:
                rb_select.setChecked(True)
                self.active_plot_run_idx = idx
            rb_select.toggled.connect(lambda checked, r_idx=idx: self._switch_active_plot(r_idx) if checked else None)
            sel_layout.addWidget(rb_select)
            sel_layout.addStretch()
            gl.addLayout(sel_layout)

            pbar = QtWidgets.QProgressBar()
            pbar.setValue(0)
            pbar.setStyleSheet(
                "QProgressBar { border: 1px solid #334444; border-radius: 3px; text-align: center; "
                "color: white; background-color: #112222; } "
                "QProgressBar::chunk { background-color: #FF9800; }"
            )
            gl.addWidget(pbar)
            self.progress_bars[idx] = pbar

            slbl = QtWidgets.QLabel("Status: Queued")
            slbl.setStyleSheet("font-size: 10px; color: #889999;")
            slbl.setTextInteractionFlags(QtCore.Qt.TextSelectableByMouse)
            gl.addWidget(slbl)
            self.status_labels[idx] = slbl
            self.scroll_layout.addWidget(group_box)

        self.scroll_layout.addStretch()
        self.scroll_area.setWidget(self.scroll_widget)

        self.splitter.addWidget(self.scroll_area)
        self.splitter.addWidget(self.right_tabs)
        self.splitter.setSizes([450, 650])
        self.main_layout.addWidget(self.splitter)

        # Buttons
        btn_layout = QtWidgets.QHBoxLayout()
        self.btn_abort = QtWidgets.QPushButton("Abort All")
        self.btn_abort.setStyleSheet(
            "background-color: #D32F2F; color: white; font-weight: bold; height: 35px; border-radius: 4px;"
        )
        self.btn_abort.clicked.connect(self.abort_all)
        btn_layout.addWidget(self.btn_abort)

        self.btn_close = QtWidgets.QPushButton("Close Window")
        self.btn_close.setEnabled(False)
        self.btn_close.setStyleSheet(
            "background-color: #37474F; color: white; font-weight: bold; height: 35px; border-radius: 4px;"
        )
        self.btn_close.clicked.connect(self.close)
        btn_layout.addWidget(self.btn_close)
        self.main_layout.addLayout(btn_layout)

        # Shared queue
        self.progress_queue = multiprocessing.Queue()

        # Timer
        self.timer = QtCore.QTimer()
        self.timer.timeout.connect(self._update_loop)
        self.timer.start(100)

    def _switch_active_plot(self, run_idx):
        self.active_plot_run_idx = run_idx
        label = next((rc["run_label"] for rc in self.run_configs if rc["run_idx"] == run_idx), f"Run #{run_idx}")
        self.p1.setTitle(f"Timing Components: {label}", color='#FF9800', size='10pt')
        self.p2.setTitle(f"Population & Disk Writes: {label}", color='#FF9800', size='10pt')
        self.p3.setTitle(f"Storage & Python Used Time: {label}", color='#FF9800', size='10pt')
        self._refresh_plot_curves(run_idx)

    def _refresh_plot_curves(self, run_idx):
        x = self.plot_x_data.get(run_idx, [])
        self.curve_fortran.setData(x, self.plot_timings_fortran.get(run_idx, []))
        self.curve_grid_time.setData(x, self.plot_timings_grid.get(run_idx, []))
        self.curve_dead_time.setData(x, self.plot_timings_dead.get(run_idx, []))
        self.curve_overhead.setData(x, self.plot_timings_overhead.get(run_idx, []))
        self.curve_alive.setData(x, self.plot_agents_alive.get(run_idx, []))
        self.curve_dead_count.setData(x, self.plot_agents_dead.get(run_idx, []))
        self.curve_grid_mb.setData(x, self.plot_grid_mb.get(run_idx, []))
        self.curve_python_used.setData(x, self.plot_python_used.get(run_idx, []))

    # ---- Scheduler ----

    def _update_loop(self):
        import queue

        # Display queue size (Linux support)
        try:
            q_len = self.progress_queue.qsize()
        except Exception:
            q_len = -1
        q_str = f" | Queue Backlog: {q_len} messages" if q_len >= 0 else ""
        self.lbl_title.setText(f"Test Suite: {self.num_sims} runs (max {self.max_parallel} parallel){q_str}")

        latest_progress = {} # maps run_idx -> tuple of args
        finished_runs = []
        error_runs = {}

        # Drain queue
        while True:
            try:
                run_idx, ptype, *args = self.progress_queue.get_nowait()
                if ptype == "progress":
                    latest_progress[run_idx] = args
                elif ptype == "finished":
                    finished_runs.append(run_idx)
                elif ptype == "error":
                    error_runs[run_idx] = args[0]
            except queue.Empty:
                break
            except Exception as e:
                print(f"Error in test suite update loop: {e}")
                break

        # Process aggregated progress updates
        now = time.time()
        for run_idx, args in latest_progress.items():
            progress, t, count, elapsed = args[:4]
            ram_mb = args[4] if len(args) > 4 else 0.0

            fortran_ms = args[5] if len(args) > 5 else 0.0
            grid_ms = args[6] if len(args) > 6 else 0.0
            dead_ms = args[7] if len(args) > 7 else 0.0
            grid_mb = args[8] if len(args) > 8 else 0.0
            dead_written = args[9] if len(args) > 9 else 0.0
            python_used = args[10] if len(args) > 10 else 0.0

            # Calculate instantaneous parent-side rate
            prev_time = self.last_update_time.get(run_idx, now)
            prev_tick = self.last_update_tick.get(run_idx, 0)
            ticks_diff = t - prev_tick
            time_diff = now - prev_time
            inst_parent_ms_per_tick = (time_diff / ticks_diff) * 1000.0 if ticks_diff > 0 else 0.0

            # Update state
            self.last_update_time[run_idx] = now
            self.last_update_tick[run_idx] = t

            ram_str = f" | RAM: {ram_mb:.1f} MB" if ram_mb > 0 else ""
            
            self.status_labels[run_idx].setText(
                f"Running | Tick: {t} | Agents: {count} | "
                f"Fortran: {fortran_ms:.2f} ms/tick | "
                f"Grid: {grid_ms:.2f} ms/tick | "
                f"Dead: {dead_ms:.2f} ms/tick | "
                f"Python: {inst_parent_ms_per_tick:.2f} ms/tick{ram_str}"
            )
            self.progress_bars[run_idx].setValue(progress)

            if t > 0:
                self.plot_x_data[run_idx].append(t)
                self.plot_timings_fortran[run_idx].append(fortran_ms)
                self.plot_timings_grid[run_idx].append(grid_ms)
                self.plot_timings_dead[run_idx].append(dead_ms)
                
                # Calculate Python overhead: inst_parent_ms_per_tick - fortran_ms - grid_ms - dead_ms
                overhead = max(0.0, inst_parent_ms_per_tick - fortran_ms - grid_ms - dead_ms)
                self.plot_timings_overhead[run_idx].append(overhead)
                
                self.plot_agents_alive[run_idx].append(count)
                self.plot_agents_dead[run_idx].append(dead_written)
                self.plot_grid_mb[run_idx].append(grid_mb)
                self.plot_python_used[run_idx].append(python_used)

                # If this is the active plot, refresh the curves
                if run_idx == self.active_plot_run_idx:
                    self._refresh_plot_curves(run_idx)

        # Process finished runs
        for run_idx in finished_runs:
            self.progress_bars[run_idx].setValue(100)
            self.status_labels[run_idx].setText("Finished successfully.")
            self.run_statuses[run_idx] = "finished"
            self.active_processes.pop(run_idx, None)

        # Process errors
        for run_idx, err_msg in error_runs.items():
            self.progress_bars[run_idx].setValue(100)
            self.status_labels[run_idx].setText(f"Error: {err_msg[:200]}")
            self.run_statuses[run_idx] = "error"
            self.active_processes.pop(run_idx, None)
            show_selectable_error(self, f"Run #{run_idx} Error", err_msg)

        # Check for silent crashes (processes that terminated but did not send a finished/error message)
        for run_idx, p in list(self.active_processes.items()):
            if not p.is_alive():
                exit_code = p.exitcode
                if self.run_statuses.get(run_idx) == "running":
                    err_msg = f"Process terminated abnormally with exit code {exit_code}."
                    if exit_code == -11:
                        err_msg += " (Segmentation Fault)"
                    elif exit_code == -9:
                        err_msg += " (Killed / Out of Memory)"

                    # Read last 30 lines of the subprocess console log
                    log_path = self.run_log_paths.get(run_idx)
                    if log_path and os.path.exists(log_path):
                        try:
                            with open(log_path, "r", errors="replace") as lf:
                                all_lines = lf.readlines()
                            tail = "".join(all_lines[-30:])
                            err_msg += f"\n\n--- Last 30 lines of console output ---\n{tail}"
                        except Exception as e:
                            err_msg += f"\n(Could not read log file: {e})"
                    
                    self.progress_bars[run_idx].setValue(100)
                    self.status_labels[run_idx].setText(f"Crashed: {err_msg[:100]}")
                    self.run_statuses[run_idx] = "error"
                    self.active_processes.pop(run_idx, None)
                    show_selectable_error(self, f"Run #{run_idx} Crashed", err_msg)

        # Schedule
        if len(self.active_processes) < self.max_parallel:
            for rc in self.run_configs:
                idx = rc["run_idx"]
                if self.run_statuses[idx] == "queued":
                    self._launch_run(rc)
                    break

        # Check done
        if all(s in ("finished", "error", "aborted") for s in self.run_statuses.values()):
            self.timer.stop()
            self.btn_abort.setEnabled(False)
            self.btn_close.setEnabled(True)
            self.btn_close.setStyleSheet(
                "background-color: #00E676; color: black; font-weight: bold; height: 35px; border-radius: 4px;"
            )
            # Cleanup temp dir
            try:
                shutil.rmtree(self.tmp_dir, ignore_errors=True)
            except Exception:
                pass
            QtWidgets.QMessageBox.information(self, "Test Suite Complete", "All test simulation runs have completed.")

    def _launch_run(self, rc):
        idx = rc["run_idx"]
        self.run_statuses[idx] = "running"
        self.status_labels[idx].setText("Initializing...")
        self.last_update_time[idx] = time.time()
        self.last_update_tick[idx] = 0
        self.run_start_time[idx] = time.time()
        self.last_child_elapsed[idx] = 0.0

        # Generate temp config
        if rc["config_overrides"]:
            tmp_cfg = os.path.join(self.tmp_dir, f"config_run{idx}.nml")
            patch_namelist_file(self.config_path, rc["config_overrides"], tmp_cfg)
            cfg_path = tmp_cfg
        else:
            cfg_path = self.config_path

        # Output paths — file_tag encodes module IDs and config values
        tag = rc['file_tag']
        gif_path = os.path.join(self.output_folder, f"{tag}.gif")
        nc_path = os.path.join(self.output_folder, f"{tag}.nc")
        dead_nc_path = os.path.join(self.output_folder, f"{tag}_dead.nc")
        log_path = os.path.join(self.tmp_dir, f"run_{idx}_console.log")
        self.run_log_paths[idx] = log_path

        p = multiprocessing.Process(
            target=run_simulation_process,
            args=(
                idx, self.start_year, self.end_year, self.save_interval,
                cfg_path, self.hep_paths, rc["modules"],
                self.spawn_points, self.age_dist, self.clustering_alg,
                self.kmeans_k, self.dbscan_eps, self.dbscan_minpts, self.current_npops,
                self.store_dead_agents, gif_path,
                nc_path if self.store_grid_data else None,
                dead_nc_path, self.progress_queue,
                self.ipc_interval,
                self.dead_export_interval,
                self.dead_export_threshold,
                log_path
            )
        )
        p.start()
        self.active_processes[idx] = p

    def abort_all(self):
        self.timer.stop()
        self.btn_abort.setEnabled(False)
        for idx, p in list(self.active_processes.items()):
            if p.is_alive():
                p.terminate()
                p.join()
            self.run_statuses[idx] = "aborted"
            self.status_labels[idx].setText("Aborted.")
            self.progress_bars[idx].setValue(0)
        for rc in self.run_configs:
            idx = rc["run_idx"]
            if self.run_statuses[idx] == "queued":
                self.run_statuses[idx] = "aborted"
                self.status_labels[idx].setText("Queued run aborted.")
        self.active_processes.clear()
        self.btn_close.setEnabled(True)
        self.btn_close.setStyleSheet(
            "background-color: #00E676; color: black; font-weight: bold; height: 35px; border-radius: 4px;"
        )
        try:
            shutil.rmtree(self.tmp_dir, ignore_errors=True)
        except Exception:
            pass
        QtWidgets.QMessageBox.warning(self, "Aborted", "All test simulation runs were aborted.")

    def closeEvent(self, event):
        self.timer.stop()
        for idx, p in list(self.active_processes.items()):
            if p.is_alive():
                p.terminate()
                p.join()
        try:
            shutil.rmtree(self.tmp_dir, ignore_errors=True)
        except Exception:
            pass
        event.accept()
