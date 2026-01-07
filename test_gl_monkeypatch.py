import sys
import OpenGL.contextdata
import OpenGL.error

# Monkeypatch getContext to avoid the error
_original_getContext = OpenGL.contextdata.getContext

def safe_getContext(context=None):
    try:
        return _original_getContext(context)
    except OpenGL.error.Error:
        # Return 0 as a fallback context ID
        return 0

OpenGL.contextdata.getContext = safe_getContext

import numpy as np
import pyqtgraph.opengl as gl
from pyqtgraph.Qt import QtCore, QtWidgets

app = QtWidgets.QApplication(sys.argv)
w = gl.GLViewWidget()
w.opts['distance'] = 20
w.show()
w.setWindowTitle('pyqtgraph example: Monkeypatch Test')

g = gl.GLGridItem()
w.addItem(g)

pos = np.random.random(size=(1000,3)) * 10
color = np.ones((1000, 4))
color[:, 0] = 1.0
sp1 = gl.GLScatterPlotItem(pos=pos, color=color, size=5, pxMode=True)
w.addItem(sp1)

if __name__ == '__main__':
    sys.exit(app.exec_())
