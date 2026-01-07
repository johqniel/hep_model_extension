import sys
import os
import OpenGL

# Disable PyOpenGL error checking to avoid context retrieval errors
OpenGL.ERROR_CHECKING = False

import numpy as np
import pyqtgraph.opengl as gl
from pyqtgraph.Qt import QtCore, QtWidgets
import OpenGL.GL as GL

app = QtWidgets.QApplication(sys.argv)
w = gl.GLViewWidget()
w.opts['distance'] = 20
w.show()
w.setWindowTitle('pyqtgraph example: Patch Test')

g = gl.GLGridItem()
w.addItem(g)

# Simple scatter plot
pos = np.random.random(size=(100,3)) * 10
color = np.ones((100, 4))
color[:, 0] = 1.0 # Red
sp1 = gl.GLScatterPlotItem(pos=pos, color=color, size=10, pxMode=True)
w.addItem(sp1)

if __name__ == '__main__':
    sys.exit(app.exec_())
