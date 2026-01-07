import sys
import numpy as np
import pyqtgraph.opengl as gl
from pyqtgraph.Qt import QtCore, QtWidgets

app = QtWidgets.QApplication(sys.argv)
w = gl.GLViewWidget()
w.opts['distance'] = 20
w.show()
w.setWindowTitle('pyqtgraph example: GLScatterPlotItem')

g = gl.GLGridItem()
w.addItem(g)

pos = np.random.random(size=(1000,3))
pos *= [10,10,10]
pos[0] = (0,0,0)
color = np.ones((pos.shape[0], 4))
d2 = (pos**2).sum(axis=1)**0.5
pos[:, 2] = d2
d2 /= 10
color[:, 0] = d2
color[:, 1] = d2
color[:, 2] = 1
sp1 = gl.GLScatterPlotItem(pos=pos, color=color, size=5, pxMode=True)
w.addItem(sp1)

def update():
    ## update volume colors
    global sp1, pos
    pos[:, 2] = np.random.random(size=pos.shape[0]) * 10
    sp1.setData(pos=pos)

t = QtCore.QTimer()
t.timeout.connect(update)
t.start(50)

if __name__ == '__main__':
    sys.exit(app.exec_())
