import scipy as sc
from scipy import signal
import bstates as bs
import pickle
import numpy as np
from sys import argv

with open('../data/bsPat_1.pickle', 'rb') as f:
    Phases, syncConnAux = pickle.load(f)

end = int(argv[1])

syncConn = np.zeros([syncConnAux.shape[0],
                     syncConnAux.shape[1],
                     syncConnAux.shape[2],
                     end])

idx = 0

for i in range(end):

    with open('../data/bsPat_' + str(i + 1) + '.pickle', 'rb') as f:
        Phases, syncConnAux = pickle.load(f)
    syncConnAux = sc.signal.savgol_filter(
        syncConnAux[:, :, :, 0], 61, 3, axis=0)
    syncConn[:, :, :, idx] = syncConnAux
    idx = idx + 1
    print(idx)

print("Data loaded")

statesArray = bs.get_patstatesArray(syncConn)
del syncConn

for i in range(statesArray.shape[2]):

    if (i == 0):
        ensembleArray = statesArray[:, :, i]
    else:
        ensembleArray = np.vstack((ensembleArray, statesArray[:, :, i]))
    print(i)

with open('../data/ensembleArray_sg.pickle', 'wb') as f:
    pickle.dump(ensembleArray, f, protocol=4)

print("statesArray obtained (signal).")
