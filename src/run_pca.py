## IMPORT BASIC LIBS FOR SCRIPT

import scipy as sc
from scipy import io
from scipy import linalg
import bstates as bs
import pickle
import numpy as np
import feather
import h5py
import pandas as pd
from sklearn.decomposition import PCA
import os
from sys import argv

## TREATING ARGV VARS

nPats = int(argv[1]) # NUMBER OF SUBJECTS
pipe = argv[2] # PIPELINE
surr = int(argv[3])


## COMMAND TO ALLOW THE USE OF HDF5 IN NETWORK (HPC) STORAGE SYSTEMS

os.environ['HDF5_USE_FILE_LOCKING'] = 'FALSE'

################################################################
##################### LOAD SYNC MATRICES #######################
################################################################

with open('../data/bsPat_1.pickle', 'rb') as f:
    Phases, syncConnAux, leidaArrayAux = pickle.load(f)

leidaArray = np.zeros([leidaArrayAux.shape[0],
                       leidaArrayAux.shape[1],
                       nPats])

idx = 0

print("Loading pickle files - It's a bit of a pickle!")

for i in range(nPats):

    with open('../data/bsPat_' + str(i + 1) + '.pickle', 'rb') as f:
        Phases, syncConnAux, leidaArrayAux = pickle.load(f)
    leidaArrayAux = leidaArrayAux[:, :, 0]
    leidaArray[:, :, i] = leidaArrayAux
    idx = idx + 1
    print(idx)


################################################################
##################### MERGE SYNC MATRICES ######################
################################################################

for i in range(leidaArray.shape[2]):

    if (i == 0):
        ensembleArray = leidaArray[:, :, i]

    else:
        ensembleArray = np.vstack((ensembleArray, leidaArray[:, :, i]))


    print(i)

del leidaArray

print("ensembleArray obtained (signal).")

## CREATING HDF5 FILE FOR ENSEMBLE ARRAY

h5f = h5py.File('../data/' + pipe + '.hdf5', 'w')

## SAVING ENSEMBLE ARRAY ON HDF5 FILE

h5f.create_dataset('ensembleArray', data = ensembleArray)

## CLEANING MEMORY TO ALLOW FURTHER OPERATIONS

del ensembleArray

print("Created HDF5")
print("ensembleArray saved")
print("Load dataset + clinical data")

## LOADING MATLAB FILE AND BOLD SIGNAL + CLINICAL DETAILS

mat = sc.io.loadmat('../data/2020_07_MASTER_connectomes90_All_select.mat',
                    squeeze_me = True)

################################################################
#################### LOADING CLINICAL DATA #####################
################################################################


sub = mat['sub']  # SUBJECTS

ses = mat['ses']  # SCAN SESSION

tt = 2280  # <- NEEDS AUTOMATION - DURATION OF SIGNAL PER PATIENT
ttime = np.arange(tt)

print("Clinical data loaded")

################################################################
##################### CREATE TAG VECTORS #######################
################################################################

print("Replicating clinical data")

## TRANFERING DATA TO VARIABLE THAT COMPOSE FINAL DATAFRAME

patTags = np.repeat(sub, tt).transpose()
ses = np.repeat(ses, tt).transpose()
ttime = np.tile(ttime, nPats).transpose()

## CLEANING MEMORY TO ALLOW FURTHER OPERATIONS

del mat

################################################################
##################### SAVING CLINICAL DATA #####################
################################################################

print("Saving data to HDF5 file")

dt = h5py.string_dtype(encoding = 'utf-8')
h5f.create_dataset('patTags', data = patTags, dtype = dt)
h5f.create_dataset('ses', data = ses, dtype = dt)
h5f.create_dataset('ttime', data = ttime)

## CLEANING MEMORY TO ALLOW FURTHER OPERATIONS

del patTags, ses

h5f.close()

print("Read HDF5")

h5f = h5py.File('../data/' + pipe + '.hdf5', 'r')
data = h5f['ensembleArray']
#data_fit = h5f['ensembleArray_fit']

print("Read HDF5 finished")

pca = PCA(n_components=50)
pca.fit(data)

varExp = pca.explained_variance_ratio_

with open('../data/varExp_' + pipe + '.pickle', 'wb') as f:
    pickle.dump(varExp, f)

data_pca = pca.transform(data)

with open('../data/pca_out_' + pipe + '.pickle', 'wb') as f:
    pickle.dump(data_pca, f)

print("PCA finished")

h5f.close()

print("Read HDF5")

h5f = h5py.File('../data/' + pipe + '.hdf5', 'r')

print("Load clinical data")

patTags = h5f['patTags']
ses = h5f['ses']
ttime = h5f['ttime']
data = h5f['ensembleArray']

print("Creating dataframe")

df = pd.DataFrame({'ttime': np.array(ttime), 
                   'pc1': np.array(data_pca[:, 0]),
                   'pc2': np.array(data_pca[:, 1]),
                   'pc3': np.array(data_pca[:, 2]),
                   'patTags': np.array(patTags),
                   'ses': np.array(ses)})

print("Exporting dataframe")

df.to_csv('../data/df_' + pipe + '.csv')

h5f.close()

print("Routine finished")
