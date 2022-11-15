## IMPORT BASIC LIBS FOR SCRIPT

import scipy as sc
from scipy import io
import pickle
import numpy as np
import h5py
import pandas as pd
import os
from sys import argv

## TREATING ARGV VARS

nPats = int(argv[1]) # NUMBER OF SUBJECTS
pipe = argv[2] # PIPELINE


## COMMAND TO ALLOW THE USE OF HDF5 IN NETWORK (HPC) STORAGE SYSTEMS

os.environ['HDF5_USE_FILE_LOCKING'] = 'FALSE'

################################################################
##################### LOAD SYNC MATRICES #######################
################################################################

with open('../data/kmPat_1.pickle', 'rb') as f:
    metastab, sync, shEntropy = pickle.load(f)

syncArray = np.zeros([sync.shape[0],
                      nPats])

metastabArray = np.zeros([sync.shape[0],
                          nPats])

entropyArray = np.zeros([sync.shape[0],
                         nPats])

idx = 0

print("Loading pickle files - It's a bit of a pickle!")

for i in range(nPats):
    
    with open('../data/kmPat_' + str(i + 1) + '.pickle', 'rb') as f:
        metastab, sync, shEntropy = pickle.load(f)
    metastabAux = np.repeat(metastab, sync.shape[0])
    entropyAux = np.repeat(shEntropy, sync.shape[0])
    metastabArray[:, i] = metastabAux
    entropyArray[:, i] = entropyAux
    syncArray[:, i] = sync[:, 0]

    idx = idx + 1
    print(idx)


################################################################
##################### MERGE SYNC MATRICES ######################
################################################################

for i in range(syncArray.shape[1]):

    if (i == 0):

        ensembleSyncArray = syncArray[:, i]
        ensembleMetastab = metastabArray[:, i]
        ensembleEntropy = entropyArray[:, i]
    else:
        ensembleSyncArray = np.hstack((ensembleSyncArray, syncArray[:, i]))
        ensembleMetastab = np.hstack((ensembleMetastab, metastabArray[:, i]))
        ensembleEntropy = np.hstack((ensembleEntropy, entropyArray[:, i]))

    print(i)

del syncArray, metastabArray, entropyArray

print("ensembleArray obtained (signal).")


## CREATING HDF5 FILE FOR ENSEMBLE ARRAY

h5f = h5py.File('../data/' + pipe + '.hdf5', 'w')

## SAVING ENSEMBLE ARRAY ON HDF5 FILE

h5f.create_dataset('ensembleSyncArray', data = ensembleSyncArray)
h5f.create_dataset('ensembleMetastab', data = ensembleMetastab)
h5f.create_dataset('ensembleEntropy', data = ensembleEntropy)

## CLEANING MEMORY TO ALLOW FURTHER OPERATIONS


print("Created HDF5")
print("ensembleArray saved")
print("Load dataset + clinical data")

## LOADING MATLAB FILE AND BOLD SIGNAL + CLINICAL DETAILS

mat = sc.io.loadmat('../data/2020_04_MASTER_connectomes90_All_select.mat',
                    squeeze_me = True)

################################################################
#################### LOADING CLINICAL DATA #####################
################################################################

## RESTRICTING TO SUBS PMA >= 37 WEEKS

pma = mat['pma'] # POST-MENSTRUAL AGE AT SCAN
pma_cut = pma[0:nPats,]

ga = mat['ga'] # GESTATIONAL AGE AT BIRTH
ga = ga[0:nPats,]

sub = mat['sub']  # SUBJECTS
sub = sub[0:nPats,]

ses = mat['ses']  # SCAN SESSION
ses = ses[0:nPats,]

hr = mat['HR']  # HIGHER RISK ASD (FAMILY)
hr = hr[0:nPats,]

sex = mat['sex']
sex = sex[0:nPats,]

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
sex = np.repeat(sex, tt).transpose()
ga = np.repeat(ga, tt).transpose()
hr = np.repeat(hr, tt).transpose()
pma_cut = np.repeat(pma_cut, tt).transpose()
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
h5f.create_dataset('sex', data = sex)
h5f.create_dataset('ga', data = ga)
h5f.create_dataset('hr', data = hr)
h5f.create_dataset('pma', data = pma_cut)
h5f.create_dataset('ttime', data = ttime)

## CLEANING MEMORY TO ALLOW FURTHER OPERATIONS

del patTags, ga, pma, ses, sex, ttime, hr

h5f.close()

print("Read HDF5")

h5f = h5py.File('../data/' + pipe + '.hdf5', 'r')

print("Load clinical data")

patTags = h5f['patTags']
ses = h5f['ses']
sex = h5f['sex']
ga = h5f['ga']
hr = h5f['hr']
pma = h5f['pma']
ttime = h5f['ttime']
ensembleSyncArray = h5f['ensembleSyncArray']
ensembleMetastab = h5f['ensembleMetastab']
ensembleEntropy = h5f['ensembleEntropy']

print("Creating dataframe")

df = pd.DataFrame({'ttime': np.array(ttime), 
                   'ga': np.array(ga),
                   'hr': np.array(hr),
                   'pma': np.array(pma),
                   'patTags': patTags,
                   'ses': ses, 
                   'sex': np.array(sex),
                   'ensembleSync': np.array(ensembleSyncArray), 
                   'ensembleMetastab': np.array(ensembleMetastab), 
                   'ensembleEntropy': np.array(ensembleEntropy)})

df['patTags'] = df['patTags'].str.decode('utf-8')
df['ses'] = df['ses'].str.decode('utf-8')

print("Feathering dataframe")

df.to_csv('../data/df_' + pipe + '.csv')

h5f.close()

print("Routine finished")
