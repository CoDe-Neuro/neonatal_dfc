## IMPORT BASIC LIBS FOR SCRIPT

import scipy as sc
from scipy import io, stats
import bstates as bs
import pickle
import numpy as np
import h5py
import feather
import pandas as pd
import os
from sys import argv

## TREATING ARGV VARS

nPats = int(argv[1]) # NUMBER OF SUBJECTS
pipe = argv[2] # PIPELINE
surr = int(argv[3])

tt = 2280  # <- NEEDS AUTOMATION - DURATION OF SIGNAL PER PATIENT

## COMMAND TO ALLOW THE USE OF HDF5 IN NETWORK (HPC) STORAGE SYSTEMS

os.environ['HDF5_USE_FILE_LOCKING'] = 'FALSE'

print("Read HDF5")

h5f = h5py.File('../data/ensembleArray.hdf5', 'r')
data = h5f['ensembleArray']

print("Read HDF5 finished")

patTags = np.repeat(np.arange(0, nPats), tt).transpose()

out = h5py.File('../data/out_' + pipe + '.hdf5', 'w')
out.create_dataset('patTags', data = patTags)
del patTags

data_mean = np.mean(data,
                    axis = 1)

data_stdev = np.std(data,
                    axis = 1)

data_skwnss = sc.stats.skew(data, 
                            axis = 1, 
                            bias = True, 
                            nan_policy = 'propagate')

data_kurt = sc.stats.kurtosis(data, 
                              axis = 1, 
                              fisher = True, 
                              bias = True, 
                              nan_policy = 'propagate')

print("PCA finished")

out.close()
h5f.close()

print("Read HDF5")

h5f = h5py.File('../data/ensembleArray.hdf5', 'r')

print("Load clinical data")

patTags = h5f['patTags']
ses = h5f['ses']
hr = h5f['hr']
ga = h5f['ga']
pma = h5f['pma']
dvarsAll = h5f['dvarsAll']
dvarsAllOutliers = h5f['dvarsAllOutliers']
data = h5f['ensembleArray']

print("Read HDF5")

out = h5py.File('../data/out_' + pipe + '.hdf5', 'r')

print("Euclidean distances")

dist_full = bs.doEuclid(data)

print("Creating dataframe")

df = pd.DataFrame({'mean': np.array(data_mean),
                   'stdev': np.array(data_stdev),
                   'skwnss': np.array(data_skwnss),
                   'kurt': np.array(data_kurt),
                   'hr': np.array(hr),
                   'ga': np.array(ga),
                   'pma': np.array(pma),
                   'dvarsAll': np.array(dvarsAll),
                   'dvarsAllOutliers': np.array(dvarsAllOutliers),
                   'patTags': np.array(patTags),
                   'ses': np.array(ses),
                   'dist_full': np.array(dist_full)})

print("Feathering dataframe")

feather.write_dataframe(df, '../data/df_' + pipe + '.feather')

h5f.close()

out.close()

print("Routine finished")
