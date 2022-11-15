## IMPORT BASIC LIBS FOR SCRIPT

import scipy as sc
from scipy import io, stats
import bstates as bs
import sklearn as sk
from sklearn import cluster, metrics
import pickle
import numpy as np
import h5py
import feather
import pandas as pd
import os
from sys import argv

## TREATING ARGV VARS

nPats = int(argv[1])  # NUMBER OF SUBJECTS
pipe = argv[2]  # PIPELINE
surr = int(argv[3])
n_low = 2
n_high = 20

tt = 2280

## COMMAND TO ALLOW THE USE OF HDF5 IN NETWORK (HPC) STORAGE SYSTEMS

os.environ['HDF5_USE_FILE_LOCKING'] = 'FALSE'

print("Read HDF5")

h5f = h5py.File('../data/' + pipe + '.hdf5', 'r')

print("Load ensembleArray")

data = h5f['ensembleArray']

print("Load clinical data")

patTags = h5f['patTags']
ses = h5f['ses']
hr = h5f['hr']
ga = h5f['ga']
pma = h5f['pma']
ttime = h5f['ttime']

ch_score = np.zeros([1, n_high - n_low + 1])
db_score = np.zeros([1, n_high - n_low + 1])
sl_score = np.zeros([1, n_high - n_low + 1])

labs = np.zeros([data.shape[0], n_high - n_low + 1])

for i in range(n_low, n_high + 1):
    print(i)
    kmeans = sk.cluster.KMeans(
        n_clusters = i, random_state = 42).fit(data)

    labs[:, i - 2] = kmeans.labels_
    centroids = kmeans.cluster_centers_

    np.savetxt('../data/centroids_' + pipe + '_' + str(i) + '.csv',
               centroids,
               delimiter=",")

    ch_score[0, i - 2] = sk.metrics.calinski_harabasz_score(
        data, labs[:, i - 2])

    db_score[0, i - 2] = sk.metrics.davies_bouldin_score(
        data, labs[:, i - 2])

    sl_score[0, i - 2] = sk.metrics.silhouette_score(
        data, labs[:, i - 2])


np.savetxt('../data/ch_score_' + pipe + '_dim' + '.csv', ch_score[0].transpose(), delimiter=",")
np.savetxt('../data/db_score_' + pipe + '_dim' + '.csv', db_score[0].transpose(), delimiter=",")
np.savetxt('../data/sl_score_' + pipe + '_dim' + '.csv', sl_score[0].transpose(), delimiter=",")

df = pd.DataFrame({'ttime': np.array(ttime),
                   'clus2': np.array(labs[:, 0]),
                   'clus3': np.array(labs[:, 1]),
                   'clus4': np.array(labs[:, 2]),
                   'clus5': np.array(labs[:, 3]),
                   'clus6': np.array(labs[:, 4]),
                   'clus7': np.array(labs[:, 5]),
                   'clus8': np.array(labs[:, 6]),
                   'clus9': np.array(labs[:, 7]),
                   'clus10': np.array(labs[:, 8]),
                   'clus11': np.array(labs[:, 9]),
                   'clus12': np.array(labs[:, 10]),
                   'clus13': np.array(labs[:, 11]),
                   'clus14': np.array(labs[:, 12]),
                   'clus15': np.array(labs[:, 13]),
                   'clus16': np.array(labs[:, 14]),
                   'clus17': np.array(labs[:, 15]),
                   'clus18': np.array(labs[:, 16]),
                   'clus19': np.array(labs[:, 17]),
                   'clus20': np.array(labs[:, 18]),
                   'hr': np.array(hr),
                   'ga': np.array(ga),
                   'pma': np.array(pma),
                   'sub': np.array(patTags),
                   'ses': np.array(ses)})

print("Exporting dataframe")

df.to_csv('../data/df_clus_' + pipe + '_dim' + '.csv')

h5f.close()
