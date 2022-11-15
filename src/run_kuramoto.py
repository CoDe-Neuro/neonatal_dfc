## IMPORT BASIC LIBS FOR SCRIPT

import scipy as sc
import scipy.io
import numpy as np
import dynfc as dyn
import pickle
from sys import argv

## LOADING MATLAB FILE AND BOLD SIGNAL + PMA FOR ALL SUBS

mat = sc.io.loadmat('../data/2020_07_MASTER_connectomes90_All_select.mat',
    squeeze_me = True)
RSsig = mat['meanBOLDAll']

## INPUT SUBJECT # FROM ARGV 

patNum = int(argv[1])

## RUN SCRIPT FOR SUBJECT #

metastab, sync, shEntropy = dyn.run_multiPatKOP(
    RSsig[:, :, range(patNum - 1, patNum)])

print('Routine finished for patient no. ' + argv[1] + ' (signal).')

## SAVE PHASES AND SYNC MATRICES

with open('../data/kmPat_' + argv[1] + '.pickle', 'wb') as f:
    pickle.dump([metastab, sync, shEntropy], f)
