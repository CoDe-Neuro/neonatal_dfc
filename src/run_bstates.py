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

Phases, syncConn, leidaArray = dyn.run_multiPatLEiDA(
    RSsig[:, :, range(patNum - 1, patNum)],0.392, .02, .1, 2)

print('Routine finished for patient no. ' + argv[1] + ' (signal).')

## SAVE PHASES AND SYNC MATRICES

with open('../data/bsPat_' + argv[1] + '.pickle', 'wb') as f:
    pickle.dump([Phases, syncConn, leidaArray], f)

