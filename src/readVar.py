import pickle
import numpy as np


path = './data/varExp_LEiDa.pickle'
  
with open(path, 'rb') as f: 
  varExp = pickle.load(f)

np.savetxt("./data/varExp_LEiDa.csv", varExp, delimiter=",")
