import pickle
import numpy as np


path = './data/ch_score_LEiDa_dim.pickle'
  
with open(path, 'rb') as f: 
  ch_score = pickle.load(f)

path = './data/db_score_LEiDa_dim.pickle'
  
with open(path, 'rb') as f: 
  db_score = pickle.load(f)
  
np.savetxt("./data/ch_score_LEiDa_dim.csv", ch_score[0].transpose(), delimiter=",")
np.savetxt("./data/db_score_LEiDa_dim.csv", db_score[0].transpose(), delimiter=",")
