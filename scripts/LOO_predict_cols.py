import ga2m
import os
import sys
from datetime import datetime as dt

if __name__ == '__main__':
    paths = {}
    for test in ['2', '3', '4', '5', '6', '7', '8', '9', '10', '11']:
        config = {'subset_features': ['None'], 'num_interaction': ['5'], 'test': [test]}
        res_dir = '/data/Jaryd/R/LFS_fibroblasts/ga2m/ga2m_results/LOO_Columns/' + \
                  dt.now().strftime("%d-%m-%Y") + '/' + str(test) + '/'
        os.makedirs(res_dir, exist_ok=True)

        paths[test] = ga2m.fit_ga2m(config, res_dir, predicted_variable='Column', threshold=6)
