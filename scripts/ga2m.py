import pickle as pk
import pandas as pd
import numpy as np
from interpret.glassbox import ExplainableBoostingClassifier
from random import seed
from math import floor
import os
import sys
from datetime import datetime
import glob


# Helper functions below, figure out how to make your own package to clean this up some time later.
# Default folder only works on Ubuntu machine, just a friendly reminder of the obvious...
def get_data(data_dir='/data/Jaryd/R/LFS_fibroblasts/data/Datasets', ext='.csv'):
    """
    Import and concatenate a set of files with the same extension from the folder data_dir, defaults are set for the
    LFS_fibroblast project.

    :param data_dir: str of directory containing csv files of the data for this project
    :param ext: str of extension of files to be concatenated
    :return: pd.DataFrame of the merged csv files
    """
    return pd.concat([pd.read_csv(f) for f in glob.glob(data_dir + "/*" + ext, recursive=False)])


seed(7)
# Ubuntu
dat = get_data()
# Windows
#dat = get_data(data_dir=)

# Read model configuration file, which will dictate what model to fit.
config = open(sys.argv[1], 'r')
configuration = {}
for line in config:
    line = line.split('=')
    configuration[line[0].strip()] = [s.strip() for s in line[1].split(',')]

# if !os.path.isdir('/data/Jaryd/R/LFS_fibroblasts/ga2m/ga2m_results/' + os.path.basename(sys.argv[1]).split('.')[0]):

res_dir = '/data/Jaryd/R/LFS_fibroblasts/ga2m/ga2m_results/' + \
          os.path.basename(sys.argv[1]).split('.')[0] + '/' +\
          datetime.now().strftime("%d-%m-%Y-%H:%M:%S") + '/'
os.makedirs(res_dir)

# Label mutant observations
dat['mutant'] = dat.Row > 3
#dat['mutant'] = dat.Column >= (max(dat.Column) - min(dat.Column))/2 + min(dat.Column)

# Apply given configuration
if configuration['subset_features'][0] != 'None':
    dat = dat[configuration['subset_features'] + ['mutant']]

# Drop labelling columns and shuffle data order.
if configuration['test'][0] == 'random':
    dat = dat.drop(columns=['Row', 'Column', 'Time', 'S', 'M', 'FocusScore3', 'FocusScore4', 'FocusScore5', 'Centroid_1', 'Centroid_2', 'Orientation']).sample(frac=1)

    # Select random train and test sets.
    dat_train = dat.iloc[:floor(len(dat) * 0.9), :]
    dat_test = dat.iloc[floor(len(dat) * 0.9):, :]
elif sum([b.isdigit() for b in configuration['test']]) == len(configuration['test']):
    # Assume the values in configuration['test'] refer to rows which will only be in the test set.
    if not (sum([int(b) in dat.Row for b in configuration['test']]) == len(configuration['test'])):
        raise Exception('not all test values are rows in the data.')
    test_rows = [int(r) for r in configuration['test']]
    # Let the test set be a specific row which corresponds to one individual in our data.
    dat_train = dat.loc[~dat['Row'].isin(test_rows)]
    dat_train = dat_train.drop(columns=['Row', 'Column', 'Time', 'S', 'M', 'FocusScore3', 'FocusScore4', 'FocusScore5', 'Centroid_1', 'Centroid_2', 'Orientation'])
    dat_test = dat.loc[dat['Row'].isin(test_rows)]
    dat_test = dat_test.drop(columns=['Row', 'Column', 'Time', 'S', 'M', 'FocusScore3', 'FocusScore4', 'FocusScore5', 'Centroid_1', 'Centroid_2', 'Orientation'])
else:
    raise Exception('test = x, where x must be random, or a comma separated seq of digits which are valid rows in the '
                    'data')

ebm = ExplainableBoostingClassifier(interactions=int(configuration['num_interaction'][0]))
ebm.fit(X=dat_train.drop(columns='mutant'), y=dat_train['mutant'])

with open(res_dir + 'ga2m_fit', 'wb') as ga2m_file:
    pk.dump(ebm, ga2m_file)

with open(res_dir + 'dat_train', 'wb') as train_file:
    pk.dump(dat_train, train_file)

with open(res_dir + 'dat_test', 'wb') as test_file:
    pk.dump(dat_test, test_file)

