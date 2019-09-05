import pickle as pk
import pandas as pd
from interpret.glassbox import ExplainableBoostingClassifier
from random import seed
from math import floor
import os
import sys
from datetime import datetime
import glob


# Helper functions below, figure out how to make your own package to clean this up some time later.
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
dat = get_data()

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
dat['mutant'] = dat.Row >= 3
# Drop labelling columns and shuffle data order.
dat = dat.drop(columns=['Row', 'Column', 'Time', 'S', 'M', 'FocusScore3', 'FocusScore4', 'FocusScore5', 'Centroid_1', 'Centroid_2', 'Orientation']).sample(frac=1)

# Apply given configuration
if configuration['subset_features'][0] != 'None':
    dat = dat[configuration['subset_features'] + ['mutant']]

dat_train = dat.iloc[:floor(len(dat)*0.9), :]
dat_test = dat.iloc[floor(len(dat)*0.9):, :]

ebm = ExplainableBoostingClassifier(interactions=int(configuration['num_interaction'][0]))
ebm.fit(X=dat_train.drop(columns='mutant'), y=dat_train['mutant'])

with open(res_dir + 'ga2m_fit', 'wb') as ga2m_file:
    pk.dump(ebm, ga2m_file)

with open(res_dir + 'dat_train', 'wb') as train_file:
    pk.dump(dat_train, train_file)

with open(res_dir + 'dat_test', 'wb') as test_file:
    pk.dump(dat_test, test_file)

