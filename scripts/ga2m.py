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


def fit_ga2m(configuration, res_dir, predicted_variable='Row', threshold=3):
    """
    Fits a ga2m model, using the data retrieved by the function get_data, and stores the fit object the training data
    and the test set in pickle files. Always fits a two class prediction model for a given predicted_variable, and a
    threshold to separate that variable by. The predicted_variable is assumed to be ordinal. The defaults
    predicted_variable and threshold are set up for predicting the LFS and WT mutation of p53 for the individuals in the
    dataset.

    :param configuration: a dictionary of list of str
    :param res_dir: path to directory to store resulting fit model, test split and train split
    :param predicted_variable: The column in the LFS data which will be predicted.
    :param threshold: threshold for the predicted_variable
    :return: dictionary with keys 'fit', 'train', 'test' with values corresponding to the paths to the respective files.
    """
    seed(7)
    dat = get_data()

    # Label "mutant" observations, comes from the original prediction task though mutant may not be an appropriate label
    # depending on the predicted_variable, but the mutant column will be the binary predicted classes for the fit model.
    dat['mutant'] = dat[predicted_variable] > threshold
    # dat['mutant'] = dat.Column >= (max(dat.Column) - min(dat.Column))/2 + min(dat.Column)

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
        # Assume the values in configuration['test'] refer to specific entries which will only be in the test set.
        if not (sum([int(b) in dat[predicted_variable] for b in configuration['test']]) == len(configuration['test'])):
            raise Exception('not all test values are rows in the data.')
        test_rows = [int(r) for r in configuration['test']]
        # Let the test set be a set of entries, for default predicted_variable this corresponds to individuals in our
        # data.
        dat_train = dat.loc[~dat[predicted_variable].isin(test_rows)]
        # This are all indicator/irrelevant variables we don't want to consider, which should be removed from train and
        # test sets.
        dat_train = dat_train.drop(columns=['Row', 'Column', 'Time', 'S', 'M', 'FocusScore3', 'FocusScore4',
                                            'FocusScore5', 'Centroid_1', 'Centroid_2', 'Orientation'])
        dat_test = dat.loc[dat[predicted_variable].isin(test_rows)]
        dat_test = dat_test.drop(columns=['Row', 'Column', 'Time', 'S', 'M', 'FocusScore3', 'FocusScore4',
                                          'FocusScore5', 'Centroid_1', 'Centroid_2', 'Orientation'])
    else:
        raise Exception('test = x, where x must be random, or a comma separated seq of digits which are valid entries '
                        'in the predicted_variable in the data')

    # Check that the original predicted_variable isn't in the training or testing data

    ebm = ExplainableBoostingClassifier(interactions=int(configuration['num_interaction'][0]))
    ebm.fit(X=dat_train.drop(columns='mutant'), y=dat_train['mutant'])

    with open(res_dir + 'ga2m_fit', 'wb') as ga2m_file:
        pk.dump(ebm, ga2m_file)

    with open(res_dir + 'dat_train', 'wb') as train_file:
        pk.dump(dat_train, train_file)

    with open(res_dir + 'dat_test', 'wb') as test_file:
        pk.dump(dat_test, test_file)

    return {'fit': res_dir + 'ga2m_fit', 'train': res_dir + 'dat_train', 'test': res_dir + 'dat_test'}


if __name__ == '__main__':

    # Read model configuration file, which will dictate what model to fit.
    config_f = open(sys.argv[1], 'r')
    config = {}
    for line in config_f:
        line = line.split('=')
        config[line[0].strip()] = [s.strip() for s in line[1].split(',')]

    res_dir = '/data/Jaryd/R/LFS_fibroblasts/ga2m/ga2m_results/' + \
              os.path.basename(sys.argv[1]).split('.')[0] + '_test/' +\
              datetime.now().strftime("%d-%m-%Y-%H:%M:%S") + '/'
    os.makedirs(res_dir)

    fit_ga2m(config, res_dir)

