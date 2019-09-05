import pickle as pk
import glob
from functools import reduce
import matplotlib.pyplot as plt
import numpy as np
from sklearn import metrics
from datetime import datetime as dt
import os

result_dirs1 = glob.glob('/data/Jaryd/R/LFS_fibroblasts/ga2m/ga2m_results/all_features/[0-9]**/', recursive=False)
res_dir1 = result_dirs1[np.argmax([dt.strptime(d.split('/')[-2], "%d-%m-%Y-%H:%M:%S") for d in result_dirs1])]

result_dirs2 = glob.glob('/data/Jaryd/R/LFS_fibroblasts/ga2m/ga2m_results/interactions/[0-9]**/', recursive=False)
res_dir2 = result_dirs2[np.argmax([dt.strptime(d.split('/')[-2], "%d-%m-%Y-%H:%M:%S") for d in result_dirs2])]

if len([f for f in os.listdir(res_dir1) if os.path.isfile(os.path.join(res_dir1, f))]) == 0:
    print('directory is empty, check if pycharm is still generating latest models, else other error has occured.')

if len([f for f in os.listdir(res_dir2) if os.path.isfile(os.path.join(res_dir2, f))]) == 0:
    print('directory is empty, check if pycharm is still generating latest models, else other error has occured.')

with open(res_dir1 + 'ga2m_fit', 'rb') as mdl_file:
    mdl1 = pk.load(mdl_file)

with open(res_dir2 + 'ga2m_fit', 'rb') as mdl_file:
    mdl2 = pk.load(mdl_file)

with open(res_dir1 + 'dat_test', 'rb') as test_file:
    test1 = pk.load(test_file)

with open(res_dir2 + 'dat_test', 'rb') as test_file:
    test2 = pk.load(test_file)

test_probs1 = mdl1.predict_proba(test1.drop(columns='mutant'))[:, 1]
fpr1, tpr1, thresholds1 = metrics.roc_curve(test1['mutant'].to_numpy(), test_probs1, pos_label=1)
auc1 = metrics.auc(fpr1, tpr1)

test_probs2 = mdl2.predict_proba(test2.drop(columns='mutant'))[:, 1]
fpr2, tpr2, thresholds2 = metrics.roc_curve(test2['mutant'].to_numpy(), test_probs2, pos_label=1)
auc2 = metrics.auc(fpr2, tpr2)

auc_diff = [auc1 - auc2]

for i in range(1000):
    boot_test2 = test2.sample(frac=1, replace=True, axis=0)
    test_probs2 = mdl2.predict_proba(boot_test2.drop(columns='mutant'))[:, 1]
    fpr2, tpr2, thersholds2 = metrics.roc_curve(boot_test2['mutant'].to_numpy(), test_probs2, pos_label=1)
    auc2 = metrics.auc(fpr2, tpr2)
    auc_diff.append(auc1 - auc2)

print(reduce(lambda count, n: count+(n < 0), auc_diff, 0))

plt.hist(auc_diff, bins=200)
plt.ylabel('# of times')
plt.show()

# Ended up having a significant difference, but it's still a small effect size.
