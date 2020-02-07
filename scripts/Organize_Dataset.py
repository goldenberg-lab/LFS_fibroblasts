""" Organize LFS Fibroblast Dataset

Given a set of black and white images stored in a single directory source and it's subdirectories (with no other file
types present in the directory in question) it will organize the files to be in two subfolders each representing a
class. THen each folder within the class folder will contain all images representing the different channels of the
single sample.

TODO: This scripts requires _____ to be installed within the Python environment you are running this script in.

This script can be imported for access to two helper functions read_img, and write_img.

"""

import os
import glob
import shutil
from PIL import Image
import imutils
import numpy as np
import argparse
import cv2 as cv


if __name__ == '__main__':
    parser = argparse.ArgumentParser()
    parser.add_argument("-s", "--source", help="path of where to read input images", required=True)
    parser.add_argument("-d", "--destination", help="path of where to write output images", default='./destination/')
    args = parser.parse_args()

    # Append a '/' to both source and destination if missing.
    if args.source[-1] != '/':
        args.source = args.source + '/'
    if args.destination[-1] != '/':
        args.destination = args.destination + '/'

    # Get largest image size
    # fs = [f for f in glob.iglob(args.source + '**/*', recursive=True)
    #       if os.path.isfile(f) and f.lower().endswith('.png')]
    max_dims = [1598, 1530]
    # max_dims = [0, 0]
    # Write results to file to save for viewing later since it will take awhile to iterate through all the images.
    # log = open("D:/Jaryd/LFS/largest_image_log.txt", 'w')
    # for f in fs:
    #     try:
    #         i = Image.open(f)
    #     except:
    #         print(f + ' failed to read!')
    #         log.write('failed to read file: ' + f + '\n')
    #         continue
    #     if max_dims[1] < i.size[0]:
    #         max_dims[1] = i.size[0] + 1
    #     if max_dims[0] < i.size[1]:
    #         max_dims[0] = i.size[1] + 1
    #     print(f)
    # log.write('max_dims: ' + str(max_dims))
    # log.close()

    # For each
    wells = [w for w in glob.iglob(args.source + '**/Cells/*', recursive=True) if os.path.isdir(w)]
    for well in wells:
        plate = well.split('Plate')[1][0]
        channels = [c for c in glob.iglob(well + '/*') if os.path.isdir(c)]
        for i in range(len(channels)):
            print(i)
            imgs = [im for im in glob.iglob(channels[i] + '/*') if os.path.isfile(im)]
            for im in imgs:
                label = 'WT_p53' if os.path.basename(well)[0] in ['B', 'C'] else 'LFS_p53'
                im_num = ''.join(filter(str.isdigit, os.path.basename(im)))
                new_path = os.path.join(args.destination, label,
                                        'plate' + str(plate) + '_' + os.path.basename(well),
                                        str(im_num), os.path.basename(channels[i]) + '.png')
                print(new_path)
                os.makedirs(os.path.dirname(new_path), exist_ok=True)
                shutil.copyfile(im, new_path)

