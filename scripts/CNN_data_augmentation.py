""" Image data augmentation

Given a set of images stored in a single directory source and it's subdirectories (with no other file types present in
the directory in question) it will pad each image with black pixels to the size of the largest image in the set, and then
rotate the cell in each image saving a copy for every 15 degrees. Each stage will save a copy of the image in order to
check the correctness by hand or to view samples. These partial and final augmented images will be saved to a new
directory destination with the structure of source preserved in this new parent.

This scripts requires opencv-python, argparse, glob, imutils, and PIL to be installed within the Python environment you are
running this script in.

This script can be imported for access to two helper functions read_img, and write_img.

"""

import os
import glob
from PIL import Image
import imutils
import numpy as np
import argparse
import cv2 as cv


def read_img(path):
    """
    Returns a matrix of the read in image located at path.
    :param path: string of a file path
    :return: image read from path
    """
    im = cv.imread(path, 0)

    if im is None:
        raise Exception('failed to read file: {}'.format(path))

    return im


def write_img(im, path):
    """
    Writes an image to a file, raises an exception if it fails to write.
    :param im: an image object
    :param path: a string which is the path of where to write the image as a file
    :return: im, raises exception if write failed
    """
    if not cv.imwrite(path, im):
        raise Exception('failed to write image to file: {}'.format(path))

    return im


def pad_image(im, y, x, colour=(0,0,0)):
    """
    Pads a given image im with black pixels so the final image has shape (c, x_targ, y_targ)
    :param im: image object as returned by opencv2 imread
    :param x: int, resulting size in x dimension
    :param y: int, resulting size in y dimension
    :param colour: list of int, where len(colour) == 3 the colour for all padded pixels, default is black.
    :return: image object with new dimensions
    """
    size = im.shape[:2]

    delta_w = x - size[1]
    delta_h = y - size[0]
    top, bottom = delta_h // 2, delta_h - (delta_h // 2)
    left, right = delta_w // 2, delta_w - (delta_w // 2)

    return cv.copyMakeBorder(im, top, bottom, left, right, cv.BORDER_CONSTANT, value=colour)


if __name__ == '__main__':
    parser = argparse.ArgumentParser()
    parser.add_argument("-s", "--source", help="path of where to read input images", required=True)
    parser.add_argument("-d", "--destination", help="path of where to write output images", default='./destination/')
#    parser.add_argument("-r", help="recursively find all files in source directory, if false just load images in " +
#                                   "parent directory do not recurse into sub directories.")
    args = parser.parse_args()

    # Append a '/' to both source and destination if missing.
    if args.source[-1] != '/':
        args.source = args.source + '/'
    if args.destination[-1] != '/':
        args.destination = args.destination + '/'

    # Get largest image size
    fs = [f for f in glob.iglob(args.source + '**/*', recursive=True) if os.path.isfile(f)]
    max_dims = [1500, 1500]
    # max_dims = [0, 0]
    # for f in fs:
    #     try:
    #         i = Image.open(f)
    #     except:
    #         print(f)
    #         continue
    #     if max_dims[1] < i.size[0]:
    #         max_dims[1] = i.size[0] + 1
    #     if max_dims[0] < i.size[1]:
    #         max_dims[0] = i.size[1] + 1
    #     print(f)

    # For each
    wells = [w for w in glob.iglob(args.source + '*') if os.path.isdir(w)]
    for well in wells:
        channels = [c for c in glob.iglob(well + '/*') if os.path.isdir(c)]
        padded_single_channel = [[] for x in range(len(channels))]
        channel_names = [os.path.basename(c) for c in channels]
        for i in range(len(channels)):
            print(i)
            for f in glob.iglob(channels[i] + '/*'):
                if os.path.isfile(f):
                    padded_single_channel[i].append(pad_image(read_img(f), max_dims[0], max_dims[1]))
                    # cv.imshow("image", padded_single_channel[i][-1])
                    # cv.waitKey(0)
                    # cv.destroyAllWindows()

        for im_num in range(len(padded_single_channel[0])):
            for ch_num in range(len(padded_single_channel)):
                for theta in np.arange(0, 360, 60):
                    rotated = imutils.rotate(padded_single_channel[ch_num][im_num], theta)
                    # if max_dims[1] - 100 < size[0] < max_dims[1] or max_dims[0] - 100 < size[1] < max_dims[0]:
                    #     cv.imshow("Rotated (Problematic)", rotated)
                    #     cv.waitKey(0)
                    #     cv.destroyAllWindows()

                    label = 'WT_p53' if os.path.basename(well)[0] in ['B', 'C'] else 'LFS_p53'
                    new_path = os.path.join(args.destination, label, os.path.basename(well),
                                            str(theta), str(im_num), channel_names[ch_num] + '.png')
                    print(new_path)
                    os.makedirs(os.path.dirname(new_path), exist_ok=True)
                    write_img(rotated, new_path)

