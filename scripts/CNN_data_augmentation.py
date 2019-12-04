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


if __name__ == '__main__':
    parser = argparse.ArgumentParser()
    parser.add_argument("-s", "--source", help="path of where to read input images")
    parser.add_argument("-d", "--destination", help="path of where to write output images")
#    parser.add_argument("-r", help="recursively find all files in source directory, if false just load images in " +
#                                   "parent directory do not recurse into sub directories.")
    args = parser.parse_args()

    # Append a '/' to both source and destination if missing.
    if args.source[-1] != '/':
        args.source = args.source + '/'
    if args.destination[-1] != '/':
        args.destination = args.destination + '/'

    fs = [f for f in glob.iglob(args.source + '**/*', recursive=True) if os.path.isfile(f)]

    # Get largest image size
    max_dims = [0, 0]
    for f in fs:
        i = Image.open(f)
        if max_dims[1] < i.size[0]:
            max_dims[1] = i.size[0] + 1
        if max_dims[0] < i.size[1]:
            max_dims[0] = i.size[1] + 1

    # Standardize image size
    for f in fs:
        img = read_img(f)
        print(f)
        size = img.shape[:2]

        delta_w = max_dims[1] - size[1]
        delta_h = max_dims[0] - size[0]
        top, bottom = delta_h // 2, delta_h - (delta_h // 2)
        left, right = delta_w // 2, delta_w - (delta_w // 2)

        color = [0, 0, 0]
        new_img = cv.copyMakeBorder(img, top, bottom, left, right, cv.BORDER_CONSTANT,
                                    value=color)

        # cv.imshow("image", new_img)
        # cv.waitKey(0)
        # cv.destroyAllWindows()
        os.makedirs(os.path.dirname(os.path.join(args.destination, "standard_size", os.path.basename(f))), exist_ok=True)
        write_img(new_img, args.destination + "standard_size/" + os.path.basename(f))

        # Rotated images
        for theta in np.arange(15, 360, 15):
            rotated = imutils.rotate(new_img, theta)
            # if max_dims[1] - 100 < size[0] < max_dims[1] or max_dims[0] - 100 < size[1] < max_dims[0]:
            #     cv.imshow("Rotated (Problematic)", rotated)
            #     cv.waitKey(0)
            #     cv.destroyAllWindows()
            os.makedirs(os.path.dirname(os.path.join(args.destination, "rotated", str(theta), os.path.basename(f))),
                        exist_ok=True)
            write_img(rotated, os.path.join(args.destination + "rotated/" + str(theta) + '/' + os.path.basename(f)))
