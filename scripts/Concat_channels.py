""" Concatenate channels

Given a set of image folders stored in a single directory source and it's subdirectories (with no other file types
present in the directory in question) it will read all images in the each leaf directory and concatenate all images
together saving the concatenated numpy array in a copied folder tree with a different root. The set of file names and
number of files in each leaf directory is assumed to be the same.

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

    leaf_dirs = [directory for directory, subdirs, subfiles in os.walk(args.source) if not subdirs]

    for d in leaf_dirs:
        (_, _, img_paths) = next(os.walk(d))
        imgs = [read_img(d + '/' + img) for img in img_paths]
        # read_img should always read in a grayscale image with a single channel, but only a 2d numpy array. Double
        # check the number of dimensions (for safety) and extend to three if check passes.
        expanded_imgs = [np.expand_dims(img, 2) for img in imgs if len(img.shape) == 2]
        assert len(imgs) == len(expanded_imgs), "not all images in directory " + d + " were read with 2 dimensions"

        concated = np.concatenate(expanded_imgs, 2)
        new_path = d.replace(args.source, args.destination)
        os.makedirs(new_path, exist_ok=True)
        np.save(new_path + '/' + 'all_chan.npy', concated)

