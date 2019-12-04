import os
import sys
import cv2 as cv

# This script should read all files in the given directory without recursing into subdirectories, convert them all into
# png files and save them in a sub folder labelled png.

if __name__ == '__main__':
    path = sys.argv[1]
    for r, d, f in os.walk(path):
        for name in f:
            new_img_name, old_ext = name.rsplit(".", 1)
            new_img_name += '.jpg'
            img = cv.imread(r + '/' + name, cv.IMREAD_GRAYSCALE)
            cv.imwrite(r + '/' + new_img_name, img)
