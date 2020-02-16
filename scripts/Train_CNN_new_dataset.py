""" Train CNN


"""
import argparse
import torch
import torchvision
import torchvision.transforms as transforms
import torch.nn as nn
import torch.nn.functional as fun
import torch.optim as optim
from torch.utils.data.sampler import RandomSampler
from torch.utils.data import Dataset, DataLoader
import matplotlib.pyplot as plt
import numpy as np
from math import floor
import cv2 as cv
from PIL import Image
import os
import glob
import sys
import re
import warnings as w
import imutils


def imshow(image):
    image = image / 2 + 0.5     # unnormalize
    npimg = image.numpy()
    plt.imshow(np.transpose(npimg, (1, 2, 0)))
    plt.show()


class Net(nn.Module):
    def __init__(self, in_size, kernel_size=5):
        """

        :param in_size: tuple, 3 element tuple of the form (channels, y, x)
        :param kernel_size: int, size of the kernel for the convolution layers.
        """
        super(Net, self).__init__()
        self.stride = 1
        self.padding = 0

        self.conv1 = nn.Conv2d(in_size[0], 6, kernel_size, stride=self.stride, padding=self.padding)
        conv1_outsize = (floor((in_size[1] - kernel_size + 2*self.padding)/self.stride + 1),
                         floor((in_size[2] - kernel_size + 2*self.padding)/self.stride + 1))
        self.pool = nn.MaxPool2d(2, 2)
        pool1_outsize = (floor((conv1_outsize[0] - 2)/2 + 1), floor((conv1_outsize[1] - 2)/2 + 1))
        self.conv2 = nn.Conv2d(6, 16, kernel_size, stride=self.stride, padding=self.padding)
        conv2_outsize = (floor((pool1_outsize[0] - kernel_size + 2*self.padding)/self.stride + 1),
                         floor((pool1_outsize[1] - kernel_size + 2*self.padding)/self.stride + 1))
        pool2_outsize = (floor((conv2_outsize[0] - 2)/2 + 1), floor((conv2_outsize[1] - 2)/2 + 1))
        self.fc1 = nn.Linear(16 * np.prod(pool2_outsize), 120)
        self.fc2 = nn.Linear(120, 84)
        self.fc3 = nn.Linear(84, 2)

    def forward(self, x):
        x = self.pool(fun.relu(self.conv1(x)))
        x = self.pool(fun.relu(self.conv2(x)))
        x = x.view(-1, np.prod(x.shape[1:]))
        x = fun.relu(self.fc1(x))
        x = fun.relu(self.fc2(x))
        x = self.fc3(x)
        return x


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


def pad_image(im, y, x, colour=(0, 0, 0)):
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


def aug_image(d, paths, max_dims):
    """
    Randomly augment the images when read in.
    :param d: string, path to the directory all images are contained in
    :param paths: list of string, list of paths to images to be read in, padded, and augmented all in the same way.
    :param max_dims: list of integers, dimensions for the end images after augmentation, should be the largest size in
                        the set of images.
    :return: list of augmented images
    """
    theta = np.random.randint(0, 359, 1)

    imgs = [imutils.rotate(pad_image(read_img(d + '/' + img), max_dims[0], max_dims[1]), theta) for img in paths]
    return imgs


def get_max_dims(root_dir):
    """
    Given a root directory, it will attempt to read each file with extension .png in root_dir as an image , if it fails
    it will warn the user that the file was not read. It will then return the largest set of dimensions for all
    successfully read images in root_dir.
    :param root_dir:
    :return: list of two integers, dimensions for the largest image in root_dir.
    """
    fs = [f for f in glob.iglob(args.source + '**/*', recursive=True)
          if os.path.isfile(f) and f.lower().endswith('.png')]
    max_dims = [0, 0]
    for f in fs:
        try:
            i = Image.open(f)
        # TODO: make this exception more specific
        except:
            w.warn('Failed to read file: ' + f)
            continue
        if max_dims[1] < i.size[0]:
            max_dims[1] = i.size[0] + 1
        if max_dims[0] < i.size[1]:
            max_dims[0] = i.size[1] + 1
    return max_dims


def LFS_loader(d, fs, max_dims):
    """
    Call aug_image on each image in dir, then concatenate all images together.
    :param d: string, path to directory where each f in fs is stored
    :param fs: list of string, file basenames in dir
    :param max_dims: list of integer, to be passed to aug_image
    :return: numpy array
    """
    imgs = aug_image(d, fs, max_dims)
    expanded_imgs = [np.expand_dims(img, 0) for img in imgs if len(img.shape) == 2]
    assert len(imgs) == len(expanded_imgs), "not all images in directory " + d + " were read with 2 dimensions"

    return np.concatenate(expanded_imgs, 0)


class LFSDataset(Dataset):
    """
    LFS fibroblast dataset, contains set of images with multiple channels each as their own black and white image. Has
    two target classes WT, and LFS.
    """

    def __init__(self, root_dir, transform=None, max_dims=None, exclude_files=None):
        """
        Initialize LFS dataset, reads each folder in root_dir as a class, each subfolder of that is a grouping of images
        which are different channels for one sample. Checks all leaf directories (images) have the same number of
        channels (black and white images).

        :param root_dir: root directory of the dataset
        :param transform: function which transforms each sample when used for training
        :param max_dims: list of integers, the dimension of the largest image for all images to get padded to.
                            Calculating the largest size of any image in the set can take a long time, so if running
                            multiple times it can be best to calculate before and pass in. It will only calculate the
                            largest image if max_dims is None.
        :param exclude_files: string, regex pattern indicating which leaf directories in root_dir to be excluded.
        """
        def _check_len(f):
            """
            Internal helper function to check the number of channels for each image is the same. Additionally
            it will capture the first number of channels and compare it to all subsequent sets of channels, and raise
            an exception if any of the images have a different number of channels.

            :param f: list of channels for an image in the dataset
            :return: len(f)
            """
            l = len(f)
            _check_len.lens.append(l)
            if _check_len.num_chan == 0:
                _check_len.num_chan = l
            else:
                if _check_len.num_chan != l:
                    # TODO: come up with better custom error indicating that the number of channels for each image is
                    #  not the same.
                    raise StopIteration

            _check_len.num_chan = max(_check_len.num_chan, l)
            return f

        # attributes for the internal function check_len, so that the tree only needs to be traversed once.
        _check_len.num_chan = 0
        _check_len.lens = []

        self.classes = os.listdir(root_dir)
        self.classes.sort()
        self.class_to_idx = {self.classes[i]: i for i in range(len(self.classes))}
        self.root_dir = root_dir
        self.transform = transform

        if exclude_files:
            self.exclude = re.compile(re.escape(exclude_files))
            self.samples = [(r, _check_len(f), re.findall(r"[^\\/]+", r.split(root_dir)[1])[0])
                            for r, d, f in os.walk(root_dir) if len(f) > 0 and not self.exclude.search(r)]
        else:
            self.exclude = exclude_files
            self.samples = [(r, _check_len(f), re.findall(r"[^\\/]+", r.split(root_dir)[1])[0])
                            for r, d, f in os.walk(root_dir) if len(f) > 0]

        self.num_channel = _check_len.num_chan
        self.len = sum(_check_len.lens) // self.num_channel
        if max_dims is None:
            self.max_dims = get_max_dims(root_dir)
        else:
            self.max_dims = max_dims

    def __len__(self):
        """
        :return: Number of samples in the dataset
        """
        return self.len

    def __getitem__(self, item):
        """

        :param item:
        :return:
        """
        sample = LFS_loader(self.samples[item][0], self.samples[item][1], self.max_dims)

        return sample, self.class_to_idx[self.samples[item][2]]


if __name__ == '__main__':
    parser = argparse.ArgumentParser()
    parser.add_argument("-s", "--source", help="path of where to read input images")
    parser.add_argument("-m", "--model", help="path of where to save the trained model")
    parser.add_argument("-I", "--image_size", nargs=3, type=int, help="dimension of largest image in dataset, "
                                                                      "first dimension is number of channels, "
                                                                      "second and third are width and Height")
    parser.add_argument("-e", "--exclude", help="regex pattern to match directories which should be excluded in the "
                                                "dataset.")
    parser.add_argument("-l", "--log", help="path to file to write loss at each epoch.")
    args = parser.parse_args()

    device = torch.device("cuda:0" if torch.cuda.is_available() else "cpu")

    transform = transforms.Compose(
        [transforms.ToTensor(),
         transforms.Normalize((0.5, 0.5, 0.5), (0.5, 0.5, 0.5))])

    dataset = LFSDataset(root_dir=args.source, transform=torchvision.transforms.ToTensor(),
                         max_dims=args.image_size[1:], exclude_files=args.exclude)

    train_data, val_data, test_data = torch.utils.data.random_split(dataset,
                                                                    [floor(0.8*len(dataset)), floor(0.1*len(dataset)),
                                                                     len(dataset) - floor(0.8*len(dataset)) -
                                                                     floor(0.1*len(dataset))])

    train_loader = torch.utils.data.DataLoader(
        # TODO: increase num_workers on Linux
        train_data,
        batch_size=8,
        num_workers=0,
        shuffle=True
    )

    classes = ('WT', 'Mutant')

    net = Net(in_size=args.image_size)

    criterion = nn.CrossEntropyLoss()
    optimizer = optim.SGD(net.parameters(), lr=0.001, momentum=0.9)

    for epoch in range(2):  # loop over the dataset multiple times

        running_loss = 0.0
        for i, data in enumerate(train_loader, 0):
            # get the inputs; data is a list of [inputs, labels]
            inputs, labels = data[0].to(device, dtype=torch.float32), data[1].to(device, dtype=torch.long)

            # zero the parameter gradients
            optimizer.zero_grad()

            # forward + backward + optimize
            outputs = net(inputs)
            loss = criterion(outputs, labels)
            loss.backward()
            optimizer.step()

            # print statistics
            running_loss += loss.item()
            if i % 10 == 9:  # print every 10 mini-batches
                print('[%d, %5d] loss: %.3f' % (epoch + 1, i + 1, running_loss/10))
                log = open(args.log, "a")
                log.write('[%d, %5d] loss: %.3f\n' % (epoch + 1, i + 1, running_loss/10))
                log.close()
                running_loss = 0.0


    print('Finished Training')

    PATH = args.model
    torch.save(net.state_dict(), PATH)
