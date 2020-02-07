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
from PIL import Image
import os
import glob
import sys


def imshow(image):
    image = image / 2 + 0.5     # unnormalize
    npimg = image.numpy()
    plt.imshow(np.transpose(npimg, (1, 2, 0)))
    plt.show()

class Net(nn.Module):
    def __init__(self):
        super(Net, self).__init__()

        # TODO: fix input layer for appropriate depth
        self.conv1 = nn.Conv2d(4, 6, 5)
        self.pool = nn.MaxPool2d(2, 2)
        self.conv2 = nn.Conv2d(6, 16, 5)
        self.fc1 = nn.Linear(16 * 372 * 372, 120)
        self.fc2 = nn.Linear(120, 84)
        self.fc3 = nn.Linear(84, 2)

    def forward(self, x):
        x = self.pool(fun.relu(self.conv1(x)))
        x = self.pool(fun.relu(self.conv2(x)))
        x = x.view(-1, 16 * 372 * 372)
        x = fun.relu(self.fc1(x))
        x = fun.relu(self.fc2(x))
        x = self.fc3(x)
        return x


class LFSDataset(Dataset):
    """
    LFS fibroblast dataset, contains set of images with multiple channels each as their own black and white image. Has
    two target classes WT, and LFS.
    """

    def __init__(self, root_dir, transform=None):
        """
        Initialize LFS dataset, reads each folder in root_dir as a class, each subfolder of that is a grouping of images
        which are different channels for one sample. Checks all leaf directories (images) have the same number of
        channels (black and white images).

        :param root_dir: root directory of the dataset
        :param transform: function which transforms each sample when used for training
        """
        self.root_dir = root_dir
        self.transform = transform

        check_len.num_chan = 0
        check_len.lens = []
        def check_len(f):
            """
            Internal helper function to check the number of channels for each image is the same. More important function
            is it will capture the first number of channels and compare it to all subsequent sets of channels, and raise
            an exception if any of the images have a different number of channels.

            :param f: list of channels for an image in the dataset
            :return: len(f)
            """
            l = len(f)
            check_len.lens.append(l)
            if check_len.num_chan == 0:
                check_len.num_chan = l
            else:
                if check_len.num_chan != l:
                    # TODO: come up with better custom error indicating that the number of channels for each image is
                    #  not the same.
                    raise StopIteration

            check_len.num_chan = max(check_len.num_chan, l)
            return f

        def get_target(r):
            """
            Extract the images target from the path of it's root
            :param r: root path
            :return: target label
            """
            return
        self.samples = [(check_len(f), r.split(root_dir)[1].split('/')[0]) for r, d, f in os.walk(root_dir) if len(f) > 0]
        sum_all_channels = sum(check_len.lens)/self.num_channel
        self.num_channel = check_len.num_chan
        self.len = sum_all_channels/self.num_channel


    def __len__(self):
        """
        :return: Number of samples in the dataset
        """
        return self.len

    def __getitem__(self, item):
        return None


if __name__ == '__main__':
    parser = argparse.ArgumentParser()
    parser.add_argument("-s", "--source", help="path of where to read input images")
    parser.add_argument("-m", "--model", help="path of where to save the trained model")
    args = parser.parse_args()

    transform = transforms.Compose(
        [transforms.ToTensor(),
         transforms.Normalize((0.5, 0.5, 0.5), (0.5, 0.5, 0.5))])



    dataset = torchvision.datasets.DatasetFolder(root=args.source, transform=torchvision.transforms.ToTensor(),
                                                 loader=np.load, extensions=('npy',))

    train_data, val_data, test_data = torch.utils.data.random_split(dataset,
                                                                    [floor(0.8*len(dataset)), floor(0.1*len(dataset)),
                                                                     len(dataset) - floor(0.8*len(dataset)) -
                                                                     floor(0.1*len(dataset))])

    train_loader = torch.utils.data.DataLoader(
        # TODO: up num_workers on Linux
        train_data,
        batch_size=8,
        # Note from pytorch tutorial: If running on Windows and you get a BrokenPipeError, try setting
        # the num_worker of torch.utils.data.DataLoader() to 0.
        num_workers=0,
        shuffle=True
    )

    classes = ('WT', 'Mutant')

    net = Net()

    criterion = nn.CrossEntropyLoss()
    optimizer = optim.SGD(net.parameters(), lr=0.001, momentum=0.9)

    for epoch in range(2):  # loop over the dataset multiple times

        running_loss = 0.0
        for i, data in enumerate(train_loader, 0):
            # get the inputs; data is a list of [inputs, labels]
            inputs, labels = data

            # zero the parameter gradients
            optimizer.zero_grad()

            # forward + backward + optimize
            outputs = net(inputs)
            loss = criterion(outputs, labels)
            loss.backward()
            optimizer.step()

            # print statistics
            running_loss += loss.item()
            # if i % 10 == 9:  # print every 2000 mini-batches
            print('[%d, %5d] loss: %.3f' %
                  (epoch + 1, i + 1, running_loss))
            running_loss = 0.0

    print('Finished Training')

    PATH = args.model
    torch.save(net.state_dict(), PATH)
