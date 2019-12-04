""" Train CNN


"""
import argparse
import torch
import torchvision
import torchvision.transforms as transforms
import torch.nn as nn
import torch.nn.functional as fun
import torch.optim as optim
import matplotlib.pyplot as plt
import numpy as np
import os
import glob


def imshow(image):
    image = image / 2 + 0.5     # unnormalize
    npimg = image.numpy()
    plt.imshow(np.transpose(npimg, (1, 2, 0)))
    plt.show()


def lfs_fibro_loader(data_path):
    # train_dataset = torchvision.datasets.ImageFolder(
    #     root=data_path,
    #     transform=torchvision.transforms.ToTensor()
    # )
    # loader = torch.utils.data.DataLoader(
    #     train_dataset,
    #     batch_size=64,
    #     # Note from pytorch tutorial: If running on Windows and you get a BrokenPipeError, try setting
    #     # the num_worker of torch.utils.data.DataLoader() to 0.
    #     num_workers=0,
    #     shuffle=True
    # )
    if data_path[-1] != '/':
        data_path = data_path + '/'

    # TODO: change the loader here to return a list of tuples where the first element is the path to the image, the \
    #  second element is the label ('WT', 'LFS')
    loader = [f for f in glob.iglob(data_path + '**/*', recursive=True) if os.path.isfile(f)]
    return loader


class Net(nn.Module):
    def __init__(self):
        super(Net, self).__init__()

        # TODO: fix input layer for appropriate depth
        self.conv1 = nn.Conv2d(1, 6, 5)
        self.pool = nn.MaxPool2d(2, 2)
        self.conv2 = nn.Conv2d(6, 16, 5)
        self.fc1 = nn.Linear(16 * 5 * 5, 120)
        self.fc2 = nn.Linear(120, 84)
        self.fc3 = nn.Linear(84, 10)

    def forward(self, x):
        x = self.pool(fun.relu(self.conv1(x)))
        x = self.pool(fun.relu(self.conv2(x)))
        x = x.view(-1, 16 * 5 * 5)
        x = fun.relu(self.fc1(x))
        x = fun.relu(self.fc2(x))
        x = self.fc3(x)
        return x



if __name__ == '__main__':
    parser = argparse.ArgumentParser()
    parser.add_argument("-s", "--source", help="path of where to read input images")
    parser.add_argument("-m", "--model", help="path of where to save the trained model")
    args = parser.parse_args()

    transform = transforms.Compose(
        [transforms.ToTensor(),
         transforms.Normalize((0.5, 0.5, 0.5), (0.5, 0.5, 0.5))])

    train_loader = lfs_fibro_loader(args.source)

    # test_set = torchvision.datasets.CIFAR10(root='./data', train=False,
    #                                        download=True, transform=transform)
    # test_loader = torch.utils.data.DataLoader(testset, batch_size=4,
    #                                          shuffle=False, num_workers=2)

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
            if i % 2000 == 1999:  # print every 2000 mini-batches
                print('[%d, %5d] loss: %.3f' %
                      (epoch + 1, i + 1, running_loss / 2000))
                running_loss = 0.0

    print('Finished Training')

    PATH = args.model
    torch.save(net.state_dict(), PATH)
