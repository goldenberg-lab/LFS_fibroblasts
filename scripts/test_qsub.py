import argparse

parser = argparse.ArgumentParser()
parser.add_argument("-s", "--source", help="path of where to read input images")
parser.add_argument("-m", "--model", help="path of where to save the trained model")
args = parser.parse_args()

print('\n' + args.source)
print('\n' + args.model)
