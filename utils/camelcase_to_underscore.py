#!/usr/bin/env python

def main(filename):
    data = filename[1:]
    for char in range(65,91):
        letter = chr(char)
        data = data.replace(letter, '_' + letter.lower())
    print filename[0].lower() + data

if __name__ == "__main__":
    import sys
    filename = sys.argv[1]
    main(filename)
