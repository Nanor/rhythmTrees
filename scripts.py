from os import listdir
from os.path import isfile, join
from subprocess import Popen, PIPE
from random import choice


def main():
    path1 = 'midi/Ragtime_rtpress.com_MIDIRip/cj/'
    path2 = 'midi/Classical_Guitar_classicalguitarmidi.com_MIDIRip/'

    files1 = [join(path1, f) for f in listdir(path1) if isfile(join(path1, f))]
    files2 = [join(path2, f) for f in listdir(path2) if isfile(join(path2, f))]

    def compare(file1, file2):
        process = Popen(['./dist/build/RhythmTrees/RhythmTrees',
                         'compare', file1, file2], stdout=PIPE)
        return float(process.communicate()[0].decode('utf-8').strip())

    main_file = choice(files1)

    within = []
    for _ in range(50):
        other = choice(files1)
        if main_file == other:
            continue

        within.append(compare(main_file, other))

    between = []
    for _ in range(50):
        other = choice(files2)

        between.append(compare(main_file, other))

    print(sum(within) / len(within))
    print(sum(between) / len(between))

if __name__ == '__main__':
    main()
