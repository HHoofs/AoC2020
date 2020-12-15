from copy import deepcopy
from typing import Dict

from tqdm import tqdm


def final_call(last_call: int, last_called: Dict[int, int], calls: int):
    for i in tqdm(range(len(last_called) + 1, calls)):
        last_seen = last_called.get(last_call, None)
        if last_seen:
            next_call = i - last_seen
            last_called[last_call] = i
        else:
            next_call = 0
            last_called[last_call] = i

        last_call = next_call

    return last_call

if __name__ == '__main__':
    starting_numbers = open('day15_inp.txt', 'r').readlines()[0].split(',')

    last_called = {int(call): i + 1 for i, call in enumerate(starting_numbers[:-1])}
    last_call = int(starting_numbers[-1])

    print(final_call(last_call, deepcopy(last_called), 2020))
    print(final_call(last_call, deepcopy(last_called), 30000000))
