from tqdm import tqdm


class Circle:
    def __init__(self, cups=None):
        self.max = len(cups)
        self.circle = {cup: Cup(cup) for cup in cups}
        next_cups = cups[1:] + [cups[0]]
        for cup, next_cup in zip(cups, next_cups):
            self.circle[cup].next = next_cup
        self.last = cups[-1]

    def move(self, active):
        pick_up_1 = self.circle[active].next
        pick_up_2 = self.circle[pick_up_1].next
        pick_up_3 = self.circle[pick_up_2].next
        new_active = self.circle[pick_up_3].next

        placement = active
        while True:
            placement -= 1
            if placement == 0:
                placement = self.max
            if placement not in (pick_up_1, pick_up_2, pick_up_3):
                break

        prev_next = self.circle[placement].next
        self.circle[placement].next = pick_up_1
        # self.circle[pick_up_1].next = pick_up_2
        # self.circle[pick_up_2].next = pick_up_3
        self.circle[pick_up_3].next = prev_next
        self.circle[active].next = new_active
        return new_active

    def end_state(self, length_out=9):
        value = 1
        values = []
        for i in range(length_out):
            value = self.circle[value].next
            values.append(str(value))
        print(' - '.join(values))

class Cup:
    def __init__(self, data):
        self.data = data
        self.next = None


if __name__ == '__main__':
    game = Circle([6,5,3,4,2,7,9,1,8] + list(range(10,1000001)))
    new_active = 6
    for round in tqdm(range(10000000)):
        new_active = game.move(new_active)
    game.end_state(2)