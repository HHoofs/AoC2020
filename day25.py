value = 1
remainder = 20201227
loops = 1
retrieved = []

while True:
    value *= 7
    value = value % remainder
    if value == 9232416:
        retrieved.append(loops)
        if len(retrieved) == 2:
            break
    if value == 14144084:
        retrieved.append(loops)
        if len(retrieved) == 2:
            break
    loops += 1


print(retrieved)