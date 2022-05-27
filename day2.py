from nis import match


file = open('Desktop/advent-of-code/day2.txt', 'r')
x = 0
y = 0
aim = 0

for entry in file.read().splitlines():
    dir, value = entry.split()
    if dir == 'forward':
        x += int(value)
        y += aim * int(value)
    elif dir == 'up':
        aim -= int(value)
    elif dir == 'down':
        aim += int(value)
print("pt 1 answer is:", x * aim)
print("pt 2 answer is:", x * y)
file.close()