from collections import deque
from copy import deepcopy
from typing import List

rule = {0: [[1, 2]],
        1: 'a',
        2: [[1, 3], [3, 1]],
        3: 'b'}

rule = {0: [[4, 1, 5]],
1: [[2, 3] , [3, 2]],
2: [[4, 4] , [5, 5]],
3: [[4, 5] , [5, 4]],
4: "a",
5: "b"}

patt = ['ababbb', 'abbbab', 'bababa']

def check_patter(rules, idx, patt: str):
    start = deque(rules[idx])

    b = set(['a', 'b'])

    def iters(ids: List):
        fin = ''
        finn = True
        cur = []
        while ids and finn:
            id = ids.pop()
            child_rule = rules[id]
            for chil_rule in child_rule:
                if isinstance(chil_rule, str) and len(chil_rule) == 1 and chil_rule in b:
                    fin = chil_rule + fin
                    cur = [id] + cur
                else:
                    finn = False
                    if fin == patt[-len(fin):] or fin == '':
                        ff_x = ids + chil_rule + cur
                        if len(ff_x) <= len(patt):
                            start.append(ff_x)
        if finn:
            return fin

    while start:
        cur_rule = start.popleft()
        a = iters(cur_rule)
        if a == patt:
            return True

    return False

rules = {}

pat = []

with open("inp/day19_inp2.txt", "r") as rr:
    for r in rr.readlines():
        if ':' in r:
            k, v = r.rstrip().split(':')
            k = int(k)
            f = []
            if '|' in v:
                pa = v.lstrip().split(' | ')
            else:
                pa = [v.lstrip()]
            for p in pa:
                try:
                    f.append([int(a) for a in p.split(' ')])
                except:
                    f = p[1]
            rules[k] = f
        else:
            pat.append(r.rstrip())

print(rules)
print(patt)

patt = pat[1:]
rule = rules

# exit(0)

ss = 0

for pa in patt:
    print(pa)
    ff = check_patter(deepcopy(rule), 0, pa)
    if ff:
        ss += 1

print(ss)
