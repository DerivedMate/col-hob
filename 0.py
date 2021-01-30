#!/usr/bin/python3

from functools import reduce

def cmp(p, e):
  if not p or (e[1] > p[1]) or (e[1] == p[1] and e[0] < p[0]): 
    return e 
  else: 
    return p

def ser(es, e):
  if e:
    es.append(int(e))

  return es

que = {}

for e in reduce(ser, input().split(sep=' '), []):
  if e in que:
    que[e] += 1
  else:
    que[e] = 1

p, _ = reduce(cmp, que.items())
print(p)