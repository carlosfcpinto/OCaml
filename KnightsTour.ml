##!/usr/bin/python
## Uncomment the following two lines 
## to enable JIT compilation.
import psyco
psyco.full()

knightsmoves = [(-1,-2), (-2,-1), (1,-2), (-2,1), (-1,2), (2,-1), (1,2), (2,1)]
## Uncomment one of the following two definitions if you
## Want an alternative order of search.
#knightsmoves = [((-1)**y*(1+x), (-1)**z*(2-x))
#		for x in [0,1]
#		for y in [0,1]
#		for z in [0,1]]
#knightsmoves = [(1, 2), (1, -2), (-1, 2), (-1, -2), (2, 1), (2, -1), (-2, 1), (-2, -1)]

def add(pos1,pos2):
      return (pos1[0] + pos2[0], pos1[1] + pos2[1])

def onboard(pos):
      return (pos[0] >= 0) and \
            (pos[0] < 8) and \
            (pos[1] >= 0) and \
            (pos[1] < 8)

def moves(pos):
      return [newpos for newpos in
                         [add(pos,move) for move in knightsmoves]
                     if onboard(newpos)]

def filmoves(pos,soln):
      return [newpos for newpos in moves(pos)
                     if not (newpos in soln)]

def compval(pos1,pos2,soln):
      return len(filmoves(pos1,soln)) - len(filmoves(pos2,soln))

def sortedmoves(soln):
      list = filmoves(soln[0],soln[1:])
      list.sort(lambda x,y: compval(x,y,soln))
      return list

def extend(soln):
      if len(soln) == 64:
            if soln[-1] in moves(soln[0]):
                  return soln
            else:
                  return False
      else:
            for newpos in sortedmoves(soln):
                  # We have already checked this
                  #if newpos in soln:
                  #      continue
                  #print "(",newpos[0],",",newpos[1],")"
                  sol=extend([newpos]+soln)
                  if not sol:
                        continue
                  else:
                        return sol
            return False

def	printsol(soln):
	line = "-"
	for i in range(8):
		for j in range(8):
			line = line + "-----"
		print line
		line = "|"
		for j in range(8):
			line = line + " %2i |" % (64-soln.index((i,j))) 
		print line
		line = "-"
	for j in range(8):
		line = line + "-----"
	print line

## Uncomment these lines if you want an executable
#from sys import argv
#
#if (len(argv) > 2):
#	printsol(extend([(int(argv[1]), int(argv[2]))]))
#else:
#	print "Usage:",argv[0],"x y\n",