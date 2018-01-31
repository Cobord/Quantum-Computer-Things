# these are the examples in
# http://emis.ams.org/journals/EJC/Volume_17/PDF/v17i1n9.pdf

coxeterExample=[[1,3,2,2],[3,1,3,3],[2,3,1,3],[2,3,3,1]]
wordExample1 = [0,1,0,2]
# this one has the interveningWords property, outputs (-1,-1)
wordExample2 = [0,1,0,2,1]
# this one does not, it should output the segment [1,0,2,1] which fails bc doesn't have 3

# given a list of integers and an integer char, return all the locations of said i
def locationOfI(word,char):
    toReturn=[]
    for i in range(len(word)):
        if (word[i]==char):
            toReturn.append(i)
    return toReturn

def hasInterveningWords(segment,coxeterMatrix,numGens):
    toReturn = True
    for i in range(numGens):
        # only need to check neighbors of segment[0] so itself is not included
        # nor are generators which commute with it who are labeled by 2 in matrix
        if ( (i==segment[0]) | (coxeterMatrix[segment[0]][i]==2 )):
            toReturn = toReturn
        else:
            # i is a neighbor so make sure it is in the list
            toReturn = toReturn & (i in segment)
        if (not toReturn):
            # the following segment does not have property bc
            # of the printed neighbor i
            print(segment)
            print(i)
            return False
    return toReturn

# finds all segments by going through each letter and for each letter find all its occurences
#   for each segment it checks if it has all the neighbors property hiw
#   if not hiw then returns the location of that segment
#   the default answer if the word has the property is (-1,-1)
#   so if you get (-1,-1) the word might be reduced but this test is inconclusive
#   any other response gives a segment of the word which shows it is definitely not reduced
def interveningWords(word,coxeterMatrix,numGens):
    for i in range(numGens):
        locs = locationOfI(word,i)
        for j in range(len(locs)-1):
            currentSeg=word[locs[j]:(locs[j+1]+1)]
            hiw = hasInterveningWords(currentSeg,coxeterMatrix,numGens)
            if (not hiw):
                return (locs[j],locs[j+1])
    return (-1,-1)