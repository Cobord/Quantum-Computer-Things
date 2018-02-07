import networkx as nx
import matplotlib as pyplot

def toColor(weight):
    # if the weight is 2, they commute, shouldn't even be an edge
    if (weight==2):
        color='white'
    elif (weight==4):
        color='aqua'
    elif (weight==8):
        color='brown'
    elif (weight==3):
        color='black'
    else:
        color='pink'
    return color

numQubits = 5

G = nx.Graph()
G.graph['numQubits']=numQubits

j=0
for i in range(numQubits):
    G.add_node(j,name="H on "+str(i),color='blue',size=5)
    j=j+1
    G.add_node(j,name="X on "+str(i),color='blue',size=5)
    j=j+1
    G.add_node(j,name="Y on "+str(i),color='blue',size=5)
    j=j+1
    G.add_node(j,name="Z on "+str(i),color='blue',size=5)
    j=j+1
    # Z and H
    G.add_edge(j-1,j-4,weight=8)
    # Z and X
    G.add_edge(j-1,j-3,weight=4)
    # Z and Y
    G.add_edge(j-1,j-2,weight=4)
    # Y and H
    G.add_edge(j-2,j-4,weight=4)
    # Y and X
    G.add_edge(j-2,j-3,weight=4)
    # X and H
    G.add_edge(j-3,j-4,weight=8)
    if (i>0):
        G.add_node(j,name="Swap on "+str(i)+" and "+str(i-1),color='red',size=10)
        # swaps i,i-1 with one qubit gates of i
        G.add_edge(j,j-1,weight=4)
        G.add_edge(j,j-2,weight=4)
        G.add_edge(j,j-3,weight=4)
        G.add_edge(j,j-4,weight=4)
        # if i>1, then relation with previous swap
        if (i !=1):
            G.add_edge(j,j-5,weight=3)
        # swaps i,i-1 with one qubit gates of i-1
        G.add_edge(j,j-6+(i==1),weight=4)
        G.add_edge(j,j-7+(i==1),weight=4)
        G.add_edge(j,j-8+(i==1),weight=4)
        G.add_edge(j,j-9+(i==1),weight=4)
        j=j+1

# color the single qubit gates blue and the two qubit gates red
numNodes=(numQubits*4+numQubits-1)
colorList = list(str('0') * (numNodes))
for i in range(numNodes):
    colorList[i]=colorList[i].replace('0',G.nodes(1)[i][1]['color'])
# color the edges based on the value of m_{s,t}. See the helper function
numEdges=(numQubits*6+8*(numQubits-1)+(numQubits-2))
edgeColorList = list(str('0') * (numEdges))
edgeList=list(G.edges())
for i in range(numEdges):
    (source,target)=edgeList[i]
    currentEdgeColor=toColor(G[source][target]['weight'])
    edgeColorList[i]=edgeColorList[i].replace('0',currentEdgeColor)

pos=nx.spring_layout(G)
nx.draw_networkx(G,pos,node_size=50,with_labels=True,node_color=colorList,edge_color=edgeColorList)