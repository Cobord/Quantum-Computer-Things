︠9f34ee67-d260-4d5b-a207-1a3e57036022s︠
G = Graph([(0, 1, 3), (1, 2, oo)])
W = CoxeterGroup(G)
W.coxeter_diagram() == G
CoxeterGroup(W.coxeter_diagram()) is W
︡2c1aacdf-88e4-4235-96c6-19509abc0e95︡{"stdout":"True\n"}︡{"stdout":"True\n"}︡{"done":true}︡
︠94994eb2-db27-46c3-9740-6e65b10184c3s︠
G = Graph([(1,4,4), (2,4,4), (3,5,4), (4,5,4), (5,7,4)])
W = CoxeterGroup(G)
W.coxeter_diagram() == G
CoxeterGroup(W.coxeter_diagram()) is W
︡62c14454-dabb-45b4-a2b0-d1cda1db404a︡{"stdout":"True\n"}︡{"stdout":"True\n"}︡{"done":true}︡
︠3374d966-61a7-461a-b995-a4e802456b0as︠
s = W.simple_reflections()
w = s[1]*s[2]*s[3]*s[4]*s[5]*s[7]*s[5]*s[4]*s[3]*s[2]*s[1]
w.coset_representative([]).reduced_word()
︡3cdb075a-6385-4d70-a4b8-03751143b376︡{"stdout":"[2, 1, 4, 3, 5, 7, 5, 4, 3, 2, 1]\n"}︡{"done":true}︡
︠787a151d-8059-4c06-98e1-5c7894c433b1︠
︡e4944131-c037-413d-bf7b-f9be1980cca0︡
︠6485c818-a072-4a5a-a3f6-8c51b85c358bs︠
wHelper=[choice(list([1,2,3,4,5,7])) for i in range(50)]
wHelper
w=s[wHelper[0]]
for i in wHelper[1:]:
    w=w*s[i]
w.coset_representative([]).reduced_word()
︡e5d03282-1f1a-4460-8ad1-6877b36356f2︡{"stdout":"[3, 2, 2, 4, 4, 7, 2, 7, 7, 1, 7, 1, 7, 4, 2, 3, 3, 1, 1, 7, 7, 5, 1, 5, 3, 5, 1, 2, 2, 7, 3, 7, 7, 3, 4, 3, 3, 5, 4, 7, 3, 4, 2, 7, 4, 4, 4, 2, 4, 7]\n"}︡{"stdout":"[7, 2, 4, 5, 7, 2, 4, 5, 7, 4, 2, 4, 3, 2]\n"}︡{"done":true}︡
︠90175676-664c-4689-8dd9-da115defca5as︠
basic_swap=matrix(ZZ,[[1,0,0,0],[0,0,1,0],[0,1,0,0],[0,0,0,1]])
r={}
r[1]=basic_swap.tensor_product(identity_matrix(2^5))
r[2]=(identity_matrix(2).tensor_product(basic_swap)).tensor_product(identity_matrix(2^4))
r[3]=(identity_matrix(2^2).tensor_product(basic_swap)).tensor_product(identity_matrix(2^3))
r[4]=(identity_matrix(2^3).tensor_product(basic_swap)).tensor_product(identity_matrix(2^2))
r[5]=(identity_matrix(2^4).tensor_product(basic_swap)).tensor_product(identity_matrix(2^1))
r[6]=identity_matrix(2^5).tensor_product(basic_swap)
r[7]=matrix(ZZ,[[0,1],[1,0]]).tensor_product(identity_matrix(2^6))
r[8]=matrix(ZZ,[[1,0,0,0],[0,1,0,0],[0,0,0,1],[0,0,1,0]]).tensor_product(identity_matrix(2^5))
︡25050100-4ee0-4d94-9607-a26a78811f53︡{"done":true}︡
︠d9a7f70c-1403-4423-8030-45a337ffd81cs︠
m=matrix(ZZ,identity_matrix(8))
for i in range(8):
    for j in range(8):
        current=r[i+1]*r[j+1]
        m[i,j]=-1
        if (current^1==identity_matrix(2^7)):
            m[i,j]=1
        elif (current^2==identity_matrix(2^7)):
            m[i,j]=2
        elif (current^3==identity_matrix(2^7)):
            m[i,j]=3
        elif (current^4==identity_matrix(2^7)):
            m[i,j]=4
        elif (current^5==identity_matrix(2^7)):
            m[i,j]=5
        elif (current^6==identity_matrix(2^7)):
            m[i,j]=6
        else:
            m[i,j]=-2
print(m)
︡67b31809-5606-4247-a81f-e0f4c1a52b28︡{"stdout":"[1 3 2 2 2 2 4 3]\n[3 1 3 2 2 2 2 4]\n[2 3 1 3 2 2 2 2]\n[2 2 3 1 3 2 2 2]\n[2 2 2 3 1 3 2 2]\n[2 2 2 2 3 1 2 2]\n[4 2 2 2 2 2 1 4]\n[3 4 2 2 2 2 4 1]\n"}︡{"done":true}︡
︠b1980dca-6c4c-4eb6-a94d-c37c18ad114bs︠
edge_list=[]
for i in range(8):
    for j in range(i):
        edge_list.append((j+1,i+1,m[i,j]))
G2 = Graph(edge_list)
W2 = CoxeterGroup(G2)
s2 = W2.simple_reflections()
︡8beeb97f-3100-4a89-99d8-277efddc3bb2︡{"done":true}︡
︠1e9e7903-0639-4965-836e-912f532d9698︠
for i in range(10):
    for rep in range(10):
        word=[choice(list([1,2,3,4,5,7,8])) for j in range(i+1)]
        wTesting=s2[word[0]]
        for i in word[1:]:
            wTesting=wTesting*s2[i]
        word=wTesting.coset_representative([]).reduced_word()
        if(len(word)>0):
            answer=identity_matrix(2^7)
            for j in word:
                answer=answer*r[j]
            if (answer==identity_matrix(2^7)):
                print("Yes")
                print(word)
            else:
                print("No")
                print(word)
︡6dca1e77-7d64-460e-a82a-8d713c48e2fe︡{"stdout":"No\n[3]\nNo\n[5]\nNo\n[3]\nNo\n[1]\nNo\n[5]\nNo\n[7]\nNo\n[7]\nNo\n[4]\nNo\n[2]\nNo\n[8]\nNo\n[7, 2]\nNo\n[5]\nNo\n[5]\nNo"}︡{"stdout":"\n[7, 5, 2, 3, 4, 2]\nNo\n[7, 1, 2]\nNo\n[2, 3, 2]\nNo\n[8, 4]\nNo\n[7, 4, 5]\nNo\n[5, 3]\nNo"}︡{"stdout":"\n[7, 8]\nNo\n[5]\nNo\n[8]\nNo\n[5, 3]\nNo\n[7, 8]\nNo\n[4, 2, 3]\nNo"}︡{"stdout":"\n[7, 3, 4]\nNo\n[3]\nNo\n[8]\nNo\n[1, 8, 3, 2, 8, 5, 2]\nNo"}︡{"stdout":"\n[2, 1, 7, 8, 4, 3, 1]\nNo\n[1, 8, 3, 1]\nNo\n[8, 3]\nNo\n[1, 8, 4, 3]\nNo"}︡{"stdout":"\n[3, 4, 5, 1, 2]\nNo\n[7, 4, 5, 1]\nNo\n[8, 4]\nNo\n[8, 4, 1, 2, 1]\nNo"}︡{"stdout":"\n[3, 1]\nNo\n[5, 4, 1, 2]\nNo\n[5]\nNo\n[2, 8, 5]\nNo"}︡{"stdout":"\n[5, 3, 4, 2, 3]\nNo\n[8, 5]\nNo\n[7, 2, 8, 5, 3]\nNo\n[1, 8, 1]\nNo"}︡{"stdout":"\n[8, 2, 8, 4, 5]\nNo\n[7, 1, 8, 5, 2]\nNo\n[5]\nNo\n[3, 1]\nNo\n[7, 3]\nNo\n[5, 3]\nNo"}︡{"stdout":"\n[5, 1]\nNo\n[4, 2]\nNo\n[4, 3]\nNo\n[3, 1]\nNo\n[5, 2]\nNo\n[1, 7, 3]\nNo\n[8, 5]\nNo"}︡{"stdout":"\n[8, 4, 5]\nNo\n[5, 4, 1, 2, 3, 1]\nNo\n[7, 1, 7, 5]\nNo\n[2, 8, 7, 5, 3, 2]\nNo"}︡{"stdout":"\n[7, 2, 1]\nNo\n[5, 3]\nNo\n[7, 8, 1, 7]\nNo\n[3, 4, 5, 3, 1, 2]\nNo\n[7, 2, 1, 7]\nNo"}︡{"stdout":"\n[4, 2]\nNo\n[2, 8, 7, 8, 4, 2]\nNo\n[8]\nNo\n[7, 5, 4]\nNo\n[1, 8, 5, 3, 1]\nNo"}︡{"stdout":"\n[8, 1, 7, 4, 3]\nNo\n[8, 5, 3, 2]\nNo\n[5, 4, 2]\nNo\n[7, 5, 3]\nNo\n[3, 2, 8, 4, 5, 4]\nNo"}︡{"stdout":"\n[4, 5, 2, 3]\nNo\n[2, 8, 7, 8, 7, 3, 4]\nNo\n[3, 4, 1]\nNo\n[5, 1, 2]\nNo"}︡{"stdout":"\n[4, 5, 1, 2]\nNo\n[8, 5, 2]\nNo\n[2, 8, 7, 4, 5, 3]\nNo\n[4, 1]\nNo\n[3, 1, 2]\nNo\n[7, 3]\nNo"}︡{"stdout":"\n[4, 1]\nNo\n[1, 7, 1, 2, 3, 4]\nNo\n[1, 7, 5, 3, 2]\nNo\n[7, 3, 4]\nNo\n[7, 2, 8, 5, 4]\nNo"}︡{"stdout":"\n[7, 2, 1, 7, 4, 5, 4]\nNo\n[7, 1, 8, 1, 7, 4]\nNo\n[1, 7, 4, 2, 3, 2]\nNo\n[7, 4, 5, 2, 3]\nNo"}︡{"stdout":"\n[3, 2, 8, 5]\nNo\n[2, 8, 7, 1, 2, 8, 7, 2, 8]\n"}︡{"done":true}︡
︠206a0a1e-a4a9-48fa-a40a-762f938e5fab︠









