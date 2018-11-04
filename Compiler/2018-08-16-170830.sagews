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
︠90175676-664c-4689-8dd9-da115defca5a︠









