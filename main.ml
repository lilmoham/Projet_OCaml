(*QUESTION 1*)
type candidat=string;;
type bulletin=string;;
type urne=candidat list;;
type score=int;;
type pannel=candidat list;;
type resultat=(candidat*score) list;;
(*QUESTION 2*)
let rec compte(c:candidat)(s:urne):score=
match s with
|[]->0
|x::f->if x=c then 1+compte(c)(f) else compte(c)(f);;
let lc1 = ["Eric";"Kyle";"Stan"];;
let u1 = ["Eric";"Kyle";"Stan";"Kyle";"Kyle";"Stan";"Eric";"Eric";"Kyle";"Eric";"Stan";"Eric";
"Eric";"Eric";"Stan";"Stan"];;
compte("Eric")(u1);;
(*QUESTION 3*)
let rec depouiller(lc:pannel)(u:urne):resultat=
match lc with
|[]->[]
|x::f->(x,compte x u)::depouiller f u;;
depouiller lc1 u1;;
(*QUESTION 4*)
let rec union (r:resultat)(t:resultat):resultat=
match r ,t with
|[],[]->[]
|[],(p,q)::fi->(p,q)::fi
|(x,y)::fin,[]->(x,y)::fin
|(x,y)::fin,(p,q)::fi->(x,y+q)::union(fin)(fi)  ;;
(*QUESTION 5*)
let rec max_depouiller (r:resultat):(candidat*score)=
match r with
|[]->("f",0)
|(x,y)::fin->let (p,q)=max_depouiller(fin) in if q>y then (p,q)else(x,y);;
max_depouiller([("Eric", 7); ("Kyle", 4); ("Stan", 8)]);;
(*QUESTION 6*)
let vainqueur_scrutin_uniomial(u:urne)(lc:pannel):candidat*score=
let res=depouiller(lc)(u) in max_depouiller(res);;
vainqueur_scrutin_uniomial u1 lc1;;
(*QUESTION 7*)
let rec suppr_eleme (l:'a list)(e:'a):'a list=
match l with
|[]->[]
|x::fin->if x=e then suppr_eleme(fin)(e) else x::suppr_eleme(fin)(e);;
let deux_premiers(u:urne)(p:pannel):(candidat*score)*(candidat*score)=
let k=depouiller(p)(u)in let x=max_depouiller(k)in let y=max_depouiller(suppr_eleme(k)(x)) in (x,y);;
deux_premiers(u1)(lc1);;
(*QUESTION 8/9*)
let lc2 = ["Eric";"Kyle";"Stan";"Keny"];;
let u2 = ["Keny";"Kyle";"Keny";"Kyle";"Kyle";"Keny";"Eric";"Eric";"Kyle";"Eric";"Stan";"Eric";
"Eric";"Eric";"Stan";"Stan"];;
depouiller(lc2)(u2);;
(*Nous pouvons voir que l'ajout d'un nouveau candidat a eu pour effet de réduire la part de voix des autres candidats. Cela soulève la question de savoir si le candidat qui a remporté l'élection est vraiment représentatif de la volonté de la majorité des électeurs.
De plus, dans un mode de scrutin à vote unique transférable, les électeurs n'ont souvent qu'une seule voix, qu'ils doivent donner au candidat de leur choix. Cela signifie que les électeurs ne peuvent pas exprimer leur préférence pour plusieurs candidats, ce qui peut entraîner une distorsion dans la représentation des préférences de l'électorat.
En fin de compte, cela met en évidence certaines limites du mode de scrutin à vote unique transférable, qui peut ne pas toujours être le plus représentatif de la volonté de l'électorat.
*)
(*Le problème avec le scrutin uninominal est qu'il ne permet pas une représentation précise de la volonté de l'électorat. En effet, en ne donnant qu'une seule voix à chaque électeur, il peut y avoir un biais dans la représentation des préférences de l'électorat, ce qui peut conduire à des résultats qui ne reflètent pas nécessairement la volonté de la majorité.*)

(*Dans l'élection présidentielle française de 2022, il y avait un grand nombre de candidats en lice, avec plus de 10 candidats représentant différents partis et mouvements politiques. Cela a conduit à une fragmentation de l'électorat, avec de nombreux candidats obtenant une faible proportion de voix.
L'un des exemples les plus frappants de ce phénomène est le cas de Jean-Luc Mélenchon, candidat de la France insoumise, qui a obtenu près de 20% des voix au premier tour, mais qui n'a pas réussi à se qualifier pour le second tour en raison de la fragmentation de l'électorat de gauche.
De même, Marine Le Pen, candidate du Rassemblement National, a obtenu un nombre important de voix, mais n'a pas réussi à obtenir suffisamment de soutien pour l'emporter au second tour, en raison de la polarisation de l'électorat.
Ces exemples illustrent comment le scrutin uninominal peut conduire à des résultats qui ne reflètent pas nécessairement la volonté de la majorité des électeurs, en particulier dans des contextes où il y a une forte fragmentation de l'électorat.
*)

(*QUESTION 10*)
(*pour une election avec 12 candidats est 12x6=72 possibilite par contre celui pour 13 candidats est de 13x6=78
Cela signifie qu'un électeur peut choisir parmi les combinaisons de bulletins différents dans une élection avec 12 candidats.
En comparaison, dans le scrutin uninominal, l'électeur n'a que 13 possibilités : choisir un des 12 candidats ou voter blanc/nul.
Cette différence illustre l'avantage du jugement majoritaire, qui permet à l'électeur d'exprimer une gamme plus large de préférences et donc de mieux représenter sa volonté. Cependant, il est important de noter que le comptage des votes peut être plus complexe dans le jugement majoritaire, car il faut prendre en compte la médiane des bulletins de chaque candidat pour déterminer le gagnant.*)
(*QUESTION 11*)
type mention=Tresbien|Assezbien|Arejeter|Passable;;
type bulletin_jm=mention list;;
type urne_jm=bulletin_jm list;;
(*QUESTION 12*)
let u3_jm =[[Tresbien; Assezbien; Arejeter; Passable];[Assezbien; Assezbien; Arejeter; Tresbien];[Tresbien; Arejeter; Arejeter; Tresbien]];;
let rec depouiller_jm(u:urne_jm):mention list list=
match u with
| [] -> []
| []::_ -> []
| m -> List.map List.hd m :: depouiller_jm (List.map List.tl m);;
depouiller_jm u3_jm;;
(*QUESTION 13*)
let tri_mentions(l:mention list list):mention list list=
List.map (List.rev) (List.map (List.sort compare) l) ;;
let res3=[[Tresbien; Assezbien; Tresbien];[Assezbien; Assezbien; Arejeter];[Arejeter; Arejeter; Arejeter];[Passable; Tresbien; Tresbien]];;
tri_mentions res3;;
(*QUESTION 14*)
let mediane (l:'a list):'a = 
let x=(List.length l )/2 in List.nth (List.sort compare l) x;; 
(*QUESTION 15*)
let meilleure22_mediane (l:mention list list):mention list=
let num (x:mention):int=
match x with
|Tresbien->3
|Assezbien->2
|Arejeter->0
|Passable->1 
in 
let k=List.map mediane (List.map (List.sort compare)((List.map (List.map num ) l)))
in let k1=List.sort compare k in let p=List.nth k1 (List.length k1 /2) in 
List.find (fun o -> mediane (List.map num o)=p) l;;
meilleure22_mediane (tri_mentions res3);;
(*////*)
let meilleure_mediane (l:mention list list):mention =
List.nth (List.nth (tri_mentions ([List.map (fun x-> List.nth x (List.length x /2) )(l)])) (0) ) (List.length l /2);;
meilleure_mediane (tri_mentions res3);;
(*QUESTION 16*)
let supprime_perdant (l:mention list list):mention list list=
let x=meilleure_mediane(l) in 
List.map (fun c-> if List.nth c (List.length c /2)=x then c else []) l ;;
let ms_triee =[[Assezbien; Tresbien; Tresbien];[Arejeter; Assezbien; Assezbien];[Arejeter; Arejeter; Arejeter];[Passable; Tresbien; Tresbien]];;
supprime_perdant ms_triee;;
(*QUESTION 17*)
let rec supprime_mention (l:mention list)(m:mention):mention list=
match l with
|[]->[]
|[e]->if e=m then []else [e]
|e::fin-> if e=m then fin else e::supprime_mention(fin)(m);;
let supprime_meilleure_mediane (l:mention list list):mention list list=
let m=meilleure_mediane(l) in 
let k=supprime_perdant(l) in 
List.map (fun x->supprime_mention (x)(m)) (k);;
supprime_meilleure_mediane(ms_triee);;
(*QUESTION 18*)
let rec join (l:(int*(mention list))list):(int list)*(mention list list)=
match l with
|[]->([],[])
|(u,v)::fin->let (x,y)=join(fin)in if v=[] then (x,y)else (u::x,v::y);;
let rec indexage (l:'a list)(i:int):(int*'a)list=
match l with 
|[]->[]
|pr::fin->(i,pr)::indexage(fin)(i+1);;

let rec vainqueur_jm1(l:(int*(mention list))list):int*(mention list)=
match l with
|[]->failwith ""
|[e]->e
|(u,v)::fin->
  let indexes,listeds=join ((u,v)::fin) in 
  let x=meilleure_mediane (listeds) in 
  let l1= List.filter(fun (p,q) -> if meilleure_mediane([q])=x then true else false)l in
   if List.length l1=1 then List.nth l1 0 else vainqueur_jm1(List.map (fun(b,c)->(b,supprime_mention(c)(x)))l1);;

let vainqueur_jm(l:mention list list):int*(mention list)=
let l1=indexage l 0 in
let (x,y)= vainqueur_jm1 l1 in x,List.nth l x  ;;
let ms_triee1 =[[Arejeter; Assezbien; Assezbien];[Assezbien; Tresbien; Tresbien];[Arejeter; Arejeter; Arejeter];[Passable; Tresbien; Tresbien]];;
vainqueur_jm(ms_triee1);;

(*QUESTION 19*)
(*QUESTION 20*)
(*Le jugement majoritaire est une méthode de vote qui permet de résoudre le problème du vote utile. Contrairement au vote uninominal (où chaque électeur doit choisir un seul candidat), le jugement majoritaire permet à chaque électeur de donner une mention à chaque candidat. Les mentions possibles sont par exemple Excellent, Très bon, Bon, Passable, Insuffisant, À rejeter.
Pour déterminer le gagnant, on calcule la médiane des mentions reçues par chaque candidat. Le candidat ayant la meilleure médiane est élu.
Le jugement majoritaire résout le problème du vote utile, car il permet aux électeurs de donner leur avis sur tous les candidats sans avoir peur de "gâcher leur vote". En effet, même si un candidat est peu populaire, les électeurs peuvent lui donner une mention "Bon" ou "Très bon", ce qui peut contribuer à faire remonter sa médiane et ainsi améliorer ses chances de l'emporter.
Cependant, une critique que l'on peut faire au jugement majoritaire est qu'il peut être vulnérable à la manipulation stratégique. En effet, un électeur peut chercher à maximiser les chances de son candidat préféré en donnant une mention très basse à un autre candidat populaire mais perçu comme une menace pour son candidat. De cette manière, il peut faire baisser la médiane de ce candidat et ainsi augmenter les chances de son propre candidat. Cette stratégie est appelée "bullet voting" et peut fausser les résultats du jugement majoritaire.*)

(*QUESTION 21*)
type ville=Bv of string*((candidat*score)list);;
type zone=Dpt of string|Reg of string|N of zone*(ville list);;
type arbre=N of zone*(zone list);;
(*QUESTION 22*)
let rec trouve_bv(a:arbre)(s:string):(candidat*score)list=(*restreint cas normal sans les cas vide*)
match a with
|N(Reg x ,[])->[]
|N(Reg x,N(Dpt(y),[])::fin)->trouve_bv (N(Reg x,fin)) s
|N(Reg x,N(Dpt(y),Bv(n,l)::suite)::fin)->if n=s then l else trouve_bv (N(Reg x,N(Dpt(y),suite)::fin)) s;;
let ara =
N (Reg "Auvergne-Rhône-Alpes",
 [N (Dpt "Drôme",
   [Bv ("Valence",
     [("ARTHAUD", 161); ("ROUSSEL", 595); ("MACRON", 7756); ("LASSALLE", 590);
      ("LE PEN", 4679); ("ZEMMOUR", 2080); ("MÉLENCHON", 8398);
      ("HIDALGO", 519); ("JADOT", 1701); ("PÉCRESSE", 1423); ("POUTOU", 186);
      ("DUPONT-AIGNAN", 573)]);
    Bv ("Romans-sur-Isère",
     [("ARTHAUD", 181); ("ROUSSEL", 371); ("MACRON", 4030); ("LASSALLE", 334);
      ("LE PEN", 3270); ("ZEMMOUR", 1072); ("MÉLENCHON", 4108);
      ("HIDALGO", 251); ("JADOT", 850); ("PÉCRESSE", 631); ("POUTOU", 111);
      ("DUPONT-AIGNAN", 341)])]);
  N (Dpt "Isère",
   [Bv ("Meylan",
     [("ARTHAUD", 28); ("ROUSSEL", 169); ("MACRON", 4457); ("LASSALLE", 164);
      ("LE PEN", 1288); ("ZEMMOUR", 928); ("MÉLENCHON", 2198);
      ("HIDALGO", 251); ("JADOT", 906); ("PÉCRESSE", 763); ("POUTOU", 64);
      ("DUPONT-AIGNAN", 162)]);
    Bv ("Echirolles",
     [("ARTHAUD", 104); ("ROUSSEL", 506); ("MACRON", 3276); ("LASSALLE", 259);
      ("LE PEN", 2737); ("ZEMMOUR", 779); ("MÉLENCHON", 5121);
      ("HIDALGO", 223); ("JADOT", 590); ("PÉCRESSE", 360); ("POUTOU", 92);
      ("DUPONT-AIGNAN", 202)]);
    Bv ("Fontaine",
     [("ARTHAUD", 55); ("ROUSSEL", 363); ("MACRON", 2111); ("LASSALLE", 146);
      ("LE PEN", 1835); ("ZEMMOUR", 541); ("MÉLENCHON", 3113);
      ("HIDALGO", 185); ("JADOT", 493); ("PÉCRESSE", 212); ("POUTOU", 83);
      ("DUPONT-AIGNAN", 121)]);
    Bv ("Saint-Martin-d'Hères",
     [("ARTHAUD", 58); ("ROUSSEL", 436); ("MACRON", 2769); ("LASSALLE", 207);
      ("LE PEN", 2289); ("ZEMMOUR", 661); ("MÉLENCHON", 4763);
      ("HIDALGO", 242); ("JADOT", 777); ("PÉCRESSE", 300); ("POUTOU", 119);
      ("DUPONT-AIGNAN", 161)]);
    Bv ("Gières",
     [("ARTHAUD", 16); ("ROUSSEL", 66); ("MACRON", 1071); ("LASSALLE", 84);
      ("LE PEN", 641); ("ZEMMOUR", 205); ("MÉLENCHON", 844); ("HIDALGO", 96);
      ("JADOT", 301); ("PÉCRESSE", 155); ("POUTOU", 30);
      ("DUPONT-AIGNAN", 61)]);
    Bv ("Grenoble",
     [("ARTHAUD", 256); ("ROUSSEL", 1300); ("MACRON", 15968);
      ("LASSALLE", 845); ("LE PEN", 6444); ("ZEMMOUR", 3389);
      ("MÉLENCHON", 24568); ("HIDALGO", 1488); ("JADOT", 5644);
      ("PÉCRESSE", 2019); ("POUTOU", 508); ("DUPONT-AIGNAN", 661)])])]);;
(*trouve_bv(ara)("Gières");;*)
(*QUESTION 23*)
let resultat (a:arbre)(l:string list):candidat*score=
let rec intermediaire (a:arbre)(l:string list):(candidat*score)list list=
match l with
|[]->[]
|pr::fin->trouve_bv (a) (pr)::intermediaire (a)(fin) in 
let res1=intermediaire (a)(l)in
let res2=List.fold_left union [] res1  in 
let rec maxi (r:(string*int) list):string*int=
match r with
|[]->failwith "error"
|pr::[]->pr
|pr::fin-> let (p1,r1)=maxi(fin) and (p2,r2)= pr in if r1>r2 then (p1,r1)else(p2,r2)
in maxi res2 ;;
let panel_2022 = ["ARTHAUD";"ROUSSEL";"MACRON";"LASSALLE";"LE PEN";"ZEMMOUR";"MÉLENCHON";"HIDALGO";"JADOT";"PÉCRESSE";"POUTOU";"DUPONT-AIGNAN"];;
resultat ara ["Grenoble";"Fontaine";"Valence"];;