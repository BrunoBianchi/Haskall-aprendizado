soma::[Int]->Int
soma[] = 0
soma(cab:cauda) = cab +soma(cauda)

verificar::String->Char->Bool
verificar[] ch = False
verificar(cab:cauda) ch
  |  cab == ch = True
  | otherwise = verificar cauda ch

maior::[Float]->Float
maior [elemento] = elemento
maior(h:t) =
  if h > maiorcauda  then h else maiorcauda
  where maiorcauda = maior t


delta::Float->Float->Float->[Float]
delta a b c
  | delta > 0 = [raiz1,raiz2]
  | delta == 0 = [raiz1]
  | otherwise = []
  where
    delta = b*b - 4*a*c
    raiz1 = (-b+sqrt(delta))/(2*a)
    raiz2 = (-b-sqrt(delta))/(2*a)


somaPares::[Int]->Int
somaPares[] = 0
somaPares(h:t)
  | (mod h 2) == 0 = h + somaPares t
  | otherwise = somaPares t

ocorrencias::String->Char->Int
ocorrencias[] ch = 0
ocorrencias(h:t) ch
  |h == ch =  (ocorrencias t ch) + 1
  |otherwise = ocorrencias t ch

removeDupli:: [Int] -> [Int]
removeDupli [] = []
removeDupli [h] = [h]
removeDupli(h:(ht:tt))
  | h == ht = removeDupli (h:tt)
  | otherwise =  h: removeDupli (ht:tt)

divisores::Int->[Int]
divisores n = [ x| x<-[1..n],mod n x == 0]

primo::Int->Bool
primo n = if length(divisores n) == 2 then True else False
