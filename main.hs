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
