{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE MultiParamTypeClasses,
             TypeSynonymInstances, FlexibleInstances #-}

module GAS where

import ProblemState

import qualified Data.Map.Strict as M

{-
    Pozițiile tablei de joc, în formă (linie, coloană), unde ambele coordonate
    pot fi negative.
-}
type Position = (Int, Int)

{-
    Culorile pătratelor și cercurilor.
-}
data Color = Red | Blue | Gray
    deriving (Eq, Ord, Show)

{-
    Orientările pătratelor și săgeților.
-}
data Heading = North | South | East | West
    deriving (Eq, Ord)

instance Show Heading where
    show North = "^"
    show South = "v"
    show East  = ">"
    show West  = "<"

{-
    *** TODO ***

    Un obiect de pe tabla de joc: pătrat/ cerc/ săgeată.
-}
data Object = P { culoare_p :: Color, orientare_p :: Heading } | C { culoare_c :: Color } | S { orientare_s :: Heading } deriving (Eq, Ord)

{-
    *** TODO ***

    Reprezetarea textuală a unui obiect.
-}
instance Show Object where
	show (P culoare_p orientare_p) 
								| culoare_p == Red = "R" ++ show orientare_p
								| culoare_p == Blue = "B" ++ show orientare_p
								| culoare_p == Gray = "G" ++ show orientare_p
	show (C culoare_c) 
					| culoare_c == Red = "r"
					| culoare_c == Blue = "b"
					| culoare_c == Gray = "g"
	show (S orientare_s) = show orientare_s

{-
    *** TODO ***

    Un nivel al jocului.

    Recomandăm Data.Map.Strict.
-}
data Level = L (M.Map Position [Object]) 
    deriving (Eq, Ord)

{-
    *** TODO ***

    Reprezetarea textuală a unui nivel.
-}
max_LC :: [Position] -> Position
max_LC poziti = (maximum (map fst poziti) + 1, maximum (map snd poziti) + 1)

poziti :: Position -> Int -> Int -> [Position]
poziti lc linie coloana = [(x, y) | x <- [linie..(fst lc - 1)], y <- [coloana..(snd lc - 1)]]

spati :: Position -> Position -> String 
spati pozitie max 
				| (fst pozitie + 1, snd pozitie + 1) == max = "   "
				| snd pozitie + 1 == snd max = "   \n"											
				| otherwise = "   |"

obiecte :: Position -> Position -> Object -> Object -> String
obiecte pozitie max patrat cerc_sageata
									| (fst pozitie + 1, snd pozitie + 1) == max = show patrat ++ show cerc_sageata
									| snd pozitie + 1 == snd max = show patrat ++ show cerc_sageata ++ "\n"
									| otherwise = show patrat ++ show cerc_sageata ++ "|"  

obiect :: Position -> Position -> Object -> String
obiect pozitie max (P c o)
						| (fst pozitie + 1, snd pozitie + 1) == max = show (P c o) ++ " "
						| snd pozitie + 1 == snd max = show (P c o) ++ " \n"
						| otherwise = show (P c o) ++ " |"  
obiect pozitie max (C c)
						| (fst pozitie + 1, snd pozitie + 1) == max = "  " ++ show (C c)
						| snd pozitie + 1 == snd max = "  " ++ show (C c) ++ "\n"
						| otherwise = "  " ++ show (C c) ++ "|"
obiect pozitie max (S o)
						| (fst pozitie + 1, snd pozitie + 1) == max = "  " ++ show (S o)
						| snd pozitie + 1 == snd max = "  " ++ show (S o) ++ "\n"
						| otherwise = "  " ++ show (S o) ++ "|"
 
instance Show Level where
    show (L nivel) = foldl (++) [] (map afisare (poziti (max_LC (M.keys nivel)) (minimum (map fst (M.keys nivel))) (minimum (map snd (M.keys nivel)))))
					where
						afisare pozitie = if(M.notMember pozitie nivel) then
												spati pozitie (max_LC (M.keys nivel)) 
										  else if(length (nivel M.! pozitie) == 2) then
												obiecte pozitie (max_LC (M.keys nivel)) (head (nivel M.! pozitie)) (last (nivel M.! pozitie))
										  else 
												obiect pozitie (max_LC (M.keys nivel)) (head (nivel M.! pozitie))		
	
{-
    *** TODO ***

    Nivelul vid, fără obiecte.
-}
emptyLevel :: Level
emptyLevel = (L M.empty)

{-
    *** TODO ***

    Adaugă un pătrat cu caracteristicile date la poziția precizată din nivel.
-}
addSquare :: Color -> Heading -> Position -> Level -> Level
addSquare culoare orientare pozitie (L nivel) 
											| M.notMember pozitie nivel = L (M.insert pozitie [(P culoare orientare)] nivel)
											| otherwise = L (M.insertWith (++) pozitie [(P culoare orientare)] nivel)

{-
    *** TODO ***

    Adaugă un cerc cu caracteristicile date la poziția precizată din nivel.
-}
addCircle :: Color -> Position -> Level -> Level 
addCircle culoare pozitie (L nivel) = L (M.insert pozitie [(C culoare)] nivel)

{-
    *** TODO ***

    Adaugă o săgeată cu caracteristicile date la poziția precizată din nivel.
-}
addArrow :: Heading -> Position -> Level -> Level
addArrow orientare pozitie (L nivel) = L (M.insert pozitie [(S orientare)] nivel)

{-
    *** TODO ***

    Mută pătratul de la poziția precizată din nivel. Dacă la poziția respectivă
    nu se găsește un pătrat, întoarce direct parametrul.
-}
muta :: Object -> Position -> Level -> Level
muta (C c) pozitie (L nivel) = (L nivel)
muta (S o) pozitie (L nivel) = (L nivel)
muta (P c o) pozitie (L nivel) = muta_patrate (P c o) o pozitie (L nivel)

muta_patrate :: Object -> Heading -> Position -> Level -> Level
muta_patrate (C c) op pozitie (L nivel) = (L nivel)
muta_patrate (S o) op pozitie (L nivel) = (L nivel)
muta_patrate (P c o) op pozitie (L nivel) = if(M.notMember (muta_pozitie op pozitie) nivel) then
									muta_patrat (P c o) (muta_pozitie op pozitie) (elimina pozitie (L nivel))
								 else
									muta_patrat (P c o) (muta_pozitie op pozitie) (elimina pozitie (muta_patrate (head (nivel M.! (muta_pozitie op pozitie))) op (muta_pozitie op pozitie) (L nivel)))
		
muta_patrat :: Object -> Position -> Level -> Level
muta_patrat (P c o) pozitie (L nivel) = if(M.notMember pozitie nivel) then
											addSquare c o pozitie (L nivel)
										else 
											addSquare c (orientare o (head (nivel M.! pozitie))) pozitie (L nivel)

orientare :: Heading -> Object -> Heading 
orientare op (C c) = op
orientare op (S o) = o

muta_pozitie :: Heading -> Position -> Position
muta_pozitie North pozitie = ((fst pozitie) - 1, snd pozitie)
muta_pozitie South pozitie = ((fst pozitie) + 1, snd pozitie)
muta_pozitie East pozitie = (fst pozitie , (snd pozitie) + 1)
muta_pozitie West pozitie = (fst pozitie , (snd pozitie) - 1)

elimina :: Position -> Level -> Level
elimina pozitie (L nivel) = if(length (nivel M.! pozitie) == 1) then
								L (M.delete pozitie nivel)
							else 
								L (M.adjust tail pozitie nivel)

move :: Position  -- Poziția
     -> Level     -- Nivelul iniția
     -> Level     -- Nivelul final
move pozitie (L nivel) = if(M.notMember pozitie nivel) then 
								L nivel
						   else 
								muta (head (nivel M.! pozitie)) pozitie (L nivel) 
					  
{-
    *** TODO ***

    Instanțiați clasa `ProblemState` pentru jocul nostru.
-}
verifica_obiecte :: Object -> Object -> Bool
verifica_obiecte (P c1 o1) (P c2 o2) = False
verifica_obiecte (P c1 o1) (C c2) = if(c1 == c2) then
										True
									else
										False
verifica_obiecte (P c1 o1) (S o2) = False
verifica_obiecte (C c1) (C c2) = True
verifica_obiecte (S o1) (S o2) = True

instance ProblemState Level Position where
    successors (L nivel) = map succesori (filter patrat (M.keys nivel))
							where
								patrat pozitie = if(length (nivel M.! pozitie) == 2) then 
													True
												 else if(verifica_obiecte (head (nivel M.! pozitie)) (last (nivel M.! pozitie)) == False) then
													True
												 else 
													False
								succesori pozitie = (pozitie, move pozitie (L nivel)) 

    isGoal (L nivel) = (M.foldl verifica True nivel)
						where
							verifica adevar obiecte = adevar && (verifica_obiecte (head obiecte) (last obiecte)) 
    -- Doar petru BONUS
    heuristic (L nivel) = foldl (+) 0 (map distanta (filter patrat (M.keys nivel)))
							where
								patrat pozitie = if(length (nivel M.! pozitie) == 2) then 
													True
												 else if(verifica_obiecte (head (nivel M.! pozitie)) (last (nivel M.! pozitie)) == False) then
													True
												 else 
													False
								cerc p1 p2 = verifica_obiecte (head (nivel M.! p1)) (last (nivel M.! p2)) 
								p pozitie = head (filter (cerc pozitie) (M.keys nivel))
								distanta pozitie = abs ((fst pozitie) - (fst (p pozitie))) + abs ((snd pozitie) - (snd (p pozitie)))
