{-# OPTIONS_GHC -Wall #-}

module Search where

import ProblemState

import qualified Data.Set as S
import qualified Data.List as L
import qualified Data.Maybe as M

{-
    *** TODO ***

    Tipul unui nod utilizat în procesul de căutare. Recomandăm reținerea unor
    informații legate de:

    * stare;
    * acțiunea care a condus la această stare;
    * nodul părinte, prin explorarea căruia a fost obținut nodul curent;
    * adâncime.
-}
data Node s a = N { stare :: s, actiune :: Maybe a, parinte :: Node s a, adancime :: Int } | NodeNil
	deriving (Eq, Show)

{-
    *** TODO ***

    Întoarce starea stocată într-un nod.
-}
nodeState :: Node s a -> s
nodeState (N stare actiune parinte adancime) = stare 

{-
    *** TODO ***

    Întoarce lista nodurilor rezultate prin parcurgerea limitată în adâncime
    a spațiului stărilor, pornind de la starea dată ca parametru.

    Pentru reținerea stărilor vizitate, recomandăm Data.Set. Constrângerea
    `Ord s` permite utilizarea tipului `Set`.

    În afara BONUS-ului, puteți ignora parametrul boolean. Pentru BONUS, puteți
    sorta lista succesorilor folosind `sortBy` din Data.List.
-}
sortare :: (ProblemState s a) => (a, s) -> (a, s) -> Ordering
sortare s1 s2 = compare (heuristic (snd s1)) (heuristic (snd s2)) 

verifica :: (Ord s) => Node s a -> S.Set s -> Int -> Bool
verifica (N stare actiune parinte adancime) vizitat a = if((S.notMember stare vizitat) && (adancime <= a)) then
															True
														else
															False

adauga :: (Ord s) => Node s a -> S.Set s -> S.Set s
adauga (N stare actiune parinte adancime) vizitat = (S.insert stare vizitat)

nod :: Node s a -> Int -> (a, s) -> Node s a 
nod parinte adancime (actiune, stare) = (N stare (Just actiune) parinte adancime)

noduri_succesoare :: (ProblemState s a) => Node s a -> Int -> Bool -> [Node s a]
noduri_succesoare (N stare actiune parinte adancime) a bonus = if(adancime == a) then
																[] 
														 	   else 
																map (nod (N stare actiune parinte adancime) (adancime + 1)) succesori
																	where
																	succesori = if(bonus) then
																					L.sortBy sortare (successors stare)
																				else
																					successors stare																					

dfs :: (ProblemState s a, Ord s) => [Node s a] -> [Node s a] -> S.Set s -> Int -> Bool -> [Node s a] 
dfs parcurgere noduri vizitat adancime bonus = if(null parcurgere) then 
												noduri 
										 	   else if(verifica (head parcurgere) vizitat adancime) then
												dfs ((noduri_succesoare (head parcurgere) adancime bonus) ++ (tail parcurgere)) (noduri ++ [(head parcurgere)]) (adauga (head parcurgere) vizitat) adancime bonus
										 	   else
												dfs (tail parcurgere) noduri vizitat adancime bonus   


limitedDfs :: (ProblemState s a, Ord s)
           => s           -- Starea inițială
           -> Bool        -- Pentru BONUS, `True` dacă utilizăm euristica
           -> Int         -- Adâncimea maximă de explorare
           -> [Node s a]  -- Lissta de noduri
limitedDfs stare bonus adancime = dfs [(N stare Nothing NodeNil 0)] [] S.empty adancime bonus
 
{-
    *** TODO ***

    Explorează în adâncime spațiul stărilor, utilizând adâncire iterativă,
    pentru determinarea primei stări finale întâlnite.

    Întoarce o perche între nodul cu prima stare finală întâlnită și numărul
    de stări nefinale vizitate până în acel moment.

    În afara BONUS-ului, puteți ignora parametrul boolean.
-}
deepening :: (ProblemState s a, Ord s) => s -> Bool -> Int -> Int -> (Node s a, Int)  
deepening stare bonus nivel vizitat = if(index == Nothing) then
									deepening stare bonus (nivel + 1) (vizitat + length (limitedDfs stare bonus nivel))
							  else
									((limitedDfs stare bonus nivel) L.!! (M.fromJust index), vizitat + (M.fromJust index))
									where
										index = L.elemIndex True (map (isGoal . nodeState) (limitedDfs stare bonus nivel))

iterativeDeepening :: (ProblemState s a, Ord s)
    => s                -- Starea inițială
    -> Bool             -- Pentru BONUS, `True` dacă utilizăm euristica
    -> (Node s a, Int)  -- (Nod cu prima stare finală,
                        --  număr de stări nefinale vizitate)
iterativeDeepening stare bonus = deepening stare bonus 0 0
									
{-
    *** TODO ***

    Pornind de la un nod, reface calea către nodul inițial, urmând legăturile
    către părinți.

    Întoarce o listă de perechi (acțiune, stare), care se încheie în starea
    finală, dar care EXCLUDE starea inițială.
-}
cale :: Node s a -> [(a, s)]
cale (N stare Nothing parinte adancime) = []
cale (N stare actiune parinte adancime) = (extractPath parinte) ++ [(M.fromJust actiune, stare)] 

extractPath :: Node s a -> [(a, s)]
extractPath (N stare actiune parinte adancime) = cale (N stare actiune parinte adancime)

{-
    Poate fi utilizată pentru afișarea fiecărui element al unei liste
    pe o linie separată.
-}
printSpacedList :: Show a => [a] -> IO ()
printSpacedList = mapM_ (\a -> print a >> putStrLn (replicate 20 '*'))
