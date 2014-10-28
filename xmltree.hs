module XMLTree where

{-- Atributo del XML --}
data Attr = Attr String String deriving (Eq, Ord)
instance Show Attr where
    show (Attr k v) = k++"=\""++v++"\""

{-- Condicion para las busquedas --}
data Cond = Cond (String, (String -> Bool))

{-- Tag del XML --}
data Tag = Tag String deriving (Eq, Ord)
instance Show Tag where
    show (Tag tag) = tag

{-- Nodo del XML --}
data XMLTree = XMLTree Tag [Attr] XMLSub
instance Show XMLTree where
    show t = showXMLTree t 0
instance Eq XMLTree where
    (XMLTree tag1 attrs1 sub1) == (XMLTree tag2 attrs2 sub2) = tag1 == tag2 && (length attrs1) == (length attrs2) && (all (\a -> elem a attrs2) attrs1) && sub1 == sub2
instance Ord XMLTree where
    (XMLTree tag1 attrs1 sub1) <= (XMLTree tag2 attrs2 sub2)
        | subComp /= EQ = subComp == LT
        | tagComp /= EQ = tagComp == LT
        | lenComp /= EQ = lenComp == LT
        | otherwise = leAttrs attrs1 attrs2
        where
            subComp = compare sub1 sub2
            tagComp = compare tag1 tag2
            lenComp = compare (length attrs1) (length attrs2)

{- Metodo show auxiliar para mostrar cada nodo con tabulaciones -}
showXMLTree (XMLTree tag attrs sub) n = "<"++(show tag)++""++(foldr (\att acc -> acc++" "++(show att)) "" attrs)++">"++(showXMLSub sub (n+1))++"</"++(show tag)++">"
{- Metodo auxiliar para comparar por el maximo listas de atributos de misma longitud -}
leAttrs [] [] = True
leAttrs attrs1 attrs2 = matt1 < matt2 || (matt1 == matt2 && leAttrs (filter (\a -> a/=matt1) attrs1) (filter (\a -> a/=matt2) attrs2))
    where
        matt1 = maximum attrs1
        matt2 = maximum attrs2

{-- Subarbol de un nodo, puede ser texto o lista de nodos --}
data XMLSub = XMLTrees [XMLTree]
                |XMLText String
instance Show XMLSub where
    show (XMLTrees []) = ""
    show (XMLTrees trees) = showXMLSub (XMLTrees trees) 0
    show (XMLText txt) = txt
instance Eq XMLSub where
    (XMLText txt1) == (XMLText txt2) = txt1 == txt2
    (XMLTrees trees1) == (XMLTrees trees2) = (length trees1) == (length trees2) && (all (\(n1, n2) -> n1 == n2) (zip trees1 trees2))
instance Ord XMLSub where
    (XMLText txt1) <= (XMLText txt2) = txt1 <= txt2
    (XMLText _) <= (XMLTrees _) = False
    (XMLTrees _) <= (XMLText _) = True
    (XMLTrees (t1:trees1)) <= (XMLTrees (t2:trees2)) = compTree == LT || (compTree == EQ && trees1 <= trees2)
        where
            compTree = compare t1 t2

{- Metodo show auxiliar para mostrar cada subarbol tabulado -}
showXMLSub (XMLTrees []) _ = ""
showXMLSub (XMLTrees trees) n = (foldr (\tree acc -> "\n"++(indent n)++(showXMLTree tree n)++acc) ("\n"++(indent (n-1))) trees)
showXMLSub (XMLText txt) _ = txt


{-- Metodo para indentar --}
indent n = (take (2*n) (repeat ' '))


{-- Tagged --}
quicksort []     = []
quicksort (p:xs) = (quicksort (filter (< p) xs)) ++ [p] ++ (quicksort (filter (>= p) xs))

tagged tag (XMLTree t attrs sub) = quicksort (taggedAux tag (XMLTree t attrs sub))

taggedAux tag (XMLTree t attrs (XMLText txt))
    | t==(Tag tag)  = [(XMLTree t attrs (XMLText txt))]
    | otherwise     = []
taggedAux tag (XMLTree t attrs (XMLTrees trees))
    | t==(Tag tag)  = ((XMLTree t attrs (XMLTrees trees)):sub)
    | otherwise     = sub
    where
        sub = foldr (\t1 l -> l++(taggedAux tag t1)) [] trees


{-- Search --}
search tag (k, f) ts (XMLTree xtag attrs (XMLText txt))
    | ts==[] && xtag==(Tag tag) && (k=="" || (any (\(Attr a v) -> k==a && (f v)) attrs)) = [(XMLTree xtag attrs (XMLText txt))]
    | otherwise = []
    where
        c = (Cond (k, f))
search tag (k, f) ts (XMLTree xtag attrs (XMLTrees trees))
    | xtag==(Tag tag) && (k=="" || (any (\(Attr a v) -> k==a && (f v)) attrs))
        && (ts==[] || (any (\(XMLTree tg _ (XMLText txt)) -> (any (\(t, s) -> tg==(Tag t) && (isSubstring s txt))) ts)) trees) = (XMLTree xtag attrs (XMLTrees trees)):sub
    | otherwise = sub
    where
        c = (Cond (k, f))
        sub = foldr (\t1 l -> l++(search tag (k, f) ts t1)) [] trees

{- Metodos auxiliares para saber si un string es substring de otro -}
startsWith [] _ = True
startsWith _ [] = False
startsWith (s1:sub) (s2:str)
    | s1 == s2 = startsWith sub str
    | otherwise = False
isSubstring [] _ = True
isSubstring _ [] = False
isSubstring sub (s:str)
    | startsWith sub (s:str) = True
    | otherwise = isSubstring sub str


{-- Read --}
readXMLTree = fst.readXMLAux.skipWhite

{- Read de un nodo del arbol -}
readXMLAux ('<':str) = ((XMLTree tag attrs sub), s)
    where
        (tag,s1) = readXMLTag str
        (l,r)    = splitWhile (/= '>') s1
        attrs    = readXMLAttrs (skipWhite l)
        (sub,s2) = readXMLSub (skipWhite (tail r))
        s        = skipWhite (tail (snd (splitWhile (/= '>') s2)))

{- Read de un Tag -}
readXMLTag str = ((Tag tag), s)
    where
        (tag,s) = (splitWhile (\x -> x /= ' ' && x /= '>') (skipWhite str))

{- Read de Atributos -}
readXMLAttrs [] = []
readXMLAttrs str = (attr:(filter (\(Attr k2 v2) -> k2/=k)attrs))
    where
        (k,r) = splitWhile (\x -> x/=' ' && x/='=') (skipWhite str)
        (v,s) = splitWhile (/= '\"') (tail (snd (splitWhile (/= '\"') r)))
        attrs = (readXMLAttrs (skipWhite (tail s)))
        attr  = (Attr k v)

{- Read de listas de arboles -}
readXMLTrees [] = ((XMLTrees []), [])
readXMLTrees ('<':'/':str) = (XMLTrees [], ("</"++str))
readXMLTrees ('<':str) = (XMLTrees (t:sub), s)
    where
        (t,r)               = readXMLAux ('<':str)
        (XMLTrees sub,s)    = readXMLTrees r

{- Read de un subarbol -}
readXMLSub ('<':'/':str) = (XMLTrees [], ("</"++str))
readXMLSub ('<':str) = readXMLTrees ('<':str)
readXMLSub str = (XMLText txt, s)
    where
        (txt,s) = splitWhile (/= '<') str

{- Metodo auxiliar para separar un string en dos mediante una condicion -}
splitWhile cnd1 str = ((takeWhile cnd1 str), (dropWhile cnd1 str))

{- Metodo auxiliar para saltar blancos -}
skipWhite = snd.(splitWhile (\x -> x==' '||x=='\n'||x=='\t'))


