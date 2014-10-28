import XMLTree
str = "<llibres><llibre any=\"2004\" edicio=\"1\"><titol>Razonando con Haskell</titol><autor>Blas C. Ruiz</autor><autor>Francisco Gutierrez</autor><autor>Pablo Guerrero</autor><autor>Jose E. Gallardo</autor></llibre><llibre edicio=\"2\" any=\"1999\"><titol>HASKELL: The Craft of Functional Programming</titol><editor>A. D. McGettrick</editor><autor>Simon Thompson</autor></llibre><llibre edicio=\"1\" any=\"2000\"><titol>Programming language pragmatics</titol><autor>Michael L. Scott</autor></llibre></llibres>"
ejemplo1 = "< llibres>\n <llibre \n any = \" 2001 \" edicio =\" \n 1\">\n \n \n <titol> \n Razonando con Haskell </titol>\n <autor>Blas C. Ruiz</autor>\n <autor>Francisco Gutierrez\n</autor>\n <autor>Pablo Guerrero</autor>\n <autor>Jose E. Gallardo</autor>\n </llibre>\n <llibre edicio=\"2\" any= \"1999\">\n < titol>HASKELL: The Craft of Functional Programming</titol>\n <editor>A. D. McGettrick</editor>\n <autor>Simon Thompson</autor>\n </llibre>\n < llibre edicio=\"1\" any=\"2000\">\n <titol>Programming language pragmatics</titol>\n <autor>Michael L. Scott</autor>\n </ llibre>\n</llibres >"
ejemplo2 = "<llibres>\n <llibre any=\"2004\" edicio=\"1\">\n <titol>Razonando con Haskell</titol>\n <autor>Blas C. Ruiz</autor>\n <autor>Francisco Gutierrez</autor>\n <autor>Pablo Guerrero</autor>\n <autor>Jose E. Gallardo</autor>\n </llibre>\n <llibre edicio=\"2\" any=\"1999\">\n <titol>HASKELL: The Craft of Functional Programming</titol>\n <editor>A. D. McGettrick</editor>\n <autor>Simon Thompson</autor>\n </llibre>\n <llibre edicio=\"1\" any=\"2000\">\n <titol>Programming language pragmatics</titol>\n <autor>Michael L. Scott</autor>\n </llibre>\n</llibres>"
ejemplo3 = "<llibres>\n <llibre any=\"2004\" edicio=\"1\">\n <titol>Razonando con Haskell</titol>\n <autor>Blas C. Ruiz</autor>\n <autor>Francisco Gutierrez</autor>\n <autor>Pablo Guerrero</autor>\n <autor>Jose E. Gallardo</autor>\n </llibre>\n <llibre edicio=\"2\" any=\"1999\">\n <titol>HASKELL: The Craft of Functional Programming</titol>\n <editor>A. D. McGettrick</editor>\n <autor>Simon Thompson</autor>\n <autor>Francisco Alvano</autor>\n </llibre>\n <llibre edicio=\"1\" any=\"2000\">\n <titol>Programming language pragmatics</titol>\n <autor>Michael L. Scott</autor>\n </llibre>\n</llibres>"
ejemplo4 = "<llibres>\n <llibre any=\"2004\" edicio=\"1\">\n <titol>Razonando con Haskell</titol>\n <autor>Blas C. Ruiz</autor>\n <autor>Francisco Gutierrez</autor>\n <autor>Pablo Guerrero</autor>\n <autor>Jose E. Gallardo</autor>\n </llibre>\n <llibre edicio=\"2\" any=\"1999\">\n <titol>HASKELL: The Craft of Functional Programming</titol>\n <editor>A. D. McGettrick</editor>\n <autor>Simon Thompson</autor>\n </llibre>\n <llibre edicio=\"1\" any=\"2004\">\n <titol>Programming language pragmatics</titol>\n <autor>Michael L. Scott</autor>\n </llibre>\n</llibres>"
ejemplo5 = "<test0 name=\"paco\" age=\"20\"><test2 gender=\"male\">Hola k ase</test2></test0>"
ejemplo6 = "<test1 name=\"paco\" age=\"20\"><test2 gender=\"male\">Hola k ase</test2></test1>"



main = putStr ("TEST READ\n" ++ (testRead) ++ "\nTEST TAGGED" ++ (testTagged) ++ "\nTEST SEARCH" ++ (testSearch) ++ "\nTEST ORD" ++ (testOrd) ++ "\n")

testRead = (show  (readXMLTree str)) ++ "\n" ++
    (show  (readXMLTree ejemplo1)) ++ "\n" ++
    (show  (readXMLTree ejemplo2)) ++ "\n" ++
    (show  (readXMLTree ejemplo3)) ++ "\n" ++
    (show  (readXMLTree ejemplo4)) ++ "\n" ++
    (show  (readXMLTree ejemplo5)) ++ "\n" ++
    (show  (readXMLTree ejemplo6)) ++ "\n"

testTagged = (show (tagged "autor" (readXMLTree str)))

testSearch = (show (search "llibre" ("edicio",\x->x>="1") [("titol", "Program")] (readXMLTree str)))

testOrd = "\n" ++ (show ((readXMLTree ejemplo5) < (readXMLTree ejemplo6)))
