-- Prática 04 de Haskell
-- Nome: Giordana Camilla Camargo

-- 1 - A vacinação contra COVID19 no Brasil está acontecendo por grupos prioritários. As equipes responsáveis pelas ações de vacinação devem registrar em um sistema cada dose de vacina aplicada, classificando cada indivíduo em um dos grupos previstos. No caso de idosos, este grupo prioritário é organizado em 5 faixas etárias: de 60 a 64 anos, de 65 a 69 anos. de 70 a 74 anos, de 75 a 79 anos e de 80 anos ou mais. No sistema, essas faixas são identificadas, respectivamente, pelas siglas "IDO64", "IDO69", "IDO74", "IDO79" e "IDO80". Sabendo disso, crie uma função faixaIdoso :: Int -> String que receba uma idade e retorne o código da faixa correspondente. Caso a idade não se enquadre em nenhuma das faixas do grupo prioritário, o código será "ND" (não definido).

faixaIdoso :: Int -> String
faixaIdoso x = if x>=60 && x<65 then "IDO64" 
          else if x>=65 && x<70 then "IDO69" 
          else if x>=70 && x<75 then "IDO74"
          else if x>=75 && x<80 then "IDO79"
          else if x>=80 then "IDO80"
          else "ND"


-- 2 - Usando list comprehension, crie uma função classifIdosos :: [(String,Int)] -> [(String,Int,String)] que receba uma lista de tuplas contendo nome e idade, e retorne uma lista de tuplas com nome, idade e o código da faixa correspondente (faixaIdoso).

classifIdosos :: [(String,Int)] -> [(String,Int,String)]
classifIdosos xss = [(fst x, snd x, faixaIdoso(snd x)) | x<-xss]


-- 3 - Resolva o exercício anterior com função de alta ordem, sem usar list comprehension. O novo nome da função será classifIdosos'.

--classifIdosos' :: [(String,Int)] -> [(String,Int,String)]
--classifIdosos' x = map [\x -> fst x, snd x, faixaIdoso(snd x)]x

-- 4 - Suponha que uma cor seja representada por uma tupla (Int,Int,Int), contendo valores (R=red,G=Green,B=blue). Considerando isso, crie uma função strColor :: (Int,Int,Int) -> String que receba uma tupla representando uma cor (R=red,G=Green,B=blue) e retorne uma string no formato "rgb(R,G,B)".

strColor :: (Int,Int,Int) -> String
strColor x = "rbg"++ show x

-- 5 - Suponha que um círculo seja representado por uma tupla (Int,Int,Int), contendo respectivamente as coordenadas x e y de seu centro, seguidas de seu raio. Considerando isso, crie uma função genCircs :: Int -> (Int,Int) -> Int -> [(Int,Int,Int)] que receba um número N, um ponto (cx,cy) e um raio R, e gere uma sequência de N círculos de raio R alinhados horizontalmente com um primeiro círculo centrado em (cx,cy). Você pode decidir qual será a distância entre eles.

genCircs :: Int -> (Int,Int) -> Int -> [(Int,Int,Int)]
genCircs numcircs tup raio = [(x,y,r) | x <- take numcircs[fst tup, fst tup+10..], y <- [(snd tup)], r <- [raio]]

-- 6 - Suponha novamente que uma cor seja representada por uma tupla (Int,Int,Int), contendo valores (R=red,G=Green,B=blue). Sabendo disso, crie uma função genReds :: Int -> [(Int,Int.Int)] que receba um número N e gere uma lista com N tons de vermelho calculados (não enumere cada um dos valores literalmente com números "hard-coded"). Você pode repetir valores, se desejar.

genReds :: Int -> [(Int, Int, Int)]
genReds numReds = [(r,b,g) | r <- take numReds[1, 10..256], b<-[0], g<-[0]]

