{-
  Trabalho Prático: Processamento de Texto com Haskell
  Autora: Luisa Caetano Araujo

  Este programa realiza análise de frequência de palavras em textos,
  processando cada parágrafo separadamente.
-}

module Main where

import System.Environment (getArgs)
import Data.Char (toLower, isAlpha)
import Data.List (sortBy, groupBy)
import qualified Data.Set as Set

-- =============================================================================
-- TIPOS DE DADOS
-- =============================================================================

-- | Par (palavra, frequência). Definir um tipo próprio deixa o código mais legível.
type WordFreq = (String, Int)

-- | Um parágrafo é uma lista de linhas
type Paragraph = [String]

-- =============================================================================
-- STOPWORDS
-- =============================================================================

-- | Conjunto de stopwords em português (artigos, preposições, conjunções, etc.).
-- Usamos Set, e não lista, porque Set.member é uma busca muito mais rápida —
-- feita uma vez para cada palavra do texto. Já normalizadas (sem acento, minúsculas).
stopwords :: Set.Set String
stopwords = Set.fromList
  [ -- Artigos
    "o", "a", "os", "as", "um", "uma", "uns", "umas"
  , -- Preposições
    "de", "da", "do", "das", "dos", "em", "na", "no", "nas", "nos"
  , "por", "para", "com", "sem", "sob", "sobre", "entre", "ate", "apos"
  , "perante", "contra", "desde", "durante", "mediante", "segundo"
  , -- Contrações
    "ao", "aos", "pelo", "pela", "pelos", "pelas", "num", "numa"
  , "nuns", "numas", "dum", "duma", "duns", "dumas", "nesse", "nessa"
  , "nesses", "nessas", "neste", "nesta", "nestes", "nestas", "naquele"
  , "naquela", "naqueles", "naquelas", "naquilo", "desse", "dessa"
  , "desses", "dessas", "deste", "desta", "destes", "destas", "daquele"
  , "daquela", "daqueles", "daquelas", "daquilo"
  , -- Conjunções
    "e", "ou", "mas", "porem", "contudo", "todavia", "entretanto"
  , "que", "se", "como", "porque", "pois", "logo", "portanto"
  , "quando", "enquanto", "embora", "embora", "caso", "nem"
  , -- Pronomes
    "eu", "tu", "ele", "ela", "nos", "vos", "eles", "elas"
  , "me", "te", "se", "lhe", "lhes", "mim", "ti", "si"
  , "este", "esta", "estes", "estas", "isto"
  , "esse", "essa", "esses", "essas", "isso"
  , "aquele", "aquela", "aqueles", "aquelas", "aquilo"
  , "meu", "minha", "meus", "minhas", "teu", "tua", "teus", "tuas"
  , "seu", "sua", "seus", "suas", "nosso", "nossa", "nossos", "nossas"
  , "vosso", "vossa", "vossos", "vossas"
  , "quem", "qual", "quais", "quanto", "quanta", "quantos", "quantas"
  , "cujo", "cuja", "cujos", "cujas", "onde"
  , -- Advérbios comuns
    "nao", "sim", "muito", "pouco", "mais", "menos", "bem", "mal"
  , "ja", "ainda", "sempre", "nunca", "jamais", "talvez", "apenas"
  , "so", "tambem", "tao", "tanto", "quao", "assim", "aqui", "ali"
  , "la", "ca", "ai", "onde", "aonde", "hoje", "ontem", "amanha"
  , "agora", "antes", "depois", "logo", "cedo", "tarde"
  , -- Verbos auxiliares comuns (formas)
    "ser", "estar", "ter", "haver", "ir", "vir"
  , "sou", "es", "e", "somos", "sao"
  , "era", "eras", "eramos", "eram"
  , "foi", "fomos", "foram"
  , "estou", "esta", "estamos", "estao"
  , "estava", "estavamos", "estavam"
  , "tenho", "tem", "temos", "tinha", "tinham"
  , "ha", "havia", "houve"
  , "vou", "vai", "vamos", "vao"
  , "venho", "vem", "vimos", "vinha", "vinham"
  , -- Outros
    "isso", "isto", "aquilo", "algo", "alguem", "ninguem", "nada"
  , "tudo", "cada", "outro", "outra", "outros", "outras"
  , "mesmo", "mesma", "mesmos", "mesmas", "proprio", "propria"
  , "tal", "tais", "demais", "certo", "certa", "certos", "certas"
  ]

-- =============================================================================
-- NORMALIZAÇÃO DE TEXTO
-- =============================================================================

-- | Remove o acento de um caractere, convertendo para o equivalente ASCII.
-- Cada vogal acentuada vira sua forma simples; o caso final (_ -> toLower c)
-- passa o restante para minúscula. Assim "Ação", "ação" e "ACAO" viram "acao".
removeAccent :: Char -> Char
removeAccent c = case c of
  -- Vogais minúsculas com acentos
  'á' -> 'a'; 'à' -> 'a'; 'â' -> 'a'; 'ã' -> 'a'; 'ä' -> 'a'
  'é' -> 'e'; 'è' -> 'e'; 'ê' -> 'e'; 'ë' -> 'e'
  'í' -> 'i'; 'ì' -> 'i'; 'î' -> 'i'; 'ï' -> 'i'
  'ó' -> 'o'; 'ò' -> 'o'; 'ô' -> 'o'; 'õ' -> 'o'; 'ö' -> 'o'
  'ú' -> 'u'; 'ù' -> 'u'; 'û' -> 'u'; 'ü' -> 'u'
  'ç' -> 'c'
  'ñ' -> 'n'
  -- Vogais maiúsculas com acentos
  'Á' -> 'a'; 'À' -> 'a'; 'Â' -> 'a'; 'Ã' -> 'a'; 'Ä' -> 'a'
  'É' -> 'e'; 'È' -> 'e'; 'Ê' -> 'e'; 'Ë' -> 'e'
  'Í' -> 'i'; 'Ì' -> 'i'; 'Î' -> 'i'; 'Ï' -> 'i'
  'Ó' -> 'o'; 'Ò' -> 'o'; 'Ô' -> 'o'; 'Õ' -> 'o'; 'Ö' -> 'o'
  'Ú' -> 'u'; 'Ù' -> 'u'; 'Û' -> 'u'; 'Ü' -> 'u'
  'Ç' -> 'c'
  'Ñ' -> 'n'
  -- Caractere sem acento: converte para minúscula
  _   -> toLower c

-- | Verifica se um caractere é uma letra (incluindo acentuadas)
isLetter :: Char -> Bool
isLetter c = isAlpha c || c `elem` "áàâãäéèêëíìîïóòôõöúùûüçñÁÀÂÃÄÉÈÊËÍÌÎÏÓÒÔÕÖÚÙÛÜÇÑ"

-- | Normaliza uma palavra: remove acentos e converte para minúsculas
normalizeWord :: String -> String
normalizeWord = map removeAccent

-- | Remove caracteres que não são letras de uma string
removeNonLetters :: String -> String
removeNonLetters = filter isLetter

-- | Processa uma palavra compondo duas funções com o operador ".":
-- primeiro remove o que não é letra, depois normaliza acentos e capitalização.
-- A composição lê-se da direita para a esquerda.
processWord :: String -> String
processWord = normalizeWord . removeNonLetters

-- =============================================================================
-- PROCESSAMENTO DE PARÁGRAFOS
-- =============================================================================

-- | Divide o texto em parágrafos (blocos separados por linhas em branco)
splitParagraphs :: String -> [Paragraph]
splitParagraphs text = filter (not . null) $ splitOn isBlankLine (lines text)
  where
    isBlankLine :: String -> Bool
    isBlankLine = all (`elem` " \t\r")

-- | Divide uma lista em sublistas usando um predicado como separador
splitOn :: (a -> Bool) -> [a] -> [[a]]
splitOn _ [] = []
splitOn p xs =
  let (chunk, rest) = break p xs
      remaining = dropWhile p rest
  in if null chunk
     then splitOn p remaining
     else chunk : splitOn p remaining

-- | Extrai todas as palavras de um parágrafo
extractWords :: Paragraph -> [String]
extractWords = concatMap words

-- | Filtra as stopwords de uma lista de palavras.
-- Usa a função de alta ordem "filter", que recebe outra função (a função
-- anônima \w -> ...) e mantém apenas os elementos que satisfazem a condição.
filterStopwords :: [String] -> [String]
filterStopwords = filter (\w -> not (Set.member w stopwords) && not (null w))

-- | Conta a frequência de cada palavra em uma lista
countFrequencies :: [String] -> [WordFreq]
countFrequencies ws = map toFreqPair grouped
  where
    sorted = sortBy compare ws
    grouped = groupBy (==) sorted
    -- toFreqPair usa casamento de padrão (pattern matching) em vez de "if":
    -- (x:xs) casa uma lista separando cabeça x e cauda xs; [] casa a lista vazia.
    toFreqPair (x:xs) = (x, 1 + length xs)
    toFreqPair []     = ("", 0)  -- Caso impossível, mas necessário para completude

-- | Ordena pares (palavra, frequência) por frequência decrescente
-- Em caso de empate, ordena alfabeticamente
sortByFrequency :: [WordFreq] -> [WordFreq]
sortByFrequency = sortBy compareFreq
  where
    compareFreq (w1, f1) (w2, f2) =
      case compare f2 f1 of  -- Frequência decrescente
        EQ -> compare w1 w2  -- Alfabético em caso de empate
        x  -> x

-- | Processa um parágrafo e retorna lista de (palavra, frequência)
processParagraph :: Paragraph -> [WordFreq]
processParagraph paragraph =
  let rawWords = extractWords paragraph           -- Extrai palavras
      processedWords = map processWord rawWords   -- Normaliza cada palavra
      filteredWords = filterStopwords processedWords -- Remove stopwords
      frequencies = countFrequencies filteredWords    -- Conta frequências
  in sortByFrequency frequencies                      -- Ordena por frequência

-- =============================================================================
-- FORMATAÇÃO DA SAÍDA
-- =============================================================================

-- | Formata um par (palavra, frequência) para exibição
formatWordFreq :: WordFreq -> String
formatWordFreq (word, freq) = "(\"" ++ word ++ "\"," ++ show freq ++ ")"

-- | Formata a saída de um parágrafo
formatParagraphOutput :: Int -> [WordFreq] -> String
formatParagraphOutput n freqs =
  "Parágrafo " ++ show n ++ ":\n" ++
  unlines (map formatWordFreq freqs)

-- | Formata a saída completa do programa
formatOutput :: [[WordFreq]] -> String
formatOutput paragraphFreqs =
  unlines $ zipWith formatParagraphOutput [1..] paragraphFreqs

-- =============================================================================
-- FUNÇÃO PRINCIPAL (IO)
-- =============================================================================

-- | Função principal. O tipo "IO ()" indica que ela interage com o mundo externo
-- (lê arquivo, imprime na tela). Apenas a main é impura: toda a lógica de
-- processamento fica em funções puras, mantendo IO e lógica bem separados.
main :: IO ()
main = do
  args <- getArgs
  case args of
    [filename] -> do
      content <- readFile filename                   -- IO: lê o arquivo
      let paragraphs = splitParagraphs content
          results = map processParagraph paragraphs  -- lógica pura
      putStr $ formatOutput results                  -- IO: imprime o resultado
    _ -> putStrLn "Uso: ./frequencia <arquivo_de_texto>\nExemplo: ./frequencia texto.txt"
