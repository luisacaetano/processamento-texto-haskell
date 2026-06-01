================================================================================
        PROCESSAMENTO DE TEXTO COM HASKELL
        Trabalho Prático - Paradigmas de Linguagens (Sem1, 2026)
================================================================================

DESCRIÇÃO
---------
Este programa realiza análise de frequência de palavras em arquivos de texto.
Para cada parágrafo do arquivo, o programa exibe uma lista de pares
(palavra, frequência), ordenados por frequência decrescente.

REQUISITOS
----------
- GHC (Glasgow Haskell Compiler) versão 8.0 ou superior
- Biblioteca padrão do Haskell (Data.Set, Data.List, Data.Char, etc.)

================================================================================
                        INSTALAÇÃO DO GHC
================================================================================

Windows (PowerShell como Administrador):
  choco install ghc

Linux:
  sudo apt install ghc

macOS:
  brew install ghc

================================================================================
                           COMPILAÇÃO
================================================================================

Windows (PowerShell ou CMD):
  cd "C:\caminho\para\o\projeto"
  ghc -o frequencia Main.hs

  Isso gera: frequencia.exe

Linux / macOS (Terminal):
  cd /caminho/para/o/projeto
  ghc -o frequencia Main.hs

  Isso gera: frequencia

================================================================================
                            EXECUÇÃO
================================================================================

Windows:
  .\frequencia.exe texto.txt

Linux / macOS:
  ./frequencia texto.txt

================================================================================
                         EXEMPLO DE USO
================================================================================

1. Crie ou edite um arquivo de texto (ex: meutexto.txt)

2. Compile o programa:
   ghc -o frequencia Main.hs

3. Execute:
   - Windows: .\frequencia.exe meutexto.txt
   - Linux/macOS: ./frequencia meutexto.txt

4. O programa exibirá a frequência de palavras por parágrafo

FORMATO DE ENTRADA
------------------
- O arquivo deve ser um arquivo de texto simples (.txt)
- Parágrafos são separados por uma ou mais linhas em branco
- Codificação UTF-8 é suportada (acentos funcionam corretamente)

FORMATO DE SAÍDA
----------------
Para cada parágrafo, o programa exibe:
    Parágrafo N:
    ("palavra1",frequência1)
    ("palavra2",frequência2)
    ...

As palavras são ordenadas por frequência decrescente.
Em caso de empate, a ordenação é alfabética.

================================================================================
                        FUNCIONALIDADES
================================================================================

- Normalização de maiúsculas/minúsculas (Ação = ação = AÇÃO)
- Remoção de acentos (ação → acao)
- Remoção de pontuação e símbolos
- Filtragem de stopwords (artigos, preposições, conjunções, etc.)
- Processamento por parágrafo independente

================================================================================
                      ESTRUTURA DO CÓDIGO
================================================================================

Main.hs contém:

1. TIPOS DE DADOS
   - WordFreq: tupla (String, Int) para palavra e frequência
   - Paragraph: lista de linhas de texto

2. STOPWORDS
   - Conjunto (Set) de palavras comuns a serem ignoradas
   - Inclui artigos, preposições, conjunções e pronomes

3. FUNÇÕES DE NORMALIZAÇÃO
   - removeAccent: converte caracteres acentuados para ASCII
   - normalizeWord: aplica remoção de acentos e lowercase
   - processWord: remove pontuação e normaliza

4. FUNÇÕES DE PROCESSAMENTO
   - splitParagraphs: divide texto em parágrafos
   - extractWords: extrai palavras de um parágrafo
   - filterStopwords: remove stopwords da lista
   - countFrequencies: conta ocorrências de cada palavra
   - sortByFrequency: ordena por frequência decrescente
   - processParagraph: pipeline completo de processamento

5. FUNÇÕES DE FORMATAÇÃO
   - formatWordFreq: formata par (palavra, frequência)
   - formatParagraphOutput: formata saída de um parágrafo
   - formatOutput: formata saída completa

6. FUNÇÃO MAIN (IO)
   - Lê argumentos da linha de comando
   - Lê arquivo de texto
   - Processa e exibe resultados

================================================================================
                     RESOLUÇÃO DE PROBLEMAS
================================================================================

ERRO: "ghc: command not found"
  - O GHC não está instalado ou não está no PATH
  - Reinstale o GHC e reinicie o terminal

ERRO: Caracteres estranhos na saída
  - Windows: execute "chcp 65001" antes de rodar o programa
  - Verifique se o arquivo de entrada está em UTF-8

ERRO: "texto.txt: openFile: does not exist"
  - Verifique se o arquivo existe e se o caminho está correto

================================================================================

AUTORA
------
Luisa Caetano Araujo
