# gpkex-english
# GPKEX for English - Genetically programmed keyphrase extraction for English

iba Lab, the University of Tokyo, Japan
                          Copyright (c) 2021,Ray Oshikawa

Customized from : https://github.com/TakeLab/gpkex
    Reference : https://aclanthology.org/W13-2407/
Our paper : Prepareing...

## environment
All of the programs are written in Haskell programming language. They have been tested and used on a macOS 11.5.2 operating system, using stack version 2.7.3 (lts-13.11,ghc-8.6.3).

## usage
```
stack build
stack exec gpkex-english-exe
```
change Config_template to Config and write your file path.

## dataset
In the document directory, each file contains a **tokenized** sentence.
In the keyword directory, each file contains a keyphrase, each line has one keyphrase.

### memo
imput for runGP: (runGPファイルへの入力)
  file in inDirP is as follows:
  ```
  [(["apple","watch"],Candidate {cTF = 1.0, cIDF = 2.0, cTFIDF = 1.0 , cFirst = 1.0, cLast = 1.0, cF=1.0, cS=1.0, cT=1.0, cNumber=1.0, cRare=1.0, cTitle=1.0,  cOrig = "hello"}),(["apple"],Candidate {cTF = 1.0, cIDF = 2.0, cTFIDF = 1.0 , cFirst = 1.0, cLast = 1.0, cF=1.0, cS=1.0, cT=1.0, cNumber=1.0, cRare=1.0, cTitle=1.0,  cOrig = "hello"})]
  ```
  file in inDirK is as follows:
  ```
  [["apple"],["apple","watch"]]
  ```


## paper
For details, please refer to the paper:

Marko Bekavac and Jan Šnajder (2013). GPKEX: Genetically Programmed Keyphrase Extraction from
Croatian Texts. Proceedings of the 4th Biennial International Workshop on Balto-Slavic Natural
Language Processing (BNLP 2013), Sofia, ACL, 2013.

                                http://takelab.fer.hr/data/gpkex/

If you use GPKEX or the associated datasets, please cite the paper. The BibTeX citation is:

@InProceedings{bekavac2013gpkex,
  title={GPKEX: Genetically Programmed Keyphrase Extraction from Croatian Texts},
  author={Bekavac, Marko and {\v S}najder, Jan},
  booktitle={4th Biennial International Workshop on Balto-Slavic Natural Language Processing},
  year={2013},
  pages={in press}
 }

