# gpkex-english
# GPKEX for English - Genetically programmed keyphrase extraction for English

iba Lab, the University of Tokyo, Japan
                          Copyright (c) 2021,Ray Oshikawa

Customized from : https://github.com/TakeLab/gpkex
    Reference : https://aclanthology.org/W13-2407/
Our paper : Prepareing...

## environment
All of the programs are written in Haskell programming language. They have been tested and used on a macOS 11.5.2 operating system, using stack version 2.7.3 (ghc version 8.10.7).

## usage
```
stack build
stack exec gpkex-english-exe
```
change Config_template.hs to Config.hs and write your file path.

## dataset

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

