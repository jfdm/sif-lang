Sif: A Specification Language for Pattern Languages
===================================================

                                      ____  _  __       _                      
                                     / ___|(_)/ _|     | |    __ _ _ __   __ _ 
                                     \___ \| | |_ _____| |   / _` | '_ \ / _` |
                                      ___) | |  _|_____| |__| (_| | | | | (_| |
                                     |____/|_|_|       |_____\__,_|_| |_|\__, |
                                                                         |___/ 
                                                                               

## What is Sif-Lang

Named after the Norse Goddess Sif who was married to Thor and
associated with fertility, family and wedlock. Sif-lang is provided as
part of the doctoral research by Jan de Muijnck-Hughes. In his
research he looks to investigate how to facilitate the uptake of novel
cryptographic schemes through the use of pattern languages.

Sif is a _Domain Specific Language_ (DSL) for the specification of
software system pattern languages. This repository contains the
specification for sif-lang, and a simple model checker/translation
tool written in Haskell.

## The Sif Runtime

Supplied alongside with the sif specification is the Sif
_Runtime_. The runtime is actually a model checking and translation
tool. The model checker checks a Sif file for both syntaxtic and
semantic correctness. The translation tool translate a Sif file to an
output language. Currently only the Dot language is supported.

## Repository Contents

+ __Examples__
    + Various examples of existing pattern languages can be found in
      the examples folder.
+ __Grammar__
    + The eBNF grammer can be found in the grammar folder. Both type
      set and antlr4 version of the grammar are provided. The Antlr4
      grammar will be the authoritative version.
+ __Support__
    + External tooling support will be detailing in the support
      folder. Support is currently given for:
        1. The Listings LaTeX package
        2. The Emacs text editor.
        3. Snippets for the Yasnippet Emacs package.
+ __src__
    + This folder contains the sif model checker.
+ __Branch Information__
    + Sif-lang has been built upon a GIT repository. The GIT
      repository allows us to track the development of
      language. Different branches in the repository reflect the
      different states that the language can be in. There are two
      primary branches.
        + __Master__: The _Master_ branch contains finalised versions
          of the language.
        + __Development__: The _dev_ branch contains a version of the
          language that has yet to be finalised.
        + The remaining branches will either represent: feature
          development, hotfixes, or the staging process.

## References

