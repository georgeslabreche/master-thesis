#!/bin/bash
pdflatex thesis.tex
bibtex own.aux
bibtex sota.aux
bibtex marsenv.aux
bibtex moonenv.aux
bibtex power.aux
pdflatex thesis.tex
