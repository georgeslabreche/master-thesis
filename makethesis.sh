#!/bin/bash
rm *.bbl
rm *.blg

pdflatex thesis.tex

bibtex marsenv.aux
bibtex power.aux
bibtex other.aux

pdflatex thesis.tex
