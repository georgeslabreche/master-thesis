#!/bin/bash
rm *.bbl
rm *.blg

pdflatex thesis.tex

bibtex marsenv.aux
bibtex power.aux
bibtex other.aux
bibtex datasheet.aux

pdflatex thesis.tex
