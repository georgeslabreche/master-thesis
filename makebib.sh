#!/bin/bash
rm *.bbl
rm *.blg
bibtex marsenv.aux
bibtex power.aux
bibtex other.aux
