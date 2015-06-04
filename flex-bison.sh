#! /bin/sh
flex -l scan.l &&\
bison -y -d gram.y &&\
gcc y.tab.c lex.yy.c &&\
rm -f lex.yy.c && rm -f y.tab.c && rm -f y.tab.h
