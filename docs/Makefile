
TOP = ..
LIB = ../lib
POD2HTML = perl -I ~/dev/perl/podpeople/lib \
		~/dev/perl/podpeople/bin/pod2coolhtml

dist: all

all: readme htmls

readme: ../README

../README : ${LIB}/HTML/Stream.pm
	pod2readme -t $< > ../README

htmls: ${LIB}/HTML/Stream.pm
	( ${POD2HTML} -R ${LIB} -I HTML -O . )

