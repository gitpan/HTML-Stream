#!/usr/bin/perl
use ExtUtils::MakeMaker;

# Write the Makefile:
WriteMakefile(
	      NAME         => 'HTML::Stream',
	      VERSION_FROM => "lib/HTML/Stream.pm",
	      DISTNAME     => "HTML-Stream",
	      'dist'       => {
		  PREOP    => 'cvu_perl_preop -m HTML::Stream -f',
		  COMPRESS => 'gzip',
		  SUFFIX   => 'gz',
	      }
	      );



