#!/usr/bin/perl
use ExtUtils::MakeMaker;

#------------------------------------------------------------
# Configuration:
#------------------------------------------------------------

my @EXES = <bin/html2perlstream>;     # executables
my @MODS = <HTML/*.pm>;               # modules

#------------------------------------------------------------
# Distribution stuff:
#------------------------------------------------------------

# Figure out the stuff we need to do to build a distribution:
my @DIST_PREOPS;
push @DIST_PREOPS, map {
    s/\.pm$//;
    my $out = $_; $out =~ s{/}{-}g;
    "pod2html2 --infile=$_.pm \\\n\t\t --outfile=docs/$out.html ;"
} @MODS;
push @DIST_PREOPS, map {
    my $out = $_; $out =~ s{^[^/]+/}{};
    "pod2html2 --infile=$_ \\\n\t\t --outfile=docs/$out.html ;"
} @EXES;
$DIST_PREOP .= join("\\\n\t", @DIST_PREOPS) . " echo done";

#------------------------------------------------------------
# Makefile:
#------------------------------------------------------------

# Write the Makefile:
WriteMakefile(
	      NAME         => 'HTML::Stream',
	      VERSION_FROM => "HTML/Stream.pm",
	      DISTNAME     => "HTML-Stream",
	      EXE_FILES    => [@EXES],
	      'dist'       => {
		  PREOP    => $DIST_PREOP,
		  COMPRESS => 'gzip',
		  SUFFIX   => 'gz',
	      }
	      );

