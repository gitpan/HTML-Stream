#!/usr/bin/perl
use ExtUtils::MakeMaker;

#------------------------------------------------------------
# Configuration:
#------------------------------------------------------------

my $POD2HTML = "perl -I ~/dev/perl/podpeople/lib ~/dev/perl/podpeople/bin/pod2coolhtml";
my @EXES = <bin/html2perlstream>;     # executables
my @MODS = <lib/HTML/*.pm>;           # modules

#------------------------------------------------------------
# Distribution stuff:
#------------------------------------------------------------

# Figure out the stuff we need to do to build a distribution:
my @DIST_PREOPS;
push @DIST_PREOPS, "$POD2HTML -R lib -I HTML -O docs ;";
push @DIST_PREOPS, "$POD2HTML -i bin/html2perlstream -O docs -n html2perlstream -c docs/HTML/index.chc;";
$DIST_PREOP .= join("\\\n\t", @DIST_PREOPS) . " echo done";

#------------------------------------------------------------
# Makefile:
#------------------------------------------------------------

# Write the Makefile:
WriteMakefile(
	      NAME         => 'HTML::Stream',
	      VERSION_FROM => "lib/HTML/Stream.pm",
	      DISTNAME     => "HTML-Stream",
	      EXE_FILES    => [@EXES],
	      'dist'       => {
		  PREOP    => $DIST_PREOP,
		  COMPRESS => 'gzip',
		  SUFFIX   => 'gz',
	      }
	      );

