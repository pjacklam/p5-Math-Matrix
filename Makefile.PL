#!/usr/local/ls6/perl/bin/perl
#                              -*- Mode: Perl -*- 
# Makefile.PL -- 
# ITIID           : $ITI$ $Header $__Header$
# Author          : Ulrich Pfeifer
# Created On      : Fri Oct 27 11:19:11 1995
# Last Modified By: Ulrich Pfeifer
# Last Modified On: Fri Oct 27 11:36:25 1995
# Language        : Perl
# Update Count    : 7
# Status          : Unknown, Use with caution!
# 
# (C) Copyright 1995, Universität Dortmund, all rights reserved.
# 
# $Locker$
# $Log$
# 


use ExtUtils::MakeMaker;
use Config;

$Version = '0.1';

&WriteMakefile(
               'NAME'     => 'Math::Matrix',
#               'DISTNAME' => 'Matrix',
               'LINKTYPE' => '$(INST_PM)',,
               'VERSION'  => $Version,
               'dist'     => { SUFFIX => 'gz', COMPRESS => 'gzip -f' },
               'clean' 	  => {'FILES' => '*~'},
);
