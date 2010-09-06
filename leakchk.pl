#!/usr/bin/perl
use strict;
use warnings;
use Devel::Leak;
use Text::Creolize;

my $handle;

{
    Text::Creolize->new->convert("=H\n[[a]]");
}

my $then = Devel::Leak::NoteSV($handle);

{
    Text::Creolize->new->convert("=H\n[[a]]");
}

my $now = Devel::Leak::CheckSV($handle);
warn "then:$then; now:$now; ", $now - $then, " objects remain.";

