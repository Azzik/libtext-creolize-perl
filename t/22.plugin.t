#!/usr/bin/perl
use strict;
use warnings;
use Test::Base;
use Text::Creolize;

delimiters qw(.test .sect);
plan tests => 1 * blocks;

filters {
    input => [qw(test_creolize)],
};

run_is 'input' => 'expected';

sub test_creolize {
    my $plugin = PluginTitle->new;
    return Text::Creolize->new(plugin_visitor => $plugin)->convert(@_)->result;
}

package PluginTitle;

sub new { bless {title => "TestPage"}, shift }

sub visit_plugin {
    my($self, $data) = @_;
    if ($data eq 'title') {
        $self->{'text'} = $self->{'title'};
    }
    return $self;
}

__END__

.test plugin visitor
.sect input
<< title >>
.sect expected
<p>TestPage</p>

