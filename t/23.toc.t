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
    return Text::Creolize->new({toc => 1})->convert(@_)->result;
}

__END__

.test toc support
.sect input
=hoge
==fuga
.sect expected
<div class="toc">
<ul>
<li><a href="#h1ql0vgg">hoge</a>
<ul>
<li><a href="#h0sfk0tb">fuga</a></li>
</ul>
</li>
</ul>
</div>
<h1 id="h1ql0vgg">hoge</h1>
<h2 id="h0sfk0tb">fuga</h2>

