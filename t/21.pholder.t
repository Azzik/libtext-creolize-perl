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
    return Text::Creolize->new->convert(@_)->result;
}

__END__

.test Place holder one line
.sect input
<<<hoge>>>
.sect expected
<p><span class="placeholder">hoge</span></p>

.test Place holder few lines
.sect input
<<<hoge
fuga>>>
.sect expected
<p><span class="placeholder">hoge
fuga</span></p>

.test Place holder escape xml
.sect input
<<< <div class="hoge">hoge&amp;</div> >>>
.sect expected
<p><span class="placeholder"> &lt;div class=&quot;hoge&quot;&gt;hoge&amp;amp;&lt;/div&gt; </span></p>

