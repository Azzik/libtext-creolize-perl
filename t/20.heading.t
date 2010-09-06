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

.test Headings in Creole 1.0 Test
.sect input
= Top-level heading (1)
== This a test for creole 0.1 (2)
=== This is a Subheading (3)
==== Subsub (4)
===== Subsubsub (5)
.sect expected
<h1>Top-level heading (1)</h1>
<h2>This a test for creole 0.1 (2)</h2>
<h3>This is a Subheading (3)</h3>
<h4>Subsub (4)</h4>
<h5>Subsubsub (5)</h5>

.test End equals in Creole 1.0 Test
.sect input
= Top-level heading (1) =
== This a test for creole 0.1 (2) ==
=== This is a Subheading (3) ===
==== Subsub (4) ====
===== Subsubsub (5) =====
.sect expected
<h1>Top-level heading (1)</h1>
<h2>This a test for creole 0.1 (2)</h2>
<h3>This is a Subheading (3)</h3>
<h4>Subsub (4)</h4>
<h5>Subsubsub (5)</h5>

.test Level 6 and mores
.sect input
====== Level 6
======= Level 7
======== Level 8
.sect expected
<h6>Level 6</h6>
<h6>Level 7</h6>
<h6>Level 8</h6>

.test End equals more
.sect input
= a2 ==
= a3 ===
= a4 ====
= a5 =====
== b1 =
== b3 ===
== b4 ====
== b5 =====
.sect expected
<h1>a2</h1>
<h1>a3</h1>
<h1>a4</h1>
<h1>a5</h1>
<h2>b1</h2>
<h2>b3</h2>
<h2>b4</h2>
<h2>b5</h2>

.test Left Paddings
.sect input
=A
 =B
= C
 = D
.sect expected
<h1>A</h1>
<h1>B</h1>
<h1>C</h1>
<h1>D</h1>

.test Right Paddings
.sect input
= A=
= B =
= C= 
= D = 
.sect expected
<h1>A</h1>
<h1>B</h1>
<h1>C</h1>
<h1>D</h1>

.test Equal signs in text
.sect input
= a = b =
= = a =
= a = =
.sect expected
<h1>a = b</h1>
<h1>= a</h1>
<h1>a =</h1>

.test Escaped equals
.sect input
= a1 ~=
= a2 ~==
= a3 ~===
= b1 ~= =
= b2 ~== =
= b3 ~=== =
.sect expected
<h1>a1 =</h1>
<h1>a2 ==</h1>
<h1>a3 ===</h1>
<h1>b1 =</h1>
<h1>b2 ==</h1>
<h1>b3 ===</h1>

.test Heading between Paragraphs
.sect input
a b c.
= A
d e f.
.sect expected
<p>a b c.</p>
<h1>A</h1>
<p>d e f.</p>

