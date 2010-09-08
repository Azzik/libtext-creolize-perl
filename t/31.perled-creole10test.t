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
    my($wiki_source) = @_;
    my $creolize = Text::Creolize->new(type => 'perl');
    my $perl = $creolize->convert($wiki_source)->result;
    return (eval $perl)->($creolize);
}

# from http://www.wikicreole.org/wiki/Creole1.0TestCases

__END__

.test Creole1.0TestCases
.sect input
= Top-level heading (1 1)
== This a test for creole 0.1 (1 2)
=== This is a Subheading (1 3)
==== Subsub (1 4)
===== Subsubsub (1 5)

The ending equal signs should not be displayed:

= Top-level heading (2 1) =
== This a test for creole 0.1 (2 2) ==
=== This is a Subheading (2 3) ===
==== Subsub (2 4) ====
===== Subsubsub (2 5) =====


You can make things **bold** or //italic// or **//both//** or //**both**//.

Character formatting extends across line breaks: **bold,
this is still bold. This line deliberately does not end in star-star.

Not bold. Character formatting does not cross paragraph boundaries.

You can use [[internal links]] or [[http://www.wikicreole.org|external links]],
give the link a [[internal links|different]] name.

Here's another sentence: This wisdom is taken from [[Ward Cunningham's]]
[[http://www.c2.com/doc/wikisym/WikiSym2006.pdf|Presentation at the Wikisym 06]].

Here's a external link without a description: [[http://www.wikicreole.org]]

Be careful that italic links are rendered properly:  //[[http://my.book.example/|My Book Title]]// 

Free links without braces should be rendered as well, like http://www.wikicreole.org/ and http://www.wikicreole.org/users/~example. 

Creole1.0 specifies that http://bar and ftp://bar should not render italic,
something like foo://bar should render as italic.

You can use this to draw a line to separate the page:
----

You can use lists, start it at the first column for now, please...

unnumbered lists are like
* item a
* item b
* **bold item c**

blank space is also permitted before lists like:
  *   item a
 * item b
* item c
 ** item c.a

or you can number them
# [[item 1]]
# item 2
# // italic item 3 //
    ## item 3.1
  ## item 3.2

up to five levels
* 1
** 2
*** 3
**** 4
***** 5

* You can have
multiline list items
* this is a second multiline
list item

You can use nowiki syntax if you would like do stuff like this:

{{{
Guitar Chord C:

||---|---|---|
||-0-|---|---|
||---|---|---|
||---|-0-|---|
||---|---|-0-|
||---|---|---|
}}}

You can also use it inline nowiki {{{ in a sentence }}} like this.

= Escapes =
Normal Link: http://wikicreole.org/ - now same link, but escaped: ~http://wikicreole.org/ 

Normal asterisks: ~**not bold~**

a tilde alone: ~

a tilde escapes itself: ~~xxx

=== Creole 0.2 ===

This should be a flower with the ALT text "this is a flower" if your wiki supports ALT text on images:

{{Red-Flower.jpg|here is a red flower}}

=== Creole 0.4 ===

Tables are done like this:

|=header col1|=header col2| 
|col1|col2| 
|you         |can         | 
|also        |align\\ it. | 

You can format an address by simply forcing linebreaks:

My contact dates:\\
Pone: xyz\\
Fax: +45\\
Mobile: abc

=== Creole 0.5 ===

|= Header title               |= Another header title     |
| {{{ //not italic text// }}} | {{{ **not bold text** }}} |
| //italic text//             | **  bold text **          |

=== Creole 1.0 ===

If interwiki links are setup in your wiki, this links to the WikiCreole page about Creole 1.0 test cases: [[WikiCreole:Creole1.0TestCases]].
.sect expected
<h1>Top-level heading (1 1)</h1>
<h2>This a test for creole 0.1 (1 2)</h2>
<h3>This is a Subheading (1 3)</h3>
<h4>Subsub (1 4)</h4>
<h5>Subsubsub (1 5)</h5>
<p>The ending equal signs should not be displayed:</p>
<h1>Top-level heading (2 1)</h1>
<h2>This a test for creole 0.1 (2 2)</h2>
<h3>This is a Subheading (2 3)</h3>
<h4>Subsub (2 4)</h4>
<h5>Subsubsub (2 5)</h5>
<p>You can make things <strong>bold</strong> or <em>italic</em> or <strong><em>both</em></strong> or <em><strong>both</strong></em>.</p>
<p>Character formatting extends across line breaks: <strong>bold, this is still bold. This line deliberately does not end in star-star.</strong></p>
<p>Not bold. Character formatting does not cross paragraph boundaries.</p>
<p>You can use <a href="http://www.example.net/wiki/internal%20links">internal links</a> or <a href="http://www.wikicreole.org">external links</a>, give the link a <a href="http://www.example.net/wiki/internal%20links">different</a> name.</p>
<p>Here&#39;s another sentence: This wisdom is taken from <a href="http://www.example.net/wiki/Ward%20Cunningham%27s">Ward Cunningham&#39;s</a> <a href="http://www.c2.com/doc/wikisym/WikiSym2006.pdf">Presentation at the Wikisym 06</a>.</p>
<p>Here&#39;s a external link without a description: <a href="http://www.wikicreole.org">http://www.wikicreole.org</a></p>
<p>Be careful that italic links are rendered properly: <em><a href="http://my.book.example/">My Book Title</a></em></p>
<p>Free links without braces should be rendered as well, like <a href="http://www.wikicreole.org/">http://www.wikicreole.org/</a> and <a href="http://www.wikicreole.org/users/~example">http://www.wikicreole.org/users/~example</a>.</p>
<p>Creole1.0 specifies that <a href="http://bar">http://bar</a> and <a href="ftp://bar">ftp://bar</a> should not render italic, something like foo:<em>bar should render as italic.</em></p>
<p>You can use this to draw a line to separate the page:</p>
<hr />
<p>You can use lists, start it at the first column for now, please...</p>
<p>unnumbered lists are like</p>
<ul>
<li>item a</li>
<li>item b</li>
<li><strong>bold item c</strong></li>
</ul>
<p>blank space is also permitted before lists like:</p>
<ul>
<li>item a</li>
<li>item b</li>
<li>item c
<ul>
<li>item c.a</li>
</ul>
</li>
</ul>
<p>or you can number them</p>
<ol>
<li><a href="http://www.example.net/wiki/item%201">item 1</a></li>
<li>item 2</li>
<li><em>italic item 3</em>
<ol>
<li>item 3.1</li>
<li>item 3.2</li>
</ol>
</li>
</ol>
<p>up to five levels</p>
<ul>
<li>1
<ul>
<li>2
<ul>
<li>3
<ul>
<li>4
<ul>
<li>5</li>
</ul>
</li>
</ul>
</li>
</ul>
</li>
</ul>
</li>
</ul>
<ul>
<li>You can have multiline list items</li>
<li>this is a second multiline list item</li>
</ul>
<p>You can use nowiki syntax if you would like do stuff like this:</p>
<pre>Guitar Chord C:

||---|---|---|
||-0-|---|---|
||---|---|---|
||---|-0-|---|
||---|---|-0-|
||---|---|---|</pre>
<p>You can also use it inline nowiki <code>in a sentence</code> like this.</p>
<h1>Escapes</h1>
<p>Normal Link: <a href="http://wikicreole.org/">http://wikicreole.org/</a> - now same link, but escaped: http://wikicreole.org/</p>
<p>Normal asterisks: **not bold**</p>
<p>a tilde alone: ~</p>
<p>a tilde escapes itself: ~xxx</p>
<h3>Creole 0.2</h3>
<p>This should be a flower with the ALT text &quot;this is a flower&quot; if your wiki supports ALT text on images:</p>
<p><img src="http://www.example.net/static/Red-Flower.jpg" alt="here is a red flower" /></p>
<h3>Creole 0.4</h3>
<p>Tables are done like this:</p>
<table>
<tr><th>header col1</th><th>header col2</th></tr>
<tr><td>col1</td><td>col2</td></tr>
<tr><td>you</td><td>can</td></tr>
<tr><td>also</td><td>align<br />
it.</td></tr>
</table>
<p>You can format an address by simply forcing linebreaks:</p>
<p>My contact dates:<br />
Pone: xyz<br />
Fax: +45<br />
Mobile: abc</p>
<h3>Creole 0.5</h3>
<table>
<tr><th>Header title</th><th>Another header title</th></tr>
<tr><td><code>//not italic text//</code></td><td><code>**not bold text**</code></td></tr>
<tr><td><em>italic text</em></td><td><strong>bold text</strong></td></tr>
</table>
<h3>Creole 1.0</h3>
<p>If interwiki links are setup in your wiki, this links to the <a href="http://www.example.net/wiki/WikiCreole">WikiCreole</a> page about Creole 1.0 test cases: <a href="http://www.example.net/wiki/WikiCreole:Creole1.0TestCases">WikiCreole:Creole1.0TestCases</a>.</p>

