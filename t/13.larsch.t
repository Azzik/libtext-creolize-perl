use strict;
use warnings;
use Test::Base;
use Text::Creolize;

delimiters qw(Creole1.0: .sect);
plan tests => 1 * blocks;

filters {
    input => [qw(test_creolize)],
};

run_is 'input' => 'expected';

sub test_creolize {
    return Text::Creolize->new->convert(@_)->result;
}

# this test ported from Daniel Mendler's creole for ruby language.
# http://github.com/larsch/creole/blob/master/test/test_creole.rb

__END__

Creole1.0: Bold can be used inside paragraphs
.sect input
This **is** bold
.sect expected
<p>This <strong>is</strong> bold</p>

Creole1.0: Bolds can be used inside paragraphs
.sect input
This **is** bold and **bold**ish
.sect expected
<p>This <strong>is</strong> bold and <strong>bold</strong>ish</p>

Creole1.0: Bold can be used inside list item
.sect input
* This is **bold**
.sect expected
<ul>
<li>This is <strong>bold</strong></li>
</ul>

Creole1.0: Bold can be used inside table cells
.sect input
|This is **bold**|
.sect expected
<table>
<tr><td>This is <strong>bold</strong></td></tr>
</table>

Creole1.0: Links can appear inside bold text
.sect input
A bold link: **http://wikicreole.org/ nice!**
.sect expected
<p>A bold link: <strong><a href="http://wikicreole.org/">http://wikicreole.org/</a> nice!</strong></p>

Creole1.0: Bold will end at the end of paragraph
.sect input
This **is bold
.sect expected
<p>This <strong>is bold</strong></p>

Creole1.0: Bold will end at the end of list items
.sect input
* Item **bold
* Item normal
.sect expected
<ul>
<li>Item <strong>bold</strong></li>
<li>Item normal</li>
</ul>

Creole1.0: Bold will end at the end of table cells
.sect input
|Item **bold|Another **bold
.sect expected
<table>
<tr><td>Item <strong>bold</strong></td><td>Another <strong>bold</strong></td></tr>
</table>

Creole1.0: Bold should not cross paragraphs
.sect input
This **is

bold** maybe
.sect expected
<p>This <strong>is</strong></p>
<p>bold<strong> maybe</strong></p>

Creole1.0: Bold should be able to cross lines
.sect input
This **is
bold**
.sect expected
<p>This <strong>is
bold</strong></p>

Creole1.0: Italic can be used inside paragraphs
.sect input
This //is// italic
.sect expected
<p>This <em>is</em> italic</p>

Creole1.0: Italics can be used inside paragraphs
.sect input
This //is// italic and //italic//ish
.sect expected
<p>This <em>is</em> italic and <em>italic</em>ish</p>

Creole1.0: Italic can be used inside list items
.sect input
* This is //italic//
.sect expected
<ul>
<li>This is <em>italic</em></li>
</ul>

Creole1.0: Italic can be used inside table cells
.sect input
|This is //italic//|
.sect expected
<table>
<tr><td>This is <em>italic</em></td></tr>
</table>

Creole1.0: Links can appear inside italic text
.sect input
A italic link: //http://wikicreole.org/ nice!//
.sect expected
<p>A italic link: <em><a href="http://wikicreole.org/">http://wikicreole.org/</a> nice!</em></p>

Creole1.0: Italic will end at the end of paragraph
.sect input
This //is italic
.sect expected
<p>This <em>is italic</em></p>

Creole1.0: Italic will end at the end of list items
.sect input
* Item //italic
* Item normal
.sect expected
<ul>
<li>Item <em>italic</em></li>
<li>Item normal</li>
</ul>

Creole1.0: Italic will end at the end of table cells
.sect input
|Item //italic|Another //italic
.sect expected
<table>
<tr><td>Item <em>italic</em></td><td>Another <em>italic</em></td></tr>
</table>

Creole1.0: Italic should not cross paragraphs
.sect input
This //is

italic// maybe
.sect expected
<p>This <em>is</em></p>
<p>italic<em> maybe</em></p>

Creole1.0: Italic should be able to cross lines
.sect input
This //is
italic//
.sect expected
<p>This <em>is
italic</em></p>

Creole1.0: Bold italics
.sect input
**//bold italics//**
.sect expected
<p><strong><em>bold italics</em></strong></p>

Creole1.0: Italics bold
.sect input
//**italics bold**//
.sect expected
<p><em><strong>italics bold</strong></em></p>

Creole1.0: Italics and italics bold
.sect input
//This is **also** good.//
.sect expected
<p><em>This is <strong>also</strong> good.</em></p>

Creole1.0: Only three differed sized levels of heading are required
.sect input
= Heading 1 =
== Heading 2 ==
=== Heading 3 ===
.sect expected
<h1>Heading 1</h1>
<h2>Heading 2</h2>
<h3>Heading 3</h3>

Creole1.0: Optional headings, not specified in creole 1.0
.sect input
==== Heading 4 ====
===== Heading 5 =====
====== Heading 6 ======
.sect expected
<h4>Heading 4</h4>
<h5>Heading 5</h5>
<h6>Heading 6</h6>

Creole1.0: Right-side equal signs are optional
.sect input
=Heading 1
== Heading 2
=== Heading 3
.sect expected
<h1>Heading 1</h1>
<h2>Heading 2</h2>
<h3>Heading 3</h3>

Creole1.0: Right-side equal signs don't need to be balanced
.sect input
=Heading 1 ===
== Heading 2 =
=== Heading 3 ===========
.sect expected
<h1>Heading 1</h1>
<h2>Heading 2</h2>
<h3>Heading 3</h3>

Creole1.0: Whitespace is allowed before the left-side equal signs
.sect input
                            = Heading 1 =
                           == Heading 2 ==
.sect expected
<h1>Heading 1</h1>
<h2>Heading 2</h2>

Creole1.0: Only white-space characters are permitted after the closing equal signs
.sect input
 = Heading 1 =   
  == Heading 2 ==  
.sect expected
<h1>Heading 1</h1>
<h2>Heading 2</h2>

Creole1.0: doesn't specify if text after closing equal signs
.sect input
 == Heading 2 == foo
.sect expected
<h2>Heading 2 == foo</h2>

Creole1.0: Line must start with equal sign
.sect input
foo = Heading 1 =
.sect expected
<p>foo = Heading 1 =</p>

Creole1.0: Links
.sect input
[[link]]
.sect expected
<p><a href="http://www.example.net/wiki/link">link</a></p>

Creole1.0: Links can appear in paragraphs
.sect input
Hello, [[world]]
.sect expected
<p>Hello, <a href="http://www.example.net/wiki/world">world</a></p>

Creole1.0: Named links
.sect input
[[MyBigPage|Go to my page]]
.sect expected
<p><a href="http://www.example.net/wiki/MyBigPage">Go to my page</a></p>

Creole1.0: URLs
.sect input
[[http://www.wikicreole.org/]]
.sect expected
<p><a href="http://www.wikicreole.org/">http://www.wikicreole.org/</a></p>

Creole1.0: Free-standing URL's should be turned into links
.sect input
http://www.wikicreole.org/
.sect expected
<p><a href="http://www.wikicreole.org/">http://www.wikicreole.org/</a></p>

Creole1.0: Single punctuation characters at end should not be part of URLs
.sect input
http://www.wikicreole.org/,

http://www.wikicreole.org/.

http://www.wikicreole.org/?

http://www.wikicreole.org/!

http://www.wikicreole.org/:

http://www.wikicreole.org/;

http://www.wikicreole.org/'

http://www.wikicreole.org/"

.sect expected
<p><a href="http://www.wikicreole.org/">http://www.wikicreole.org/</a>,</p>
<p><a href="http://www.wikicreole.org/">http://www.wikicreole.org/</a>.</p>
<p><a href="http://www.wikicreole.org/">http://www.wikicreole.org/</a>?</p>
<p><a href="http://www.wikicreole.org/">http://www.wikicreole.org/</a>!</p>
<p><a href="http://www.wikicreole.org/">http://www.wikicreole.org/</a>:</p>
<p><a href="http://www.wikicreole.org/">http://www.wikicreole.org/</a>;</p>
<p><a href="http://www.wikicreole.org/">http://www.wikicreole.org/</a>&#39;</p>
<p><a href="http://www.wikicreole.org/">http://www.wikicreole.org/</a>&quot;</p>

Creole1.0: Nameds URLs
.sect input
[[http://www.wikicreole.org/|Visit the WikiCreole website]]
.sect expected
<p><a href="http://www.wikicreole.org/">Visit the WikiCreole website</a></p>

Creole1.0: Parsing markup within a link is optional
.sect input
[[Weird Stuff|**Weird** //Stuff//]]
.sect expected
<p><a href="http://www.example.net/wiki/Weird%20Stuff">**Weird** //Stuff//</a></p>

Creole1.0: Links in bold
.sect input
**[[link]]**
.sect expected
<p><strong><a href="http://www.example.net/wiki/link">link</a></strong></p>

Creole1.0: Whitespace inside brackets should be ignored
.sect input
[[ link ]]

[[ link me ]]

[[  http://dot.com/ |  dot.com ]]

[[  http://dot.com/ |  dot com ]]

.sect expected
<p><a href="http://www.example.net/wiki/link">link</a></p>
<p><a href="http://www.example.net/wiki/link%20me">link me</a></p>
<p><a href="http://dot.com/">dot.com</a></p>
<p><a href="http://dot.com/">dot com</a></p>

Creole1.0: One or more blank lines end paragraphs
.sect input
This is
my text.

This is
more text.


This is
more more text.



This is
more more more text.
.sect expected
<p>This is
my text.</p>
<p>This is
more text.</p>
<p>This is
more more text.</p>
<p>This is
more more more text.</p>

Creole1.0: A list end paragraphs
.sect input
Hello
* Item
.sect expected
<p>Hello</p>
<ul>
<li>Item</li>
</ul>

Creole1.0: A table end paragraphs
.sect input
Hello
|Cell|
.sect expected
<p>Hello</p>
<table>
<tr><td>Cell</td></tr>
</table>

Creole1.0: A nowiki end paragraphs
.sect input
Hello
{{{
nowiki
}}}
.sect expected
<p>Hello</p>
<pre>nowiki</pre>

Creole1.0: A heading ends a paragraph (not specced)
.sect input
Hello
= Heading 1 =
.sect expected
<p>Hello</p>
<h1>Heading 1</h1>

Creole1.0: Wiki-style for line breaks
.sect input
This is the first line,\\and this is the second.
.sect expected
<p>This is the first line,<br />
and this is the second.</p>

Creole1.0: List items begin with a * at the beginning of a line
.sect input
* Item 1
* Item 2
* Item 3
.sect expected
<ul>
<li>Item 1</li>
<li>Item 2</li>
<li>Item 3</li>
</ul>

Creole1.0: Whitespace is optional before and after the *
.sect input
   *    Item 1
*Item 2
    *       Item 3
.sect expected
<ul>
<li>Item 1</li>
<li>Item 2</li>
<li>Item 3</li>
</ul>

Creole1.0: A space is required if if the list element starts with bold text
.sect input
* **Item 1
.sect expected
<ul>
<li><strong>Item 1</strong></li>
</ul>

Creole1.0: An item ends at blank line
.sect input
* Item

Par
.sect expected
<ul>
<li>Item</li>
</ul>
<p>Par</p>

Creole1.0: An item ends at blank line
.sect input
* Item
= Heading 1 =
.sect expected
<ul>
<li>Item</li>
</ul>
<h1>Heading 1</h1>

Creole1.0: An item ends at a table
.sect input
* Item
|Cell|
.sect expected
<ul>
<li>Item</li>
</ul>
<table>
<tr><td>Cell</td></tr>
</table>

Creole1.0: An item ends at a nowiki block
.sect input
* Item
{{{
Code
}}}
.sect expected
<ul>
<li>Item</li>
</ul>
<pre>Code</pre>

Creole1.0: An item can span multiple lines
.sect input
* The quick
brown fox
    jumps over
lazy dog.
*Humpty Dumpty
sat 
on a wall.
.sect expected
<ul>
<li>The quick
brown fox
    jumps over
lazy dog.</li>
<li>Humpty Dumpty
sat 
on a wall.</li>
</ul>

Creole1.0: An item can contain line breaks
.sect input
* The quick brown\\fox jumps over lazy dog.
.sect expected
<ul>
<li>The quick brown<br />
fox jumps over lazy dog.</li>
</ul>

Creole1.0: Nested
.sect input
* Item 1
 **Item 2
 *  Item 3
.sect expected
<ul>
<li>Item 1
<ul>
<li>Item 2</li>
</ul>
</li>
<li>Item 3</li>
</ul>

Creole1.0: Nested up to 5 levels
.sect input
*Item 1
**Item 2
***Item 3
****Item 4
*****Item 5
.sect expected
<ul>
<li>Item 1
<ul>
<li>Item 2
<ul>
<li>Item 3
<ul>
<li>Item 4
<ul>
<li>Item 5</li>
</ul>
</li>
</ul>
</li>
</ul>
</li>
</ul>
</li>
</ul>

Creole1.0: following a list element will be treated as a nested one
.sect input
*Hello,
World!
**Not bold
.sect expected
<ul>
<li>Hello,
World!
<ul>
<li>Not bold</li>
</ul>
</li>
</ul>

Creole1.0: following a list element will be treated as a nested unordered one
.sect input
#Hello,
World!
**Not bold
.sect expected
<ol>
<li>Hello,
World!
<ul>
<li>Not bold</li>
</ul>
</li>
</ol>

Creole1.0: otherwise it will be treated as the beginning of bold text
.sect input
*Hello,
World!

**Bold
.sect expected
<ul>
<li>Hello,
World!</li>
</ul>
<p><strong>Bold</strong></p>

Creole1.0: List items begin with a sharp sign at the beginning of a line
.sect input
# Item 1
# Item 2
# Item 3
.sect expected
<ol>
<li>Item 1</li>
<li>Item 2</li>
<li>Item 3</li>
</ol>

Creole1.0: Whitespace is optional before and after the sharps
.sect input
   #    Item 1
#Item 2
        #               Item 3
.sect expected
<ol>
<li>Item 1</li>
<li>Item 2</li>
<li>Item 3</li>
</ol>

Creole1.0: A space is required if if the list element starts with bold text
.sect input
# **Item 1
.sect expected
<ol>
<li><strong>Item 1</strong></li>
</ol>

Creole1.0: An item ends at blank line
.sect input
# Item

Par
.sect expected
<ol>
<li>Item</li>
</ol>
<p>Par</p>

Creole1.0: An item ends at a heading
.sect input
# Item
= Heading 1 =
.sect expected
<ol>
<li>Item</li>
</ol>
<h1>Heading 1</h1>

Creole1.0: An item ends at a table
.sect input
# Item
|Cell|
.sect expected
<ol>
<li>Item</li>
</ol>
<table>
<tr><td>Cell</td></tr>
</table>

Creole1.0: An item ends at a nowiki block
.sect input
# Item
{{{
Code
}}}
.sect expected
<ol>
<li>Item</li>
</ol>
<pre>Code</pre>

Creole1.0: An item can span multiple lines
.sect input
# The quick
brown fox
    jumps over
lazy dog.
#Humpty Dumpty
sat 
on a wall.
.sect expected
<ol>
<li>The quick
brown fox
    jumps over
lazy dog.</li>
<li>Humpty Dumpty
sat 
on a wall.</li>
</ol>

Creole1.0: An item can contain line breaks
.sect input
# The quick brown\\fox jumps over lazy dog.
.sect expected
<ol>
<li>The quick brown<br />
fox jumps over lazy dog.</li>
</ol>

Creole1.0: Nested
.sect input
# Item 1
 ##Item 2
 #  Item 3
.sect expected
<ol>
<li>Item 1
<ol>
<li>Item 2</li>
</ol>
</li>
<li>Item 3</li>
</ol>

Creole1.0: Nested up to 5 levels
.sect input
#Item 1
##Item 2
###Item 3
####Item 4
#####Item 5
.sect expected
<ol>
<li>Item 1
<ol>
<li>Item 2
<ol>
<li>Item 3
<ol>
<li>Item 4
<ol>
<li>Item 5</li>
</ol>
</li>
</ol>
</li>
</ol>
</li>
</ol>
</li>
</ol>

Creole1.0: following a list element will be treated as a nested one
.sect input
#Hello,
World!
##Not monospace
.sect expected
<ol>
<li>Hello,
World!
<ol>
<li>Not monospace</li>
</ol>
</li>
</ol>

Creole1.0: following a list element will be treated as a nested ordered one
.sect input
*Hello,
World!
##Not bold
.sect expected
<ul>
<li>Hello,
World!
<ol>
<li>Not bold</li>
</ol>
</li>
</ul>

Creole1.0: otherwise it will be treated as the beginning of monospaced text
.sect input
#Hello,
World!

##Monospace
.sect expected
<ol>
<li>Hello,
World!</li>
</ol>
<p><tt>Monospace</tt></p>

Creole1.0: Ambiguity ol following ul
.sect input
*uitem
#oitem
.sect expected
<ul>
<li>uitem</li>
</ul>
<ol>
<li>oitem</li>
</ol>

Creole1.0: Ambiguity ul following ol
.sect input
#oitem
*uitem
.sect expected
<ol>
<li>oitem</li>
</ol>
<ul>
<li>uitem</li>
</ul>

Creole1.0: Ambiguity 2ol following ul
.sect input
*uitem
##oitem
.sect expected
<ul>
<li>uitem
<ol>
<li>oitem</li>
</ol>
</li>
</ul>

Creole1.0: Ambiguity 2ul following ol
.sect input
#uitem
**oitem
.sect expected
<ol>
<li>uitem
<ul>
<li>oitem</li>
</ul>
</li>
</ol>

Creole1.0: Ambiguity 3ol following 3ul
.sect input
***uitem
###oitem
.sect expected
<ul>
<li>uitem</li>
</ul>
<ol>
<li>oitem</li>
</ol>

Creole1.0: Ambiguity 3ul following 3ol
.sect input
###oitem
***uitem
.sect expected
<ol>
<li>oitem</li>
</ol>
<ul>
<li>uitem</li>
</ul>

Creole1.0: Ambiguity ol following 3ol
.sect input
###oitem1
#oitem2
.sect expected
<ol>
<li>oitem1</li>
<li>oitem2</li>
</ol>

Creole1.0: Ambiguity ul following 3ol
.sect input
###oitem
*uitem
.sect expected
<ol>
<li>oitem</li>
</ol>
<ul>
<li>uitem</li>
</ul>

Creole1.0: Ambiguity uncommon URL schemes should not be parsed as URLs
.sect input
This is what can go wrong://this should be an italic text//.
.sect expected
<p>This is what can go wrong:<em>this should be an italic text</em>.</p>

Creole1.0: Ambiguity a link inside italic text
.sect input
How about //a link, like http://example.org, in italic// text?
.sect expected
<p>How about <em>a link, like <a href="http://example.org">http://example.org</a>, in italic</em> text?</p>

Creole1.0: Ambiguity another test from Creole Wiki
.sect input
Formatted fruits, for example://apples//, oranges, **pears** ...

Blablabala (http://blub.de)
.sect expected
<p>Formatted fruits, for example:<em>apples</em>, oranges, <strong>pears</strong> ...</p>
<p>Blablabala (<a href="http://blub.de">http://blub.de</a>)</p>

Creole1.0: Ambiguity Bolds and Lists
.sect input
** bold text **

 ** bold text **
.sect expected
<p><strong> bold text </strong></p>
<p> <strong> bold text </strong></p>

Creole1.0: Verbatim block
.sect input
{{{
Hello
}}}
.sect expected
<pre>Hello</pre>

Creole1.0: Nowiki inline
.sect input
Hello {{{world}}}.
.sect expected
<p>Hello <code>world</code>.</p>

Creole1.0: No wiki markup is interpreted inbetween
.sect input
{{{
**Hello**
}}}
.sect expected
<pre>**Hello**</pre>

Creole1.0: Leading whitespaces are not permitted
.sect input
 {{{
Hello
}}}
.sect expected
<p> <code>
Hello
</code></p>

Creole1.0: Leading whitespaces are not permitted 2
.sect input
{{{
Hello
 }}}
.sect expected
<p><code>
Hello
</code></p>

Creole1.0: Assumed should preserve whitespace
.sect input
{{{
    Hello,  
     World   
}}}
.sect expected
<pre>    Hello,  
     World   </pre>

Creole1.0: In preformatted blocks, one leading space is removed
.sect input
{{{
nowikiblock
 }}}
}}}
.sect expected
<pre>nowikiblock
}}}</pre>

Creole1.0: In inline nowiki, any trailing closing brace is included in the span
.sect input
this is {{{nowiki}}}}

this is {{{nowiki}}}}}

this is {{{nowiki}}}}}}

this is {{{nowiki}}}}}}}
.sect expected
<p>this is <code>nowiki}</code></p>
<p>this is <code>nowiki}}</code></p>
<p>this is <code>nowiki}}}</code></p>
<p>this is <code>nowiki}}}}</code></p>

Creole1.0: Special HTML chars should be escaped
.sect input
<b>not bold</b>
.sect expected
<p>&lt;b&gt;not bold&lt;/b&gt;</p>

Creole1.0: Image tags should be escape
.sect input
{{image.jpg|"tag"}}
.sect expected
<p><img src="http://www.example.net/static/image.jpg" alt="&quot;tag&quot;" /></p>

Creole1.0: Malicious links should not be converted
.sect input
[[javascript:alert("Boo!")|Click]]
.sect expected
<p>[[javascript:alert(&quot;Boo!&quot;)|Click]]</p>

Creole1.0: Escapes
.sect input
~** Not Bold ~** ~// Not Italic ~//
~* Not Bullet
.sect expected
<p>** Not Bold ** // Not Italic //
* Not Bullet</p>

Creole1.0: Escapes following char is not a blank
.sect input
Hello ~ world
Hello ~
world
.sect expected
<p>Hello ~ world
Hello ~
world</p>

Creole1.0: Not escaping inside URLs
.sect input
http://example.org/~user/
.sect expected
<p><a href="http://example.org/~user/">http://example.org/~user/</a></p>

Creole1.0: Escaping links
.sect input
~http://example.org/~user/
.sect expected
<p>http://example.org/~user/</p>

Creole1.0: Four hyphens make a horizontal rule
.sect input
----
.sect expected
<hr />

Creole1.0: Whitespaces around hyphens are allowed
.sect input
 ----
----  
  ----  
.sect expected
<hr />
<hr />
<hr />

Creole1.0: Nothing else than hyphens and whitespace is allowed
.sect input
foo ----

---- foo

  -- --  
.sect expected
<p>foo ----</p>
<p>---- foo</p>
<p>  -- --  </p>

Creole1.0: Tables
.sect input
|Hello, World!|
.sect expected
<table>
<tr><td>Hello, World!</td></tr>
</table>

Creole1.0: Tables multiple columns
.sect input
|c1|c2|c3|
.sect expected
<table>
<tr><td>c1</td><td>c2</td><td>c3</td></tr>
</table>

Creole1.0: Tables multiple rows
.sect input
|c11|c12|
|c21|c22|
.sect expected
<table>
<tr><td>c11</td><td>c12</td></tr>
<tr><td>c21</td><td>c22</td></tr>
</table>

Creole1.0: Tables end pipe is optional
.sect input
|c1|c2|c3
.sect expected
<table>
<tr><td>c1</td><td>c2</td><td>c3</td></tr>
</table>

Creole1.0: Tables empty cells
.sect input
|c1||c3
.sect expected
<table>
<tr><td>c1</td><td></td><td>c3</td></tr>
</table>

Creole1.0: Tables escaping cell separator
.sect input
|c1~|c2|c3
.sect expected
<table>
<tr><td>c1|c2</td><td>c3</td></tr>
</table>

Creole1.0: Tables escape in last cell + empty cell
.sect input
|c1|c2~|
|c1|c2~||
|c1|c2~|||
.sect expected
<table>
<tr><td>c1</td><td>c2|</td></tr>
<tr><td>c1</td><td>c2|</td></tr>
<tr><td>c1</td><td>c2|</td><td></td></tr>
</table>

Creole1.0: Tables equal sign after pipe make a header
.sect input
|=Header|
.sect expected
<table>
<tr><th>Header</th></tr>
</table>

Creole1.0: Tables pipes in links or images
.sect input
|c1|[[Link|Link text]]|{{Image|Image text}}|
.sect expected
<table>
<tr><td>c1</td><td><a href="http://www.example.net/wiki/Link">Link text</a></td><td><img src="http://www.example.net/static/Image" alt="Image text" /></td></tr>
</table>

Creole1.0: Tables followed by heading
.sect input
|table|
=heading 1=

|table|

=heading 2=
.sect expected
<table>
<tr><td>table</td></tr>
</table>
<h1>heading 1</h1>
<table>
<tr><td>table</td></tr>
</table>
<h1>heading 2</h1>

Creole1.0: Tables followed by paragraph
.sect input
|table|
par

|table|

par
.sect expected
<table>
<tr><td>table</td></tr>
</table>
<p>par</p>
<table>
<tr><td>table</td></tr>
</table>
<p>par</p>

Creole1.0: Tables followed by unordered list
.sect input
|table|
*item
.sect expected
<table>
<tr><td>table</td></tr>
</table>
<ul>
<li>item</li>
</ul>

Creole1.0: Tables followed by ordered list
.sect input
|table|
#item

|table|

#item
.sect expected
<table>
<tr><td>table</td></tr>
</table>
<ol>
<li>item</li>
</ol>
<table>
<tr><td>table</td></tr>
</table>
<ol>
<li>item</li>
</ol>

Creole1.0: Tables followed by horizontal rule
.sect input
|table|
----

|table|

----
.sect expected
<table>
<tr><td>table</td></tr>
</table>
<hr />
<table>
<tr><td>table</td></tr>
</table>
<hr />

Creole1.0: Tables followed by verbatim
.sect input
|table|
{{{
verbatim
}}}

|table|

{{{
verbatim
}}}
.sect expected
<table>
<tr><td>table</td></tr>
</table>
<pre>verbatim</pre>
<table>
<tr><td>table</td></tr>
</table>
<pre>verbatim</pre>

Creole1.0: Tables followed by table
.sect input
|table|
|table|

|table|
.sect expected
<table>
<tr><td>table</td></tr>
<tr><td>table</td></tr>
</table>
<table>
<tr><td>table</td></tr>
</table>

Creole1.0: Headings followed by headings
.sect input
=heading 1
=heading 2

=heading 3
.sect expected
<h1>heading 1</h1>
<h1>heading 2</h1>
<h1>heading 3</h1>

Creole1.0: Headings followed by paragraphs
.sect input
=heading 1
par

=heading 2

par
.sect expected
<h1>heading 1</h1>
<p>par</p>
<h1>heading 2</h1>
<p>par</p>

Creole1.0: Headings followed by unordered list
.sect input
=heading 1
*item

=heading 2

*item
.sect expected
<h1>heading 1</h1>
<ul>
<li>item</li>
</ul>
<h1>heading 2</h1>
<ul>
<li>item</li>
</ul>

Creole1.0: Headings followed by ordered list
.sect input
=heading 1
#item

=heading 2

#item
.sect expected
<h1>heading 1</h1>
<ol>
<li>item</li>
</ol>
<h1>heading 2</h1>
<ol>
<li>item</li>
</ol>

Creole1.0: Headings followed by horizontal rule
.sect input
=heading 1
----

=heading 2

----
.sect expected
<h1>heading 1</h1>
<hr />
<h1>heading 2</h1>
<hr />

Creole1.0: Headings followed by verbatim
.sect input
=heading 1
{{{
verbatim
}}}

=heading 2

{{{
verbatim
}}}
.sect expected
<h1>heading 1</h1>
<pre>verbatim</pre>
<h1>heading 2</h1>
<pre>verbatim</pre>

Creole1.0: Headings followed by table
.sect input
=heading 1
|cell

=heading 2

|cell
.sect expected
<h1>heading 1</h1>
<table>
<tr><td>cell</td></tr>
</table>
<h1>heading 2</h1>
<table>
<tr><td>cell</td></tr>
</table>

Creole1.0: Paragraphs followed by headings
.sect input
par
=heading 1

par

=heading 2
.sect expected
<p>par</p>
<h1>heading 1</h1>
<p>par</p>
<h1>heading 2</h1>

Creole1.0: Paragraphs followed by paragraphs
.sect input
par
par

par

par
.sect expected
<p>par
par</p>
<p>par</p>
<p>par</p>

Creole1.0: Paragraphs followed by unordered list
.sect input
par
*item

par

*item
.sect expected
<p>par</p>
<ul>
<li>item</li>
</ul>
<p>par</p>
<ul>
<li>item</li>
</ul>

Creole1.0: Paragraphs followed by ordered list
.sect input
par
#item

par

#item
.sect expected
<p>par</p>
<ol>
<li>item</li>
</ol>
<p>par</p>
<ol>
<li>item</li>
</ol>

Creole1.0: Paragraphs followed by horizontal rule
.sect input
par
----

par

----
.sect expected
<p>par</p>
<hr />
<p>par</p>
<hr />

Creole1.0: Paragraphs followed by verbatim
.sect input
par
{{{
verbatim
}}}

par

{{{
verbatim
}}}
.sect expected
<p>par</p>
<pre>verbatim</pre>
<p>par</p>
<pre>verbatim</pre>

Creole1.0: Paragraphs followed by table
.sect input
par
|cell

par

|cell
.sect expected
<p>par</p>
<table>
<tr><td>cell</td></tr>
</table>
<p>par</p>
<table>
<tr><td>cell</td></tr>
</table>

Creole1.0: Unordered list followed by headings
.sect input
*item
=heading 1

*item

=heading 2
.sect expected
<ul>
<li>item</li>
</ul>
<h1>heading 1</h1>
<ul>
<li>item</li>
</ul>
<h1>heading 2</h1>

Creole1.0: Unordered list followed by paragraphs
.sect input
*item
par

*item

par
.sect expected
<ul>
<li>item
par</li>
</ul>
<ul>
<li>item</li>
</ul>
<p>par</p>

Creole1.0: Unordered list followed by unordered list
.sect input
*item
*item

*item

*item
.sect expected
<ul>
<li>item</li>
<li>item</li>
</ul>
<ul>
<li>item</li>
</ul>
<ul>
<li>item</li>
</ul>

Creole1.0: Unordered list followed by ordered list
.sect input
*item
#item

*item

#item
.sect expected
<ul>
<li>item</li>
</ul>
<ol>
<li>item</li>
</ol>
<ul>
<li>item</li>
</ul>
<ol>
<li>item</li>
</ol>

Creole1.0: Unordered list followed by horizontal rule
.sect input
*item
----

*item

----
.sect expected
<ul>
<li>item</li>
</ul>
<hr />
<ul>
<li>item</li>
</ul>
<hr />

Creole1.0: Unordered list followed by verbatim
.sect input
*item
{{{
verbatim
}}}

*item

{{{
verbatim
}}}
.sect expected
<ul>
<li>item</li>
</ul>
<pre>verbatim</pre>
<ul>
<li>item</li>
</ul>
<pre>verbatim</pre>

Creole1.0: Unordered list followed by table
.sect input
*item
|cell

*item

|cell
.sect expected
<ul>
<li>item</li>
</ul>
<table>
<tr><td>cell</td></tr>
</table>
<ul>
<li>item</li>
</ul>
<table>
<tr><td>cell</td></tr>
</table>

Creole1.0: Ordered list followed by headings
.sect input
#item
=heading 1

#item

=heading 2
.sect expected
<ol>
<li>item</li>
</ol>
<h1>heading 1</h1>
<ol>
<li>item</li>
</ol>
<h1>heading 2</h1>

Creole1.0: Ordered list followed by paragraphs
.sect input
#item
par

#item

par
.sect expected
<ol>
<li>item
par</li>
</ol>
<ol>
<li>item</li>
</ol>
<p>par</p>

Creole1.0: Ordered list followed by unordered list
.sect input
#item
*item

#item

*item
.sect expected
<ol>
<li>item</li>
</ol>
<ul>
<li>item</li>
</ul>
<ol>
<li>item</li>
</ol>
<ul>
<li>item</li>
</ul>

Creole1.0: Ordered list followed by ordered list
.sect input
#item
#item

#item

#item
.sect expected
<ol>
<li>item</li>
<li>item</li>
</ol>
<ol>
<li>item</li>
</ol>
<ol>
<li>item</li>
</ol>

Creole1.0: Ordered list followed by horizontal rule
.sect input
#item
----

#item

----
.sect expected
<ol>
<li>item</li>
</ol>
<hr />
<ol>
<li>item</li>
</ol>
<hr />

Creole1.0: Ordered list followed by verbatim
.sect input
#item
{{{
verbatim
}}}

#item

{{{
verbatim
}}}
.sect expected
<ol>
<li>item</li>
</ol>
<pre>verbatim</pre>
<ol>
<li>item</li>
</ol>
<pre>verbatim</pre>

Creole1.0: Ordered list followed by table
.sect input
#item
|cell

#item

|cell
.sect expected
<ol>
<li>item</li>
</ol>
<table>
<tr><td>cell</td></tr>
</table>
<ol>
<li>item</li>
</ol>
<table>
<tr><td>cell</td></tr>
</table>

Creole1.0: Horizontal rules followed by headings
.sect input
----
=heading 1

----

=heading 2
.sect expected
<hr />
<h1>heading 1</h1>
<hr />
<h1>heading 2</h1>

Creole1.0: Horizontal rules followed by paragraphs
.sect input
----
par

----

par
.sect expected
<hr />
<p>par</p>
<hr />
<p>par</p>

Creole1.0: Horizontal rules followed by unordered list
.sect input
----
*item

----

*item
.sect expected
<hr />
<ul>
<li>item</li>
</ul>
<hr />
<ul>
<li>item</li>
</ul>

Creole1.0: Horizontal rules followed by ordered list
.sect input
----
#item

----

#item
.sect expected
<hr />
<ol>
<li>item</li>
</ol>
<hr />
<ol>
<li>item</li>
</ol>

Creole1.0: Horizontal rules followed by horizontal rule
.sect input
----
----

----

----
.sect expected
<hr />
<hr />
<hr />
<hr />

Creole1.0: Horizontal rules followed by verbatim
.sect input
----
{{{
verbatim
}}}

----

{{{
verbatim
}}}
.sect expected
<hr />
<pre>verbatim</pre>
<hr />
<pre>verbatim</pre>

Creole1.0: Horizontal rules followed by table
.sect input
----
|cell

----

|cell
.sect expected
<hr />
<table>
<tr><td>cell</td></tr>
</table>
<hr />
<table>
<tr><td>cell</td></tr>
</table>

Creole1.0: Verbatims followed by headings
.sect input
{{{
verbatim
}}}
=heading 1

{{{
verbatim
}}}

=heading 2
.sect expected
<pre>verbatim</pre>
<h1>heading 1</h1>
<pre>verbatim</pre>
<h1>heading 2</h1>

Creole1.0: Verbatims followed by paragraphs
.sect input
{{{
verbatim
}}}
par

{{{
verbatim
}}}

par
.sect expected
<pre>verbatim</pre>
<p>par</p>
<pre>verbatim</pre>
<p>par</p>

Creole1.0: Verbatims followed by unordered list
.sect input
{{{
verbatim
}}}
*item

{{{
verbatim
}}}

*item
.sect expected
<pre>verbatim</pre>
<ul>
<li>item</li>
</ul>
<pre>verbatim</pre>
<ul>
<li>item</li>
</ul>

Creole1.0: Verbatims followed by ordered list
.sect input
{{{
verbatim
}}}
#item

{{{
verbatim
}}}

#item
.sect expected
<pre>verbatim</pre>
<ol>
<li>item</li>
</ol>
<pre>verbatim</pre>
<ol>
<li>item</li>
</ol>

Creole1.0: Verbatims followed by horizontal rule
.sect input
{{{
verbatim
}}}
----

{{{
verbatim
}}}

----
.sect expected
<pre>verbatim</pre>
<hr />
<pre>verbatim</pre>
<hr />

Creole1.0: Verbatims followed by verbatim
.sect input
{{{
verbatim
}}}
{{{
verbatim
}}}

{{{
verbatim
}}}

{{{
verbatim
}}}
.sect expected
<pre>verbatim</pre>
<pre>verbatim</pre>
<pre>verbatim</pre>
<pre>verbatim</pre>

Creole1.0: Verbatims followed by table
.sect input
{{{
verbatim
}}}
|cell

{{{
verbatim
}}}

|cell
.sect expected
<pre>verbatim</pre>
<table>
<tr><td>cell</td></tr>
</table>
<pre>verbatim</pre>
<table>
<tr><td>cell</td></tr>
</table>

Creole1.0: Images
.sect input
{{image.jpg}}

{{image.jpg|tag}}

{{http://example.org/image.jpg}}
.sect expected
<p><img src="http://www.example.net/static/image.jpg" alt="" /></p>
<p><img src="http://www.example.net/static/image.jpg" alt="tag" /></p>
<p><img src="http://example.org/image.jpg" alt="" /></p>

Creole1.0: Bold combo
.sect input
**bold and
|table|
end**
.sect expected
<p><strong>bold and</strong></p>
<table>
<tr><td>table</td></tr>
</table>
<p>end<strong></strong></p>

