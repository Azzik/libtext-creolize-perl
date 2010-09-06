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

# following tests based on Jason Burnett's Text-WikiCreole-0.07

__END__

.test ampersand
.sect input
Ampersand is a special case.  By itself (like here: & &and here), it should
be converted to &amp; (which should itself be left alone)
.sect expected
<p>Ampersand is a special case.  By itself (like here: &amp; &amp;and here), it should be converted to &amp; (which should itself be left alone)</p>

.test block
.sect input
* list at start of file
** second level
### third level numbered list
### second one
** back to second level

Just a regular paragraph
* interrupted by a list

another 
paragraph
| interrupted by | a table
| r2c1           | r2c2

another paragraph
= interrupted by a heading

* how about a list
====== interrupted by a small heading
* this is a new list

| tables | can be interrupted |
by paragraphs

| tables | can be interrupted |
# by lists

| tables | can be interrupted |
; by anything: that's not a table

* a list
* can be interrupted
: by an definition

* a list
* can be interrupted
{{{
  by a nowiki block
}}}

a paragraph can
be interrupted
{{{
  by a nowiki block
}}}

; any block can be
: interrupted
{{{
  by a nowiki block
}}}

horizontal lines
-----
also interrupt other blocks

{{{
-----
but not nowiki
}}}

* see, lists
-----
* are interrupted by lines also

; crazy stuff happens
-----
: when a line splits a definition list
.sect expected
<ul>
<li>list at start of file
<ul>
<li>second level
<ol>
<li>third level numbered list</li>
<li>second one</li>
</ol>
</li>
<li>back to second level</li>
</ul>
</li>
</ul>
<p>Just a regular paragraph</p>
<ul>
<li>interrupted by a list</li>
</ul>
<p>another paragraph</p>
<table>
<tr><td>interrupted by</td><td>a table</td></tr>
<tr><td>r2c1</td><td>r2c2</td></tr>
</table>
<p>another paragraph</p>
<h1>interrupted by a heading</h1>
<ul>
<li>how about a list</li>
</ul>
<h6>interrupted by a small heading</h6>
<ul>
<li>this is a new list</li>
</ul>
<table>
<tr><td>tables</td><td>can be interrupted</td></tr>
</table>
<p>by paragraphs</p>
<table>
<tr><td>tables</td><td>can be interrupted</td></tr>
</table>
<ol>
<li>by lists</li>
</ol>
<table>
<tr><td>tables</td><td>can be interrupted</td></tr>
</table>
<dl>
<dt>by anything</dt>
<dd>that&#39;s not a table</dd>
</dl>
<ul>
<li>a list</li>
<li>can be interrupted</li>
</ul>
<dl>
<dd>by an definition</dd>
</dl>
<ul>
<li>a list</li>
<li>can be interrupted</li>
</ul>
<pre>  by a nowiki block</pre>
<p>a paragraph can be interrupted</p>
<pre>  by a nowiki block</pre>
<dl>
<dt>any block can be</dt>
<dd>interrupted</dd>
</dl>
<pre>  by a nowiki block</pre>
<p>horizontal lines</p>
<hr />
<p>also interrupt other blocks</p>
<pre>-----
but not nowiki</pre>
<ul>
<li>see, lists</li>
</ul>
<hr />
<ul>
<li>are interrupted by lines also</li>
</ul>
<dl>
<dt>crazy stuff happens</dt>
</dl>
<hr />
<div style="margin-left:2em">
<p>when a line splits a definition list</p>
</div>

.test escape
.sect input
use the escape character to stop
~{{{ 
nowiki blocks from being
nowiki blocks
}}}

~* escape lists

* you can also
~* escape the second list item

~= escape a heading

~: escape an indented paragraph

escape a horizontal line
~-----

~| escape a | table
~| r2c1 | rc2c |

~{{{
escape nowiki ** with bold in it **
}}}

~; escape definition lists
~: like so

you can also escape inline markup 
like ~** bold ~**, ~// italics~//, ~\\ newlines, 
~[[ links ]], ~{{images}}, etc
.sect expected
<p>use the escape character to stop {{{ nowiki blocks from being nowiki blocks }}}</p>
<p>* escape lists</p>
<ul>
<li>you can also * escape the second list item</li>
</ul>
<p>= escape a heading</p>
<p>: escape an indented paragraph</p>
<p>escape a horizontal line -----</p>
<p>| escape a | table | r2c1 | rc2c |</p>
<p>{{{ escape nowiki <strong>with bold in it</strong> }}}</p>
<p>; escape definition lists : like so</p>
<p>you can also escape inline markup like ** bold **, // italics//, \\ newlines, [[ links ]], {{images}}, etc</p>

.test inline
.sect input
** bold at at start of file **

**bold**, //italics//, __underline__, ^^superscript^^, ,,subscript,,, 
and ##monospace## all work the same way.

// italics at start of paragraph //

a paragraph with 
## monospace in the middle ##
with more after

__ underline with
no closing 
markup

* ,, subscript as first list item,
continued on second line ,,

* first list item
* ^^ superscript as second item, no closing tag
* with more to come

* first item
## ## monospace ** with bold ** as first subitem in numbered list ##
*** another sublist after // with italics //

= heading ** with bold **, which is redundant but possible
== heading // with italics // is not redundant


| here's a table | with // italics in it
| second row ^^ squared ^^ | etcetera

here's a [[ link | link //** with bold italics **// in it ]].  
here's some __// ** bold ** inside italics // and underlined __.  
here's what happens when you don't mix ** bold and // italics ** // properly.  
don't try it in {{{ __ inline nowiki __ }}}, but feel free to put it
## // inside monospace // ##.  

{{{
no point in trying to put ^^ superscript ^^ inside nowiki blocks either
}}}

; how 'bout  **__ Underlined bold __** in a definition list?
: not to mention, the actual // italicized definition //

: should also be able to put ** bold ^^ and superscript ^^ ** in an
indented paragraph
.sect expected
<p><strong>bold at at start of file</strong></p>
<p><strong>bold</strong>, <em>italics</em>, <span class="underline">underline</span>, <sup>superscript</sup>, <sub>subscript</sub>, and <tt>monospace</tt> all work the same way.</p>
<p><em>italics at start of paragraph</em></p>
<p>a paragraph with <tt>monospace in the middle</tt> with more after</p>
<p><span class="underline">underline with no closing markup</span></p>
<ul>
<li><sub>subscript as first list item, continued on second line</sub></li>
</ul>
<ul>
<li>first list item</li>
<li><sup>superscript as second item, no closing tag</sup></li>
<li>with more to come</li>
</ul>
<ul>
<li>first item
<ol>
<li><tt>monospace <strong>with bold</strong> as first subitem in numbered list</tt>
<ul>
<li>another sublist after <em>with italics</em></li>
</ul>
</li>
</ol>
</li>
</ul>
<h1>heading <strong>with bold</strong>, which is redundant but possible</h1>
<h2>heading <em>with italics</em> is not redundant</h2>
<table>
<tr><td>here&#39;s a table</td><td>with <em>italics in it</em></td></tr>
<tr><td>second row <sup>squared</sup></td><td>etcetera</td></tr>
</table>
<p>here&#39;s a <a href="http://www.example.net/wiki/link">link //** with bold italics **// in it</a>. here&#39;s some <span class="underline"><em><strong>bold</strong> inside italics</em> and underlined</span>. here&#39;s what happens when you don&#39;t mix <strong>bold and <em>italics **</em> properly. don&#39;t try it in <code>__ inline nowiki __</code>, but feel free to put it <tt><em>inside monospace</em></tt>.</strong></p>
<pre>no point in trying to put ^^ superscript ^^ inside nowiki blocks either</pre>
<dl>
<dt>how &#39;bout <strong><span class="underline">Underlined bold</span></strong> in a definition list?</dt>
<dd>not to mention, the actual <em>italicized definition</em></dd>
</dl>
<div style="margin-left:2em">
<p>should also be able to put <strong>bold <sup>and superscript</sup></strong> in an indented paragraph</p>
</div>

.test specialchars
.sect input
there are several special chars you can use, 
such as \\ line breaks,\\
trademark symbols (TM),\\
registered trademark symbols (R),\\
copyright symbols (C),\\
elipsis ...,\\
dashes, --, or string them together: ------

* use them \\ in lists
## trademark (TM)

| in tables | column\\2
| dash -- | elipsis...

{{{
  anywhere but nowiki: (TM), (R), \\
}}}
.sect expected
<p>there are several special chars you can use, such as<br />
line breaks,<br />
trademark symbols (TM),<br />
registered trademark symbols (R),<br />
copyright symbols (C),<br />
elipsis ...,<br />
dashes, --, or string them together: ------</p>
<ul>
<li>use them<br />
in lists
<ol>
<li>trademark (TM)</li>
</ol>
</li>
</ul>
<table>
<tr><td>in tables</td><td>column<br />
2</td></tr>
<tr><td>dash --</td><td>elipsis...</td></tr>
</table>
<pre>  anywhere but nowiki: (TM), (R), \\</pre>

