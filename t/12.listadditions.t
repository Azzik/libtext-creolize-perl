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

.test definition list
.sect input
; First title of definition list
: Definition of first item.
; Second title: Second definition
beginning on the same line.
; Third title::
Third definition here: show prev colon.
:: And this also 3rd one.
: Obcource this is.
continue line.
; final : description.
.sect expected
<dl>
<dt>First title of definition list</dt>
<dd>Definition of first item.</dd>
<dt>Second title</dt>
<dd>Second definition beginning on the same line.</dd>
<dt>Third title</dt>
<dd>Third definition here: show prev colon.
<dl>
<dd>And this also 3rd one.</dd>
</dl>
</dd>
<dd>Obcource this is. continue line.</dd>
<dt>final</dt>
<dd>description.</dd>
</dl>

.test definition lists and unordered lists
.sect input
* ulist 1
;; term 2: definition 2
;; term 3
:: definition 3
:: definition 3.1
:: definition 3.2
::: definition 3.3
:: defintion 3.4
*** ulist 4
*** ulist 5
:: definition 6
.sect expected
<ul>
<li>ulist 1
<dl>
<dt>term 2</dt>
<dd>definition 2</dd>
<dt>term 3</dt>
<dd>definition 3</dd>
<dd>definition 3.1</dd>
<dd>definition 3.2
<dl>
<dd>definition 3.3</dd>
</dl>
</dd>
<dd>defintion 3.4
<ul>
<li>ulist 4</li>
<li>ulist 5</li>
</ul>
</dd>
<dd>definition 6</dd>
</dl>
</li>
</ul>

.test indented with colon
.sect input
::: level 3
continue
continue
:: level 2
:: level 2
: level 1
level 1
.sect expected
<div style="margin-left:2em">
<div style="margin-left:2em">
<div style="margin-left:2em">
<p>level 3 continue continue</p>
</div>
<p>level 2</p>
<p>level 2</p>
</div>
<p>level 1 level 1</p>
</div>

.test indented with angle bracket
.sect input
>>> level 3
continue
continue
>> level 2
>> level 2
> level 1
level 1
.sect expected
<div style="margin-left:2em">
<div style="margin-left:2em">
<div style="margin-left:2em">
<p>level 3 continue continue</p>
</div>
<p>level 2</p>
<p>level 2</p>
</div>
<p>level 1 level 1</p>
</div>

.test smart nesting
.sect input
*** item 1
** item 2
* item 3
** item 3.1
***** item 3.1.1
*** item 3.1.2
** item 3.2
** item 3.3
* item 4
* item 5
.sect expected
<ul>
<li>item 1</li>
<li>item 2</li>
<li>item 3
<ul>
<li>item 3.1
<ul>
<li>item 3.1.1</li>
<li>item 3.1.2</li>
</ul>
</li>
<li>item 3.2</li>
<li>item 3.3</li>
</ul>
</li>
<li>item 4</li>
<li>item 5</li>
</ul>

