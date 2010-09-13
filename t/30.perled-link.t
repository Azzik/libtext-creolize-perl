use strict;
use warnings;
use Test::Base;
use Text::Creolize;

delimiters qw(.test .sect);
plan tests => 1 * blocks;

filters {
    input => [qw(test_creolize chomp)],
    expected => [qw(chomp)],
};

run_is 'input' => 'expected';

sub test_creolize {
    return Text::Creolize->new({type=>'perl'})->convert(@_)->result;
}

__END__

.test single squoted string
.sect input
You can make things **bold** or //italic// or **//both//** or //**both**//.

Character formatting extends across line breaks: **bold,
this is still bold. This line deliberately does not end in star-star.

Not bold. Character formatting does not cross paragraph boundaries.

Here's a external link without a description: [[http://www.wikicreole.org]]

Free links without braces should be rendered as well, like http://www.wikicreole.org/ and http://www.wikicreole.org/users/~example. 

.sect expected
sub{
my($v) = @_;
use utf8;
my $t = '';
$t .= '<p>You can make things <strong>bold</strong> or <em>italic</em> or <strong><em>both</em></strong> or <em><strong>both</strong></em>.</p>
<p>Character formatting extends across line breaks: <strong>bold, this is still bold. This line deliberately does not end in star-star.</strong></p>
<p>Not bold. Character formatting does not cross paragraph boundaries.</p>
<p>Here&#39;s a external link without a description: <a href="http://www.wikicreole.org">http://www.wikicreole.org</a></p>
<p>Free links without braces should be rendered as well, like <a href="http://www.wikicreole.org/">http://www.wikicreole.org/</a> and <a href="http://www.wikicreole.org/users/~example">http://www.wikicreole.org/users/~example</a>.</p>
';
return $t;
}

.test escaped internal link
.sect input
You can use ~[[internal links]].

give the link a ~[[internal links|different]] name.

.sect expected
sub{
my($v) = @_;
use utf8;
my $t = '';
$t .= '<p>You can use [[internal links]].</p>
<p>give the link a [[internal links|different]] name.</p>
';
return $t;
}

.test internal link
.sect input
You can use [[internal links]].

give the link a [[internal links|different]] name.

.sect expected
sub{
my($v) = @_;
use utf8;
my $t = '';
$t .= '<p>You can use ';
$t .= $v->_build_a_element('[[internal links]]',$v->visit_link('internal links','internal links',$v));
$t .= '.</p>
<p>give the link a ';
$t .= $v->_build_a_element('[[internal links|different]]',$v->visit_link('internal links','different',$v));
$t .= ' name.</p>
';
return $t;
}

.test plugin
.sect input
Modified << last_modified >>.
.sect expected
sub{
my($v) = @_;
use utf8;
my $t = '';
$t .= '<p>Modified ';
$t .= $v->_build_plugin($v->visit_plugin('last_modified',$v));
$t .= '.</p>
';
return $t;
}

