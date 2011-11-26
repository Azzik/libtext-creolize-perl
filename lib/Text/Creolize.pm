package Text::Creolize;
use 5.008002;
use strict;
use warnings;
use Encode qw();
use English qw(-no_match_vars);

# $Id: Creolize.pm,v 0.021 2011/11/26 15:39:18Z tociyuki Exp $
use version; our $VERSION = '0.021';

# Text::Creolize->new(script_name => 'http:example.net/wiki/', ...);
# Text::Creolize->new({script_name => 'http:example.net/wiki/', ...});
sub new {
    my($class, @arg) = @_;
    my $self = bless {}, $class;
    $self->_init(@arg);
    return $self;
}

sub script_name { return shift->_attr(script_name => @_) }
sub static_location { return shift->_attr(static_location => @_) }
sub link_visitor { return shift->_attr(link_visitor => @_) }
sub plugin_visitor { return shift->_attr(plugin_visitor => @_) }
sub result { return shift->_attr(result => @_) }
sub toc { return shift->_attr(toc => @_) }
sub tocinfo { return shift->_attr(tocinfo => @_) }
sub type { return shift->_attr(type => @_) }

sub convert {
    my($self, $wiki_source) = @_;
    $wiki_source =~ s/(?:\r\n?|\n)/\n/gmosx;
    $wiki_source =~ tr/\t/ /;
    $self->{type} ||= 'xhtml';
    $self->{result} = $self->_block($wiki_source);
    if (defined $self->{toc} && @{$self->{tocinfo}} >= $self->{toc}) {
        my $toc = $self->_list_toc;
        $self->{result} = $toc . $self->{result};
    }
    if ($self->{type} eq 'perl') {
        ## no critic qw(Interpolation)
        $self->{result} = "sub{\n"
            . "my(\$v) = \@_;\n"
            . "use utf8;\n"
            . "my \$t = '';\n"
            . "\$t .= '" . $self->{result} . "';\n"
            . "return \$t;\n"
            . "}\n";
    }
    return $self;
}

sub _init {
    my($self, @arg) = @_;
    %{$self} = (
        link_visitor => undef,
        plugin_visitor => undef,
        markup_visitor => undef,
        script_name => 'http://www.example.net/wiki/',
        static_location => 'http://www.example.net/static/',
        result => q{},
        tocinfo => [],
        type => 'xhtml',
    );
    my $opt = ref $arg[0] eq 'HASH' && @arg == 1 ? $arg[0] : {@arg};
    for my $k (qw(
        script_name static_location
        link_visitor plugin_visitor markup_visitor toc type
    )) {
        next if ! exists $opt->{$k};
        $self->{$k} = $opt->{$k};
    }
    return;
}

sub _attr {
    my($self, $f, @arg) = @_;
    if (@arg) {
        $self->{$f} = $arg[0];
    }
    return $self->{$f};
}

my @BASE36 = ('0' .. '9', 'a' .. 'z');
my %XML_SPECIAL = (
    q{&} => q{&amp;}, q{<} => q{&lt;}, q{>} => q{&gt;},
    q{"} => q{&quot;}, q{'} => q{&#39;}, q{\\} => q{&#92;},
);
my $AMP = qr{(?:[a-zA-Z_][a-zA-Z0-9_]*|\#(?:[0-9]{1,5}|x[0-9a-fA-F]{2,4}))}msx;
my $S = qr{[\x20\t]}msx;

my %MARKUP = (
    '<hr>' => '<hr />', '</hr>' => "\n",
    '<blockquote>' => qq{<div style="margin-left:2em">\n},
    '</blockquote>' => "</div>\n", 
    '<table>' => "<table>\n<tr>", '</table>' => "</tr>\n</table>\n",
    '</table><table>' => "</tr>\n<tr>",
    '<ul>' => "<ul>\n<li>", '</ul>' => "</li>\n</ul>\n",
    '</ul><ul>' => "</li>\n<li>",
    '<ol>' => "<ol>\n<li>", '</ol>' => "</li>\n</ol>\n",
    '</ol><ol>' => "</li>\n<li>",
    '<dl>' => "<dl>\n<dt>", '</dl>' => "</dd>\n</dl>\n",
    '</dl><dl>' => "</dd>\n<dt>",
    '<**>' => '<strong>',   '</**>' => '</strong>',
    '<//>' => '<em>',       '<///>' => '</em>',
    '<##>' => '<tt>',     '</##>' => '</tt>',
    '<^^>' => '<sup>',      '</^^>' => '</sup>',
    '<,,>' => '<sub>',      '</,,>' => '</sub>',
    '<__>' => q{<span class="underline">}, '</__>' => q{</span>},
    '<nowiki>' => '<code>', '</nowiki>' => '</code>',
    '<placeholder>' => q{<span class="placeholder">},
    '</placeholder>' => '</span>',
    '<toc>' => qq{<div class="toc">\n}, '</toc>' => qq{</div>\n},
);

my $URIC = q{A-Za-z0-9\\-_~&*+=/}; # and q{.!\$'(),;:\@?\#}
my $HYPERLINK = qr{
    (?:f|ht)tps?://(?:[${URIC}.!\$'(),;:\@?\#]|%[0-9A-Fa-f]{2})+
}msx;
my $FREESTAND = qr{$HYPERLINK (?:[${URIC}\$\@\#]|%[0-9A-Fa-f]{2})}msx;
my $INLINE_TERM = qr{
        \[\[[^\]]*\]\] | \[+ | \{\{[^\}]*\}\}+ | \{+ | <<[^>]*>> | <+    
    |   $FREESTAND | \\\\ | \*+ | // | \#+ | \^\^ | ,, | __
}msx;
my $TILD_ESCAPE = qr{~ (?: $INLINE_TERM | =+ | [^\n ]?)}msx;
my $INLINE_SKIP = qr{$INLINE_TERM | $TILD_ESCAPE}msx;
my $BLOCKQUOTE = qr{
    .*?\n (?=\{\{\{\n
    |[ ]*(?:[\n=|>;:]|-{4,}[ ]*\n|[*](?![*])|[*]{3,}|\#(?!\#)|\#{3,}) )
}msx;

my @BLOCK_SIGN = (
    undef, undef,
    'pre', '', 'hr', 'heading', 'heading', 'table', 'dl',
    'blockquote', 'ul', 'ol',
);

sub _block {
    my($self, $src) = @_;
    chomp $src;
    $src .= "\n\n";
    my $c = {'code' => [], 'result' => q{}};
    my $sign1 = 'p';
    while (not $src =~ m{\G\z}msx) {
        my($data1, $sign, $mark, $data);
        if ($sign1 eq 'p' && $src =~ m{\G
            (.*?)
            ^(?: \{\{\{\n (.*?)\n \}\}\}\n
            |   [ ]*
                (?: () \n
                |   () -{4,} [ ]* \n
                |   (={1,6})=* [ ]* (.*?) [ ]* (?:=+[ ]*)? \n
                |   ([|].*?) [ ]* (?:(?<!~)[|] [ ]*)? \n
                |   ([;]+) [ ]*
                |   ([:>] $BLOCKQUOTE (?:[ ]* [:>] $BLOCKQUOTE)*)
                |   (?:([*](?![*])|[*]{3,}) | (\#(?!\#)|\#{3,})) [ ]*
                )
            )
        }cgmosx) {
            $data1 = $1;
            $sign = $BLOCK_SIGN[$#-];
            $mark = $5 || $8 || $10 || $11 || q{};
            $data = $+;
        }
        elsif ($sign1 eq 'dl' && $src =~ m{\G
            (.*?)
            ^(?: \{\{\{\n (.*?)\n \}\}\}\n
            |   [ ]*
                (?: () \n
                |   () -{4,} [ ]* \n
                |   (={1,6})=* [ ]* (.*?) [ ]* (?:=+[ ]*)? \n
                |   ([|].*?) [ ]* (?:(?<!~)[|] [ ]*)? \n
                |   ([;]+) [ ]*
                |   ([>] $BLOCKQUOTE (?:[ ]* [:>] $BLOCKQUOTE)*)
                |   (?:([*](?![*])|[*]{3,}) | (\#(?!\#)|\#{3,})) [ ]*
                )
            )
        }cgmosx) {
            $data1 = $1;
            $sign = $BLOCK_SIGN[$#-];
            $mark = $5 || $8 || $10 || $11 || q{};
            $data = $+;
        }
        elsif ($src =~ m{\G
            (.*?)
            ^(?: \{\{\{\n (.*?)\n \}\}\}\n
            |   [ ]*
                (?: () \n
                |   () -{4,} [ ]* \n
                |   (={1,6})=* [ ]* (.*?) [ ]* (?:=+[ ]*)? \n
                |   ([|].*?) [ ]* (?:(?<!~)[|] [ ]*)? \n
                |   ([;]+) [ ]*
                |   ([:>] $BLOCKQUOTE (?:[ ]* [:>] $BLOCKQUOTE)*)
                |   (?:([*]+)|(\#+)) [ ]*
                )
            )
        }cgmosx) {
            $data1 = $1;
            $sign = $BLOCK_SIGN[$#-];
            $mark = $5 || $8 || $10 || $11 || q{};
            $data = $+;
        }
        if ($data1 ne q{}) {
            chomp $data1;
            if ($sign1 eq 'p') {
                $self->_emit_block($c, 'p', '');
            }
            if ($sign1 eq 'dl') {
                $self->_definition_list($c, $data1);
            }
            else {
                $c->{'result'} .= $self->_inline($data1);
            }
        }
        $sign1 = 'p';
        if ($sign eq 'heading') {
            $self->_heading($c, $mark, $data);
            next;
        }
        $self->_emit_block($c, $sign, $mark);
        if ($sign eq 'pre') {
            $data =~ s/^[ ](\}{3})/$1/gmosx;
            $c->{'result'} .= $self->escape_xml($data);
        }
        elsif ($sign eq 'table') {
            $self->_table_row($c, $data);
        }
        elsif ($sign eq 'blockquote') {
            $data =~ s/^[ ]*[:>][ ]?//gmsx;
            $c->{'result'} .= $self->_block($data);
        }
        elsif ($sign eq 'dl' || $sign eq 'ul' || $sign eq 'ol') {
            $sign1 = $sign;
        }
    }
    return $c->{'result'};
}

sub _heading {
    my($self, $c, $mark, $data) = @_;
    $self->_emit_block($c, '', '');
    my $level = length $mark;
    my $inline = $self->_inline($data);
    my $attr = q{};
    if (defined $self->toc) {
        my $text = $inline;
        $text =~ s/<.*?>//gmosx;
        if ($text !~ m/\A[ ]*\z/msx) {
            my $id = 'h' . $self->hash_base36($text);
            $attr = qq{ id="$id"};
            push @{$self->{tocinfo}}, [$level, $id, $text];
        }
    }
    $c->{'result'} .= "<h$level$attr>" . $inline . "</h$level>\n";
    return $c;
}

sub _list_toc {
    my($self) = @_;
    my $c = {'result' => q{}, 'code' => []};
    for my $info (@{$self->{tocinfo}}) {
        $self->_emit_block($c, 'ul', q{*} x $info->[0]);
        $self->_insert_link($c, $info->[2], "#$info->[1]", $info->[2]);
    }
    $self->_emit_block($c, q{}, q{});
    return $MARKUP{'<toc>'} . $c->{'result'} . $MARKUP{'</toc>'};
}

sub _table_row {
    my($self, $c, $data) = @_;
    my $result = q{};
    $data .= q{|};
    while ($data =~ m{\G
        ([|]=?) [ ]* ([^|~\[\{]*?(?:$INLINE_SKIP [^|~\[\{]*?)*?) [ ]* (?=[|])
    }gcmosx) {
        my($mark, $inline) = ($1, $2);
        my $td = $mark eq q{|=} ? 'th' : 'td';
        $result .= qq{<$td>} . $self->_inline($inline) . qq{</$td>};
    }
    $c->{'result'} .= $result;
    return $c;
}

sub _definition_list {
    my($self, $c, $data) = @_;
    my($dt, $dd) = $data =~ m{\A
        ([^:~\[\{]*? (?:$INLINE_SKIP [^:~\[\{]*)*?) (?:[\n ]*[:][ ]*(.*))?
    \z}msx;
    $dd = defined $dd ? $dd : q{};
    $c->{'result'} .= $self->_inline($dt) . "</dt>\n<dd>" . $self->_inline($dd);
    return $c;
}

sub _emit_block {
    my($self, $c, $sign, $mark) = @_;
    my $level = length $mark;
    my $result = q{};
    my $code = $c->{'code'};
    if ($level > 0 && @{$code} && $code->[0][0] == 0) {
        $self->_emit_block($c, '', '');
    }
    while ($#{$code} >= 1 && $level < $code->[0][0]) {
        if ($code->[1][0] < $level) {
            $code->[0][0] = $level;
            last;
        }
        if (my $prev = (shift @{$code})->[1]) {
            my $e = "</$prev>";
            $result .= exists $MARKUP{$e} ? $MARKUP{$e} : "$e\n";
        }
    }
    my $s = "<$sign>";
    if (! @{$code}) {
        if ($sign) {
            $result .= exists $MARKUP{$s} ? $MARKUP{$s} : $s;
        }
        unshift @{$code}, [$level, $sign];
    }
    elsif ($code->[0][1] && $code->[0][0] < $level) {
        $result .= "\n";
        if ($sign) {
            $result .= exists $MARKUP{$s} ? $MARKUP{$s} : $s;
        }
        unshift @{$code}, [$level, $sign];
    }
    else {
        my $prev = $code->[0][1];
        my $e = "</$prev>";
        if (! $sign && $prev) {
            $result .= exists $MARKUP{$e} ? $MARKUP{$e} : "$e\n";
        }
        elsif ($sign && ! $prev) {
            $result .= exists $MARKUP{$s} ? $MARKUP{$s} : $s;
        }
        elsif ($sign && $prev) {
            my $joint = "$e$s";
            $result .= $MARKUP{$joint}
                || ((exists $MARKUP{$e} ? $MARKUP{$e} : "$e\n")
                    .(exists $MARKUP{$s} ? $MARKUP{$s} : $s));
        }
        @{$code->[0]} = ($level, $sign);
    }
    $c->{'result'} .= $result;
    return $c;
}

my($EOF, $LINK, $NOWIKI, $IMG, $BR, $PHRASE, $TILD,
    $PLACEHOLDER, $PLUGIN, ) = (0 .. 8);
my @INLINE_SIGN = (
    undef, undef, $EOF, $LINK, $LINK, $LINK, $NOWIKI,
    $IMG, $IMG, $IMG, $LINK, $BR, $PHRASE, $TILD,
    $PLACEHOLDER, $PLACEHOLDER, $PLUGIN, $PLUGIN,
);

sub _inline {
    my($self, $inline) = @_;
    my $c = {'result' => q{}};
    my %phrase_member;
    my @phrase;
    while ($inline =~ m{\G
        (.*?)
        (?: (\z)
        |   (\[\[ [ ]* ([^|\]]+?) [ ]* (?: [|] [ ]* ([^\]]*?) [ ]*)? \]\])
        |   (\{\{\{ .*? \}\}\}+)
        |   (\{\{ [ ]* ([^|\}]+?) [ ]* (?: [|] [ ]* ([^\}]*?) [ ]*)? \}\})
        |   \b ($FREESTAND)
        |   (\\\\) [\t\n\x20]*
        |   (\*\* | // | \#\# | \^\^ | ,, | __)
        |   ($TILD_ESCAPE)
        |   (<<<(.*?)>>>)
        |   (<< [ ]* (.*?)  [ ]* >>)
        )
    }gcmosx) {
        my($type, $text) = ($INLINE_SIGN[$#-], $3 || $7 || $+);
        $c->{'result'} .= $self->escape_text($1);
        last if $type == $EOF;
        if ($type == $LINK) {
            my $href = defined $10 ? $10 : $4;
            my $desc = defined $5 ? $5 : $href;
            $self->_insert_link($c, $text, $href, $desc);
            next;
        }
        elsif ($type == $IMG) {
            my $src = $8;
            my $alt = defined $9 ? $9 : q{};
            $self->_insert_braced($c, $text, $src, $alt);
            next;
        }
        elsif ($type == $BR) {
            $c->{'result'} .= qq{<br />\n};
            next;
        }
        elsif ($type == $PHRASE) {
            if (! $phrase_member{$text}) {
                $phrase_member{$text} = 1;
                unshift @phrase, $text;
                $c->{'result'} .= $MARKUP{"<$text>"};
                next;
            }
            elsif ($phrase[0] eq $text) {
                $phrase_member{$text} = 0;
                shift @phrase;
                $c->{'result'} .= $MARKUP{"</$text>"};
                next;
            }
        }
        elsif ($type == $PLACEHOLDER) {
            $c->{'result'} .= $MARKUP{'<placeholder>'} . $self->escape_xml($text) . $MARKUP{'</placeholder>'};
            next;
        }
        elsif ($type == $PLUGIN) {
            $self->_insert_plugin($c, $text);
            next;
        }
        elsif ($type == $NOWIKI) {
            my($data) = $text =~ m/\A... [ ]* (.*?) [ ]* ...\z/mosx;
            $c->{'result'} .= $MARKUP{'<nowiki>'} . $self->escape_xml($data) . $MARKUP{'</nowiki>'};
            next;
        }
        elsif ($type == $TILD && $text ne q{~}) {
            $text = substr $text, 1;
        }
        $c->{'result'} .= $self->escape_text($text);
    }
    $c->{'result'} .= join q{}, map { $MARKUP{"</$_>"} } @phrase;
    return $c->{'result'};
}

sub _insert_link {
    my($self, $c, $source, $link, $text) = @_;
    my $visitor = $self->{link_visitor} || $self;
    my $anchor = $visitor->visit_link($link, $text, $self);
    if ($anchor && $self->type eq 'perl' && $anchor->{runtime}) {
        ## no critic qw(Interpolation)
        my $proc = q{$v->_build_anchor(}
            . q{'} . $self->escape_quote($source) . q{',}
            . q{$v->visit_link(}
                . q{'} . $self->escape_quote($link) . q{',}
                . q{'} . $self->escape_quote($text) . q{',}
                . q{$v}
            . q{)}
        . q{)};
        $c->{'result'} .= qq{';\n\$t .= $proc;\n\$t .= '};
    }
    elsif ($anchor && ($anchor->{name} || $anchor->{href})) {
        $c->{'result'} .= $self->_build_anchor($source, $anchor);
    }
    else {
        $c->{'result'} .= $self->escape_text($source);
    }
    return $self;
}

sub _build_anchor {
    my($self, $source, $anchor) = @_;
    if (! $anchor->{href} && ! $anchor->{name}) {
        return $self->escape_xml($source);
    }
    my $t = q{};
    if (exists $anchor->{before}) {
        $t .= $anchor->{before};
    }
    my $attr = q{};
    if (my $href = $anchor->{href}) {
        $attr .= q{ href="} . $self->escape_uri($href) . q{"};
    }
    for my $k (qw(id name class rel rev type title)) {
        next if ! $anchor->{$k};
        $attr .= qq{ $k="} . $self->escape_text($anchor->{$k}) . q{"};
    }
    $t .= qq{<a$attr>} . $self->escape_text($anchor->{text}) . q{</a>};
    if (exists $anchor->{after}) {
        $t .= $anchor->{after};
    }
    return $t;
}

sub _insert_braced {
    my($self, $c, $text, $link, $title) = @_;
    my $visitor = $self->{link_visitor} || $self;
    my $image = $visitor->visit_image($link, $title, $self);
    if (! $image || ! $image->{src}) {
        $c->{'result'} .= $self->escape_text($text);
        return $self;
    }
    my $attr = q{ src="} . $self->escape_uri($image->{src}) . q{"};
    for my $k (qw(id class alt title)) {
        next if ! defined $image->{$k};
        $attr .= qq{ $k="} . $self->escape_text($image->{$k}) . q{"};
    }
    $c->{'result'} .= qq{<img$attr />};
    return $self;
}

sub _insert_plugin {
    my($self, $c, $source) = @_;
    return $self if $self->{plugin_run}; # avoid recursive calls
    local $self->{plugin_run} = 1; ## no critic qw(LocalVars)
    my $visitor = $self->{plugin_visitor} || $self;
    my $plugin = $visitor->visit_plugin($source, $self);
    if (! $plugin) {
        return $self;
    }
    if ($plugin->{runtime}) {
        ## no critic qw(Interpolation)
        my $proc= q{$v->_build_plugin($v->visit_plugin('}
            . $self->escape_quote($source)
            . q{',$v))};
        $c->{'result'} .= qq{';\n\$t .= $proc;\n\$t .= '};
    }
    else {
        $c->{'result'} .= $self->_build_plugin($plugin);
    }
    return $self;
}

sub _build_plugin {
    my($self, $plugin) = @_;
    return defined $plugin->{content} ? $plugin->{content}
        : defined $plugin->{text} ? $self->escape_text($plugin->{text})
        : defined $plugin->{xml} ? $self->escape_xml($plugin->{xml})
        : q{};
}

# VISITORS
# $hash_anchor = $creolize->link_visitor->visit_link($link, $title, $creolize);
sub visit_link {
    my($self, $link, $text, $builder) = @_;
    my $anchor = {};
    return $anchor if $link =~ /script:/imosx;
    if ($link !~ m{\A(?:(?:https?|ftps?)://|\#)}mosx) {
        if ($builder->type eq 'perl') {
            $anchor->{runtime} = 'yes';
        }
        $link = $builder->script_name . $link;
    }
    $anchor->{href} = $link;
    $anchor->{text} = defined $text ? $text : $link;
    return $anchor;
}

# $hash_image = $creolize->link_visitor->visit_image($link, $title, $creolize);
sub visit_image {
    my($self, $link, $title, $builder) = @_;
    my $image = {};
    if ($link !~ m{\Ahttps?://}mosx) {
        $link = $builder->static_location . $link;
    }
    $image->{src} = $link;
    $image->{alt} = defined $title ? $title : q{};
    return $image;
}

# $hash_plugin = $creolize->plugin_visitor->visit_plugin($data, $creolize);
sub visit_plugin {
    my($self, $data, $builder) = @_;
    my $plugin = {};
    if ($builder->type eq 'perl') {
        $plugin->{runtime} = 'yes';
    }
    $plugin->{text} = q{};
    return $plugin;
}

sub escape_xml {
    my($self, $data) = @_;
    $data =~ s{([&<>"'\\])}{ $XML_SPECIAL{$1} }egmosx;
    return $data;
}

sub escape_text {
    my($self, $data) = @_;
    $data =~ s{(?:([<>"'\\])|\&(?:($AMP);)?)}{
        $1 ? $XML_SPECIAL{$1} : $2 ? qq{\&$2;} : q{&amp;}
    }egmosx;
    return $data;
}

sub escape_uri {
    my($self, $uri) = @_;
    if (utf8::is_utf8($uri)) {
        $uri = Encode::encode('utf-8', $uri);
    }
    $uri =~ s{
        (?:(\%([0-9A-Fa-f]{2})?)|(&(?:amp;)?)|([^a-zA-Z0-9_~\-.=+\$,:\@/;?\#]))
    }{
        $2 ? $1 : $1 ? '%25' : $3 ? '&amp;' : sprintf '%%%02X', ord $4
    }egmosx;
    return $uri;
}

sub escape_name {
    my($self, $name) = @_;
    if (utf8::is_utf8($name)) {
        $name = Encode::encode('utf-8', $name);
    }
    $name =~ s{([^a-zA-Z0-9_.\-:/])}{ sprintf "%%%02X", ord($1) }msxge;
    return $name;
}

sub escape_quote {
    my($self, $data) = @_;
    $data =~ s{'}{\\'}gmosx;
    return $data;
}

sub hash_base36 {
    my($self, $text) = @_;
    if (utf8::is_utf8($text)) {
        $text = Encode::encode_utf8($text);
    }
    my $x = _murmurhash_pp($text);
    return _hex_base36(sprintf '%08x', $x);
}

sub _hex_base36 {
    my($hexbigint) = @_;
    use integer;
    my @q = unpack 'C*', pack 'H*', $hexbigint;
    my $base36 = q{};
    for (0 .. 6) {
        my $r = 0;
        for my $j (0 .. $#q) {
            my $a = ($r << 8) + $q[$j];
            $q[$j] = $a / 36;
            $r = $a % 36;
        }
        $base36 = $BASE36[$r] . $base36;
    }
    return $base36;
}

sub _murmurhash_pp {
    my($s) = @_;
    use integer;
    my $len = length $s or return 0;
    if (my $paddings = (($len + 3) & ~3) - $len) {
        $s .= "\0" x $paddings;
    }
    my $h = 0x5bd1e995 * $len;
    for (unpack 'V*', $s) {
        $h = (($h + $_) * 0x5bd1e995) & 0xffffffff;
        $h = ((($h >> 16) & 0x0000ffff) ^ $h) & 0xffffffff;
    }
    $h = ($h * 0x5bd1e995) & 0xffffffff;
    $h = ((($h >> 10) & 0x003fffff) ^ $h) & 0xffffffff;
    $h = ($h * 0x5bd1e995) & 0xffffffff;
    $h = ((($h >> 17) & 0x00007fff) ^ $h) & 0xffffffff;
    return $h;
}

1;

__END__

=pod

=head1 NAME

Text::Creolize - A practical converter for WikiCreole to XHTML.

=head1 VERSION

0.021

=head1 SYNOPSIS

    use Text::Creolize;
    use File::Slurp;
    use Encode;

    # from http://www.wikicreole.org/wiki/Creole1.0TestCases
    my $source = read_file('creole1.0test.txt');
    $source = decode('UTF-8', $source);
    my $xhtml = Text::Creolize->new->convert($source)->result;
    print encode('UTF-8', $xhtml);

    my $perlsrc = Text::Creolize->new({type => 'perl'})->convert($source)->result;
    $cache->set('key' => encode('UTF-8', $perlsrc));
    $xhtml = (eval $perlsrc)->(Text::Creolize->new);

=head1 DESCRIPTION

This module provides you to convert a WikiCreole formatted string
to XHTML.

=head1 METHODS

=over

=item C<< $creolize = Text::Creolize->new >>

Creates the Text::Creolize converter. We may set values of attributes
with hash arguments of it.

=item C<< $creolize->type >>

Readwrite attribute accessor for the result type.
Its default value is 'xhtml' to make text/xthml result.
You can generate perl subroutine source code instead of text/xthml
to set it 'perl'.
 
=item C<< $creolize->script_name >>

Readwrite attribute accessor for the script_name.
The attribute value may be used to construct word links.

=item C<< $creolize->static_location >>

Readwrite attribute accessor for the static location.
The attribute value may be used to construct image sources.

=item C<< $creolize->link_visitor >>

Readwrite attribute accessor for the link visitor.

=item C<< $creolize->plugin_visitor >>

Readwrite attribute accessor for the plugin visitor.

=item C<< $creolize->convert($string) >>

Converts from a WikiCreole formatted string to a XHTML one.
The utf8 flag of the string should be turned on.

=item C<< $creolize->toc(4) >>

When number of headings is greater than this property value,
creates and inserts the table of contents at the top of the
XHTML result.

=item C<< $toc_arrayref = $creolize->tocinfo >>

Gets the list of headings's nesting level, its XML element id,
and inner text.

=item C<< $base36 = $creolize->hash_base36($string) >>

Calculates the hash value of the given string.

=item C<< $string = $creolize->result >>

Gets a converted result. It's utf8 flag will be turned on.

=item C<< $anchor = $link_visitor->visit_link($link, $title, $builder) >>

The visitor's hook when the converter catches a href link.
The return value must be a hash reference
C<< @{$anchor}{qw(href name title rev rel id class before after runtime)} >>.

=over

=item C<< $anchor->{href} >>

the url of the href attribute in the anchor element.
href or name is required.

=item C<< $anchor->{name} >>

the value of the name attribute in the anchor element.
href or name is required.

=item C<< $anchor->{title} >>

the optional value of the title attribute in the anchor element.

=item C<< $anchor->{rev} >>

the optional value of the rev attribute in the anchor element.

=item C<< $anchor->{rel} >>

the optional value of the rel attribute in the anchor element.

=item C<< $anchor->{id} >>

the optional value of the id attribute in the anchor element.

=item C<< $anchor->{class} >>

the optional value of the class attribute in the anchor element.

=item C<< $anchor->{before} >>

the optional xhtml markup put before the anchor element.

=item C<< $anchor->{after} >>

the optional xhtml markup put after the anchor element.

=item C<< $anchor->{runtime} >>

the optional boolean runtime redering flag.

=back

=item C<< $image = $link_visitor->visit_image($link, $title, $builder) >>

The visitor's hook when the converter catches a image link.
This must return a hash reference C<< @{$image}{qw(src id class alt title)} >>.

=item C<< $plugin = $plugin_visitor->visit_plugin($data, $builder) >>

The visitor's hook when the converter catches a plugin.
This must return a hash reference C<< @{$plugin}{qw(runtime text xml content)} >>.

=item C<< $string = $creolize->escape_text($string) >>

Escapes XML special characters without XHTML entities.

=item C<< $string = $creolize->escape_xml($string) >>

Escapes XML special characters.

=item C<< $string = $creolize->escape_uri($string) >>

Encode URI with parcent encoded.

=item C<< $string = $creolize->escape_name($string) >>

Encode URI with parcent encoded for a name part.

=item C<< $string = $creolize->escape_quote($string) >>

Escape single quote with a backslash mark in the given string.

=back

=head1 LIMITATION

Cannot recognize double bracketted arrow links.

=head1 REPOSITORY

You can git-clone latest sources from
L<http://github.com/tociyuki/libtext-creolize-perl>

=head1 DEPENDENCIES

L<Encode>

=head1 SEE ALSO

L<Text::WikiCreole>
L<http://www.wikicreole.org/wiki/Creole1.0>
L<http://www.wikicreole.org/wiki/CreoleAdditions>

=head1 AUTHOR

MIZUTANI Tociyuki  C<< <tociyuki@gmail.com> >>

=head1 LICENSE AND COPYRIGHT

Copyright (c) 2011, MIZUTANI Tociyuki C<< <tociyuki@gmail.com> >>.
All rights reserved.

This module is free software; you can redistribute it and/or
modify it under the same terms as Perl itself.

=cut

