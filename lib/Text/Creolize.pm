package Text::Creolize;
use 5.008002;
use strict;
use warnings;
use Encode qw();
use English qw(-no_match_vars);
use Digest::MurmurHash;

# $Id: Creolize.pm,v 0.015 2010/09/13 12:46:41Z tociyuki Exp $
use version; our $VERSION = '0.015';

my $WTYPE_NULL = 0;
my $WTYPE_TEXT = 1;
my $WTYPE_STAG = 2;
my $WTYPE_ETAG = 3;

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
    chomp $wiki_source;
    $wiki_source .= "\n";
    $self->{type} ||= 'xhtml';
    $self->{prev_wtype} = $WTYPE_NULL;
    $self->{blank} = q{};  # and clear
    $self->_scan($wiki_source);
    if (defined $self->{toc} && @{$self->{tocinfo}} >= $self->{toc}) {
        my $toc = $self->_list_toc->result;
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
        prev_wtype => $WTYPE_NULL,
        result => q{},
        blank => q{},
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
my %MARKUP = (
    q{=} => {stag => q{<h1>}, etag => qq{</h1>\n}},
    q{==} => {stag => q{<h2>}, etag => qq{</h2>\n}},
    q{===} => {stag => q{<h3>}, etag => qq{</h3>\n}},
    q{====} => {stag => q{<h4>}, etag => qq{</h4>\n}},
    q{=====} => {stag => q{<h5>}, etag => qq{</h5>\n}},
    q{======} => {stag => q{<h6>}, etag => qq{</h6>\n}},
    q{p} => {stag => q{<p>}, etag => qq{</p>\n}},
    q{verbatim} => {stag => q{<pre>}, etag => qq{</pre>\n}},
    q{----} => {tag => qq{<hr />\n}},
    q{*} => {
        stag => qq{<ul>\n<li>}, etag => qq{</li>\n</ul>\n},
        q{*} => qq{</li>\n<li>}, q{#} => qq{</li>\n</ul>\n<ol>\n<li>},
        q{;} => qq{</li>\n</ul>\n<dl>\n<dt>},
        q{:} => qq{</li>\n</ul>\n<dl>\n<dd>},
    },
    q{#} => {
        stag => qq{<ol>\n<li>}, etag => qq{</li>\n</ol>\n},
        q{#} => qq{</li>\n<li>}, q{*} => qq{</li>\n</ol>\n<ul>\n<li>},
        q{;} => qq{</li>\n</ol>\n<dl>\n<dt>},
        q{:} => qq{</li>\n</ul>\n<dl>\n<dd>},
    },
    q{;} =>  {
        stag => qq{<dl>\n<dt>}, etag => qq{</dt>\n</dl>\n},
        q{;} => qq{</dt>\n<dt>}, q{:} => qq{</dt>\n<dd>},
        q{*} => qq{</dt>\n</dl>\n<ul>\n<li>},
        q{#} => qq{</dt>\n</dl>\n<ol>\n<li>},
    },
    q{:} =>  {
        stag => qq{<dl>\n<dd>}, etag => qq{</dd>\n</dl>\n},
        q{;} => qq{</dd>\n<dt>}, q{:} => qq{</dd>\n<dd>},
        q{*} => qq{</dd>\n</dl>\n<ul>\n<li>},
        q{#} => qq{</dd>\n</dl>\n<ol>\n<li>},
    },
    q{||} => {
        stag => qq{<table>\n<tr>}, etag => qq{</tr>\n</table>\n},
        q{||} => qq{</tr>\n<tr>},
    },
    q{|} =>  {stag => q{<td>}, etag => q{</td>}},
    q{|=} => {stag => q{<th>}, etag => q{</th>}},
    q{>} => {
        stag => qq{<div style="margin-left:2em">\n}, etag => qq{</div>\n},
    },
    q{**} => {stag => q{<strong>}, etag => q{</strong>}},
    q{//} => {stag => q{<em>}, etag => q{</em>}},
    q{##} => {stag => q{<tt>}, etag => q{</tt>}},
    q{^^} => {stag => q{<sup>}, etag => q{</sup>}},
    q{,,} => {stag => q{<sub>}, etag => q{</sub>}},
    q{__} => {stag => q{<span class="underline">}, etag => q{</span>}},
    q{\\\\} =>   {tag => qq{<br />\n}},
    q{nowiki} => {stag => q{<code>}, etag => q{</code>}},
    q{<<<} => {stag => q{<span class="placeholder">}, etag => q{</span>}},
    q{toc} => {stag => qq{<div class="toc">\n}, etag => qq{</div>\n}},
);
my %XML_SPECIAL = (
    q{&} => q{&amp;}, q{<} => q{&lt;}, q{>} => q{&gt;},
    q{"} => q{&quot;}, q{'} => q{&#39;},
);
my $AMP = qr{(?:[a-zA-Z_][a-zA-Z0-9_]*|\#(?:[0-9]{1,5}|x[0-9a-fA-F]{2,4}))}msx;
my $S = qr{[\x20\t]}msx;
my $ESCAPED = qr{
    (?: (?=[\x20\t\r\n])
    |   \*+ | \/\/ | \\\\ | \#+ | \^\^ | ,, | __ | ;+ | \:+ | =+
    |   \[\[[^\r\n]*?\]\] | \{\{(?:(?!\{)[^\r\n]*?\}\}|\{+) | <<+ | ----+
    |   (?:[A-Z][a-z]+){2,}\b
    |   https?://(?:[A-Za-z0-9\-._~:/?\#&+,;=]|%[0-9A-Fa-f]{2})+
        (?:[A-Za-z0-9\-_~/\#&+=]|%[0-9A-Fa-f]{2})
    |   ftps?://[A-Za-z0-9\-._/+]+[A-Za-z0-9\-_/+]
    |   .
    )
}msx;
my $JUSTLIST = qr{(?:\*(?:(?!\*)|\*{2,})|\#(?:(?!\#)|\#{2,}))}msx;
my $TOKEN = qr{                         (?#=> 'EOF' )
    (?: (\z)                            (?#=> 'EOF' )
    |   (\n)                            (?#=> 'EOL' )
    |   (?:\A|(?<=\n))
        (?: (\{\{\{\n.*?\n\}\}\}\n)   (?#=> 'VERBATIM' )
        |   $S* (?: (-{4,})$S*\n        (?#=> 'HRULE' )
            |   ($JUSTLIST$S*)          (?#=> 'JUSTLIST' )
            |   (\|=?)$S*               (?#=> 'TD' )
            |   (\;+)$S*                (?#=> 'TERM' )
            |   (\>+)$S*                (?#=> 'QUOTE' )
            |   (=+)$S*))               (?#=> 'HEADING' )
    |   ($S*=+)$S*(?=\n|\z)             (?#=> 'HEADING' )
    |   (\*\*|\#\#)                     (?#=> 'MAYBELIST' )
    |   ($S*\|)$S*(?=\n|\z)             (?#=> 'ENDTR' )
    |   (\|=?$S*)                       (?#=> 'TD' )
    |   (\:+)                           (?#=> 'DESC' )
    |   (\/\/|\^\^|,,|__)               (?#=> 'PHRASE' )
    |   (\\\\)                          (?#=> 'BREAK' )
    |   (\{\{\{ .*? \}\}\}+)            (?#=> 'NOWIKI' )
    |   (\[\[ [^\n]*? \]\])             (?#=> 'BRACKETED' )
    |   (\{\{(?!\{) [^\n]*? \}\})       (?#=> 'BRACED' )
    |   <<(?:<(.*?)>>>                  (?#=> 'PLACEHOLDER' )
        |$S*(.*?)$S*>>)                 (?#=> 'PLUGIN')
    |   (\{\{+ | \[\[+ | <<+)           (?#=> 'TEXT' )
    |   \b
        (   https?://(?:[A-Za-z0-9\-._~:/?\#&+,;=]|%[0-9A-Fa-f]{2})+
            (?:[A-Za-z0-9\-_~/\#&+=]|%[0-9A-Fa-f]{2})
        |   ftps?://[A-Za-z0-9\-._/+]+[A-Za-z0-9\-_/+]
        |   (?:[A-Z][a-z]+){2,}\b)      (?#=> 'FREESTAND' )
    |   (\~ $ESCAPED)                   (?#=> 'ESCAPE' )
    |   ($S+)                           (?#=> 'BLANK' )
    |   (.+?)                           (?#=> 'TEXT' )
        (?= $S*
            (?: \z
            |   [\n~\|:] | \*\* | \#\# | \/\/ | \^\^ | \,\, | \_\_ | \\\\
            |   \{\{ |\[\[ | <<
            |   =+$S*(?:\n|\z)
            |   \b (?:https?://|ftp://|(?:[A-Z][a-z]+){2,}\b)
            )
        )
    )
}mosx;

## no critic qw(ComplexMappings)
my %TOKEN_DISPATCH = (
    'EOF' => [
        [undef], [undef, 3], [undef, 3], [undef, 0], [undef, 2], [undef, 2],
        [undef, 4], [undef, 4], [undef, 4], [undef, 3, 1], [undef, 3, 1],
        [undef, 2], [undef, 2],
    ],
    'EOL' => [
        [0], [2, 27], [0, 3], [0, 0], [5, 27], [0, 2], [8], [8], [0, 4],
        [10, 27], [0, 3, 1], [12, 27], [0, 2],
    ],
    'VERBATIM' => [
        [0, 20], [0], [0, 3, 20], [0], [0], [0, 2, 20], [0], [0], [0, 4, 20],
        [0], [0, 3, 1, 20], [0], [0, 2, 20],
    ],
    'HRULE' => [
        [0, 11], [0], [0, 3, 11], [0], [0], [0, 2, 11], [0], [0], [0, 4, 11],
        [0], [0, 3, 1, 11], [0], [0, 2, 11],
    ],
    'HEADING' => [
        [3, 21], [1, 26], [3, 3, 21], [0, 0], [4, 26], [3, 2, 21], [6, 26],
        [0], [3, 4, 21], [9, 26], [3, 3, 1, 21], [11, 26], [3, 2, 21],
    ],
    'JUSTLIST' => [
        [4, 23], [0], [4, 3, 23], [0], [0], [4, 13], [0], [0], [4, 4, 23],
        [0], [4, 3, 1, 23], [0], [4, 13],
    ],
    'MAYBELIST' => [
        [1, 24, 15], [1, 15], [1, 15], [3, 15], [4, 15], [4, 13], [6, 15],
        [0], [1, 4, 24, 15], [9, 15], [10, 15], [11, 15], [4, 13],
    ],
    'TD' => [
        [6, 25], [1, 26], [6, 3, 25], [3, 26], [4, 26], [6, 2, 25], [6, 18],
        [0], [6, 19], [9, 26], [6, 3, 1, 25], [11, 26], [6, 2, 25],
    ],
    'ENDTR' => [
        [0], [1, 26], [0], [3, 26], [4, 26], [0], [7], [0], [0], [9, 26], [0],
        [11, 26], [0],
    ],
    'TERM' => [
        [11, 23], [0], [11, 3, 23], [0], [0], [11, 13], [0], [0], [11, 4, 23], [0], [11, 3, 1, 23], [0], [11, 13],
    ],
    'DESC' => [
        [9, 22, 24], [1, 26], [9, 3, 22, 24], [3, 26], [4, 26], [4, 13],
        [6, 26], [0], [9, 4, 22, 24], [9, 26], [9, 3, 12, 24], [4, 8],
        [4, 13],
    ],
    'QUOTE' => [
        [9, 22, 24], [0], [9, 3, 22, 24], [0], [0], [9, 2, 22, 24], [0], [0],
        [9, 4, 22, 24], [0], [9, 3, 12, 24], [0], [9, 2, 22, 24],
    ],
    'BLANK' => [
        [0], [1, 27], [2], [3, 27], [4, 27], [5], [6, 27], [0], [8], [9, 27],
        [10], [11, 27], [12],
    ],
    (map{
         my($token, $m) = @{$_};
         ($token => [
             [1, 24, $m], [1, $m], [1, $m], [3, $m], [4, $m], [4, $m],
             [6, $m], [$m], [1, 4, 24, $m], [9, $m], [9, $m], [11, $m],
             [11, $m],
         ]);
    } ['PHRASE' => 15], ['BREAK' => 5], ['NOWIKI' => 14], ['BRACKETED' => 7],
      ['BRACED' => 6], ['PLACEHOLDER' => 16], ['PLUGIN' => 17],
      ['FREESTAND' => 10], ['ESCAPE' => 9], ['TEXT' => 26],
    ),
);
my @TOKEN_ACTION = qw(
    _end_h _end_indent _end_list _end_p _end_table _insert_br _insert_braced
    _insert_bracketed _insert_colon _insert_escaped _insert_freestand
    _insert_hr _insert_indent _insert_list _insert_nowiki _insert_phrase
    _insert_placeholder _insert_plugin _insert_td _insert_tr _insert_verbatim
    _start_h _start_indent _start_list _start_p _start_table put puts
);
my @TOKEN_TABLE = map { $TOKEN_DISPATCH{$_} } @{_regexp_scan($TOKEN)};

sub _regexp_scan {
    my($regexp) = @_;
    my $list = join q{,}, "$regexp" =~ /\(\?\#=>\s*(\S*)\s*\)/gmsx;
    return eval "[$list]";  ## no critic (StringyEval)
}

sub _scan {
    my($self, $wiki_source) = @_;
    my $put_text = $TOKEN_DISPATCH{'TEXT'}[1][1];
    my @action = map { $self->can($_) } @TOKEN_ACTION;
    my $state = 0;
    while (defined $state) {
        $wiki_source =~ /\G$TOKEN/gcmosx;
        my($data, $token) = ($LAST_PAREN_MATCH, $#LAST_MATCH_START);
        my($succ, @methods) = @{$TOKEN_TABLE[$token][$state]};
        $state = $succ;
        for my $i (@methods) {
            if ($i == $put_text) {
                $data =~ s{(?:([<>"'])|\&(?:($AMP);)?)}{
                    $1 ? $XML_SPECIAL{$1} : $2 ? qq{\&$2;} : q{&amp;}
                }egmosx;
                $self->{result} .= $self->{blank} . $data;
                $self->{blank} = q{};
                $self->{prev_wtype} = $WTYPE_TEXT;
            }
            else {
               $action[$i]->($self, $data);
            }
        }
    }
    return $self;
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

# GENERATORS
sub put {
    my($self, $data) = @_;
    $data =~ s{(?:([<>"'])|\&(?:($AMP);)?)}{
        $1 ? $XML_SPECIAL{$1} : $2 ? qq{\&$2;} : q{&amp;}
    }egmosx;
    $self->{result} .= $self->{blank} . $data;
    $self->{blank} = q{};
    $self->{prev_wtype} = $WTYPE_TEXT;
    return $self;
}

sub puts {
    my($self, $data) = @_;
    $self->{blank} = q{};
    if ($data eq q{}) {
        $self->{result} .= "\n";
        $self->{prev_wtype} = $WTYPE_NULL;
        return $self;
    }
    if ($self->{prev_wtype} == $WTYPE_TEXT
        && $self->{result} =~ m{[\x21-\x7e]\z}mosx
    ) {
        $self->{blank} = q{ };
    }
    elsif ($self->{prev_wtype} == $WTYPE_ETAG) {
        $self->{blank} = q{ };
    }
    return $self;
}

sub put_xml {
    my($self, $data) = @_;
    $self->{result} .= $self->{blank} . $self->escape_xml($data);
    $self->{blank} = q{};
    $self->{prev_wtype} = $WTYPE_TEXT;
    return $self;
}

sub put_raw {
    my($self, $data) = @_;
    $self->{result} .= $self->{blank} . $data;
    $self->{blank} = q{};
    $self->{prev_wtype} = $WTYPE_TEXT;
    return $self;
}

sub _start_block {
    my($self, $mark) = @_;
    $self->{result} .= $self->{blank} . $MARKUP{$mark}{'stag'};
    $self->{blank} = q{};
    $self->{prev_wtype} = $WTYPE_STAG;
    $self->{phrase} = {};
    return $self;
}

sub _end_block {
    my($self, $mark) = @_;
    $self->_phrase_flush;
    $self->{result} .= $MARKUP{$mark}{'etag'};
    $self->{blank} = q{};
    $self->{prev_wtype} = $WTYPE_ETAG;
    return $self;
}

sub _put_markup {
    my($self, $mark, $type) = @_;
    if ($type ne 'stag') {
        $self->{blank} = q{};
    }
    $self->{result} .= $self->{blank} . $MARKUP{$mark}{$type};
    $self->{blank} = q{};
    $self->{prev_wtype} = $type eq 'etag' ? $WTYPE_ETAG : $WTYPE_STAG;
    return $self;
}

sub escape_xml {
    my($self, $data) = @_;
    $data =~ s{([&<>"'])}{ $XML_SPECIAL{$1} }egmosx;
    return $data;
}

sub escape_text {
    my($self, $data) = @_;
    $data =~ s{(?:([<>"'])|\&(?:($AMP);)?)}{
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
    my $x = Digest::MurmurHash::murmur_hash($text);
    my $b36 = q{};
    for my $e (2176782336, 60466176, 1679616, 46656, 1296, 36, 1) {
        $b36 .= $BASE36[$x / $e];
        $x = $x % $e;
    }
    return $b36;
}

# BLOCK ACTIONS
# paragraphs
sub _start_p { return shift->_start_block('p') }
sub _end_p { return shift->_end_block('p') }

# horizontal rules
sub _insert_hr { return shift->_put_markup(q{----}, 'tag') }

# headings
sub _start_h {
    my($self, $data) = @_;
    ($self->{heading}) = $data =~ /\A(={1,6})/mosx;
    $self->{heading_pos} = length $self->{result};
    return $self->_start_block($self->{heading});
}

sub _end_h {
    my($self, $data) = @_;
    my $mark = delete $self->{heading};
    $self->_end_block($mark);
    return $self if ! defined $self->{toc};
    my $p = index $self->{result}, q{<h}, $self->{heading_pos};
    return $self if $p < 0;
    my $text = substr $self->{result}, $self->{heading_pos};
    chomp $text;
    $text =~ s/<.*?>//gmosx;
    return $self if ! $text;
    my $id = 'h' . $self->hash_base36($text);
    substr $self->{result}, $p + 3, 0, qq{ id="$id"};
    push @{$self->{tocinfo}}, [length $mark, $id, $text];
    return $self;
}

sub _list_toc {
    my($self) = @_;
    my $toc = (ref $self)->new;
    $toc->_put_markup('toc', 'stag');
    $toc->{list} = [];
    for my $info (@{$self->{tocinfo}}) {
        $toc->_insert_list(q{*} x $info->[0]);
        $toc->_insert_link($info->[2], "#$info->[1]", $info->[2]);
    }
    $toc->_end_list;
    $toc->_put_markup('toc', 'etag');
    return $toc;
}

# verbatims: block level nowiki "\n{{{\n...\n}}}\n"
sub _insert_verbatim {
    my($self, $data) = @_;
    ($data) = $data =~ m/\A\{\{\{\n(.*?)[\x20\t]*\n\}\}\}\n\z/mosx;
    $data =~ s/\A\x20\}\}\}/\}\}\}/mosx;
    $data =~ s/\n\x20\}\}\}/\n\}\}\}/gmosx;
    $self->_put_markup('verbatim', 'stag');
    $self->put_xml($data);
    $self->_put_markup('verbatim', 'etag');
    return $self;
}

# lists: "* ...", "# ...", "; ...\n: ..."
sub _start_list {
    my($self, $data) = @_;
    $self->{list} = [];
    $self->_phrase_clear;
    return $self->_insert_list($data);
}

# inline colon "; term : definition"
sub _insert_colon {
    my($self, $data) = @_;
    return $self->_insert_list(q[:] x $self->{list}[-1][0]);
}

sub _insert_list {
    my($self, $data) = @_;
    $self->_phrase_flush;
    my($mark) = $data =~ /\A([\*\#;:]+)/mosx;
    my $level = length $mark;
    $mark = substr $mark, 0, 1;
    while (@{$self->{list}} > 1 && $level < $self->{list}[-1][0]) {
        if ($self->{list}[-2][0] < $level) {
            $self->{list}[-1][0] = $level;
            last;
        }
        my $e = pop @{$self->{list}};
        $self->_put_markup($e->[1], 'etag');
    }
    if (! @{$self->{list}}) {
        $self->_put_markup($mark, 'stag');
        push @{$self->{list}}, [$level, $mark];
    }
    elsif ($self->{list}[-1][0] < $level) {
        my $prev = $self->{list}[-1][1];
        if ($prev eq q{;} && ($mark eq q{*} || $mark eq q{#})) {
            $self->_put_markup(q[;], q[:]);
            $self->{list}[-1][1] = q[:];
        }
        $self->puts(q{});
        $self->_put_markup($mark, 'stag');
        push @{$self->{list}}, [$level, $mark];
    }
    else {
        my $prev = $self->{list}[-1][1];
        $self->_put_markup($prev, $mark);
        @{$self->{list}[-1]} = ($level, $mark);
    }
    $self->_phrase_clear;
    return $self;
}

sub _end_list {
    my($self, $data) = @_;
    $self->_phrase_flush;
    while (@{$self->{list}}) {
        my $e = pop @{$self->{list}};
        $self->_put_markup($e->[1], 'etag');
    }
    $self->{list} = undef;
    return $self;
}

# tables: "|=..|..|..|"
sub _start_table {
    my($self, $data) = @_;
    $self->_put_markup(q[||], 'stag');
    ($self->{table}) = $data =~ /\A(\|=?)/mosx;
    return $self->_start_block($self->{table});
}

sub _insert_tr {
    my($self, $data) = @_;
    $self->_end_block($self->{table});
    $self->_put_markup(q[||], q[||]);
    ($self->{table}) = $data =~ /\A(\|=?)/mosx;
    return $self->_start_block($self->{table});
}

sub _insert_td {
    my($self, $data) = @_;
    $self->_end_block($self->{table});
    ($self->{table}) = $data =~ /\A(\|=?)/mosx;
    return $self->_start_block($self->{table});
}

sub _end_table {
    my($self, $data) = @_;
    $self->_end_block($self->{table});
    $self->_put_markup(q[||], 'etag');
    $self->{table} = undef;
    return $self;
}

# indented paragraphs: ": ...", "> ..."
sub _start_indent {
    my($self, $data) = @_;
    $self->{indent} = 0;
    return $self->_insert_indent($data);
}

sub _insert_indent {
    my($self, $data) = @_;
    $data =~ s/$S+//mosx;
    my $level = length $data;
    while ($self->{indent} > $level) {
        $self->_put_markup(q{>}, 'etag');
        --$self->{indent};
    }
    if ($self->{indent} < $level) {
        while ($self->{indent} < $level) {
            $self->_put_markup(q{>}, 'stag');
            ++$self->{indent};
        }
    }
    else {
        $self->{indent} = $level;
    }
    return $self;
}

sub _end_indent {
    my($self, $data) = @_;
    return $self->_insert_indent(q{});
}

# INLINE ACTION
# phrases: bold("**"), italic("//"),
#   monospace("##"), superscript("^^"), subscript(",,"), underline("__")
sub _phrase_clear {
    my($self) = @_;
    $self->{phrase} = {};
    $self->{phrase_stack} = [];
    return $self;
}

sub _insert_phrase {
    my($self, $mark) = @_;
    if (! $self->{phrase}{$mark}) {
        $self->{phrase}{$mark} = 1;
        push @{$self->{phrase_stack}}, $mark;
        $self->_put_markup($mark, 'stag');
    }
    elsif ($self->{phrase_stack}[-1] eq $mark) {
        $self->{phrase}{$mark} = 0;
        pop @{$self->{phrase_stack}};
        $self->_put_markup($mark, 'etag');
    }
    else {
        $self->put($mark);
    }
    return $self;
}

sub _phrase_flush {
    my($self) = @_;
    return $self if ! $self->{phrase_stack} || ! @{$self->{phrase_stack}};
    while (my $mark = pop @{$self->{phrase_stack}}) {
        $self->_put_markup($mark, 'etag');
    }
    $self->{phrase} = {};
    return $self;
}

# break lines: "\\\\"
sub _insert_br { return shift->_put_markup(q{\\\\}, 'tag') }

# inline nowikis: "{{{...}}}"
sub _insert_nowiki {
    my($self, $data) = @_;
    my($text) = $data =~ /\A\{\{\{$S*(.*?)$S*\}\}\}\z/mosx;
    $self->_put_markup('nowiki', 'stag');
    $self->put_xml($text);
    $self->_put_markup('nowiki', 'etag');
    return $self;
}

# an escaped mark and an escaped character: "~..."
sub _insert_escaped {
    my($self, $data) = @_;
    my $text = length $data == 1 ? $data : (substr $data, 1);
    $self->put($text);
    return $self;
}

# placeholders: "<<< ... >>>"
sub _insert_placeholder {
    my($self, $data) = @_;
    $self->_put_markup(q{<<<}, 'stag');
    $self->put_xml($data);
    $self->_put_markup(q{<<<}, 'etag');
    return $self;
}

# plugin calls: "<< ... >>"
sub _insert_plugin {
    my($self, $source) = @_;
    return $self if $self->{plugin_run}; # avoid recursive calls
    local $self->{plugin_run} = 1; ## no critic qw(LocalVars)
    my $visitor = $self->{plugin_visitor} || $self;
    my $plugin = $visitor->visit_plugin($source, $self);
    if ($plugin && $plugin->{runtime}) {
        ## no critic qw(Interpolation)
        my $proc= q{$v->_build_plugin($v->visit_plugin('}
            . $self->escape_quote($source)
            . q{',$v))};
        $self->put_raw(qq{';\n\$t .= $proc;\n\$t .= '});
    }
    else {
        $self->put_raw($self->_build_plugin($plugin));
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

# links: "[[ url | description ]]"
sub _insert_bracketed {
    my($self, $source) = @_;
    if ($source =~ /\A\[\[$S*([^\|]*?)$S*(?:\|$S*(.*?)$S*)?\]\]\z/mosx) {
        return $self->_insert_link($source, $1, defined $2 ? $2 : $1);
    }
    return $self->put($source);
}

# freestand links: url and CamelCased wiki words
sub _insert_freestand {
    my($self, $link) = @_;
    return $self->_insert_link($link, $link, $link);
}

sub _insert_link {
    my($self, $source, $link, $text) = @_;
    my $visitor = $self->{link_visitor} || $self;
    my $anchor = $visitor->visit_link($link, $text, $self);
    if ($anchor && $self->type eq 'perl' && $anchor->{runtime}) {
        ## no critic qw(Interpolation)
        my $proc = q{$v->_build_a_element(}
            . q{'} . $self->escape_quote($source) . q{',}
            . q{$v->visit_link(}
                . q{'} . $self->escape_quote($link) . q{',}
                . q{'} . $self->escape_quote($text) . q{',}
                . q{$v}
            . q{)}
        . q{)};
        $self->put_raw(qq{';\n\$t .= $proc;\n\$t .= '});
    }
    elsif ($anchor && ($anchor->{name} || $anchor->{href})) {
        $self->put_raw($self->_build_a_element($source, $anchor));
    }
    else {
        $self->put($source);
    }
    return $self;
}

sub _build_a_element {
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

# images: "{{ url | description }}"
sub _insert_braced {
    my($self, $data) = @_;
    if ($data =~ /\A\{\{$S*([^\|]*?)$S*(?:\|$S*(.*?)$S*)?\}\}\z/mosx) {
        my($link, $title) = ($1, $2);
        my $visitor = $self->{link_visitor} || $self;
        my $image = $visitor->visit_image($link, $title, $self);
        if (! $image || ! $image->{src}) {
            return $self->put($data);
        }
        my $attr = q{ src="} . $self->escape_uri($image->{src}) . q{"};
        for my $k (qw(id class alt title)) {
            next if ! defined $image->{$k};
            $attr .= qq{ $k="} . $self->escape_text($image->{$k}) . q{"};
        }
        $self->put_raw(qq{<img$attr />});
        return $self;
    }
    return $self->put($data);
}

1;

__END__

=pod

=head1 NAME

Text::Creolize - A practical converter for WikiCreole to XHTML.

=head1 VERSION

0.015

=head1 SYNOPSIS

    use Text::Creolize;
    use File::Slrup;
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

=item C<< $creolize->put($string) >>

Appends string with escaping as a parsed XML TEXT.
The utf8 flag of the string should be turned on.

=item C<< $creolize->puts >>

Appends end of line mark or blank.

=item C<< $creolize->put_xml($string) >>

Appends string with escaping as an unparsed XML TEXT.
The utf8 flag of the string should be turned on.

=item C<< $creolize->put_raw($string) >>

Appends string without escapings.
The utf8 flag of the string should be turned on.

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
L<Digest::MurmurHash>

=head1 SEE ALSO

L<Text::WikiCreole>
L<http://www.wikicreole.org/wiki/Creole1.0>
L<http://www.wikicreole.org/wiki/CreoleAdditions>

=head1 AUTHOR

MIZUTANI Tociyuki  C<< <tociyuki@gmail.com> >>

=head1 LICENSE AND COPYRIGHT

Copyright (c) 2010, MIZUTANI Tociyuki C<< <tociyuki@gmail.com> >>.
All rights reserved.

This module is free software; you can redistribute it and/or
modify it under the same terms as Perl itself.

=cut

