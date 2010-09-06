use Data::Dumper;

my $TOKEN_DISPATCH = {
    'EOF' => [
        [undef],
        [undef, '_end_p'], [undef, '_end_p'],
        [undef, '_end_h'],
        [undef, '_end_list'], [undef, '_end_list'],
        [undef, '_end_table'], [undef, '_end_table'], [undef, '_end_table'],
        [undef, '_end_p', '_end_indent'], [undef, '_end_p', '_end_indent'],
        [undef, '_end_list'], [undef, '_end_list'],
    ],
    'EOL' => [
        [0],
        [2, 'puts'], [0, '_end_p'],
        [0, '_end_h'],
        [5, 'puts'], [0, '_end_list'],
        [8], [8], [0, '_end_table'],
        [10, 'puts'], [0, '_end_p', '_end_indent'],
        [12, 'puts'], [0, '_end_list'],
    ],
    'VERBATIM' => [map {(
        [0, $_],
        [0], [0, '_end_p', $_],
        [0],
        [0], [0, '_end_list', $_],
        [0], [0], [0, '_end_table', $_],
        [0], [0, '_end_p', '_end_indent', $_],
        [0], [0, '_end_list', $_],
    )} '_insert_verbatim'],
    'HRULE' => [map {(
        [0, $_],
        [0], [0, '_end_p', $_],
        [0],
        [0], [0, '_end_list', $_],
        [0], [0], [0, '_end_table', $_],
        [0], [0, '_end_p', '_end_indent', $_],
        [0], [0, '_end_list', $_],
    )} '_insert_hr'],
    'HEADING' => [map{(
        [3, $_],
        [1, 'put'], [3, '_end_p', $_],
        [0, '_end_h'],
        [4, 'put'], [3, '_end_list', $_],
        [6, 'put'], [0], [3, '_end_table', $_],
        [9, 'put'], [3, '_end_p', '_end_indent', $_],
        [11, 'put'], [3, '_end_list', $_],
    )} '_start_h'],
    'JUSTLIST' => [map{(
        [4, $_],
        [0], [4, '_end_p', $_],
        [0],
        [0], [4, '_insert_list'],
        [0], [0], [4, '_end_table', $_],
        [0], [4, '_end_p', '_end_indent', $_],
        [0], [4, '_insert_list'],
    )} '_start_list'],
    'MAYBELIST' => [map{(
        [1, '_start_p', $_],
        [1, $_], [1, $_],
        [3, $_],
        [4, $_], [4, '_insert_list'],
        [6, $_], [0], [1, '_end_table', '_start_p', $_],
        [9, $_], [10, $_],
        [11, $_], [4, '_insert_list'],
    )} '_insert_phrase'],
    'TD' => [map{(
        [6, $_],
        [1, 'put'], [6, '_end_p', $_],
        [3, 'put'],
        [4, 'put'], [6, '_end_list', $_],
        [6, '_insert_td'], [0], [6, '_insert_tr'],
        [9, 'put'], [6, '_end_p', '_end_indent', $_],
        [11, 'put'], [6, '_end_list', $_],
    )} '_start_table'],
    'ENDTR' => [
        [0],
        [1, 'put'], [0],
        [3, 'put'],
        [4, 'put'], [0],
        [7], [0], [0],
        [9, 'put'], [0],
        [11, 'put'], [0],
    ],
    'TERM' => [map{(
        [11, $_],
        [0], [11, '_end_p', $_],
        [0],
        [0], [11, '_insert_list'],
        [0], [0], [11, '_end_table', $_],
        [0], [11, '_end_p', '_end_indent', $_],
        [0], [11, '_insert_list'],
    )} '_start_list'],
    'DESC' => [map{(
        [9, $_, '_start_p'],
        [1, 'put'], [9, '_end_p', $_, '_start_p'],
        [3, 'put'],
        [4, 'put'], [4, '_insert_list'],
        [6, 'put'], [0], [9, '_end_table', $_, '_start_p'],
        [9, 'put'], [9, '_end_p', '_insert_indent', '_start_p'],
        [4, '_insert_colon'], [4, '_insert_list'],
    )} '_start_indent'],
    'QUOTE' => [map{(
        [9, $_, '_start_p'],
        [0], [9, '_end_p', $_, '_start_p'],
        [0],
        [0], [9, '_end_list', $_, '_start_p'],
        [0], [0], [9, '_end_table', $_, '_start_p'],
        [0], [9, '_end_p', '_insert_indent', '_start_p'],
        [0], [9, '_end_list', $_, '_start_p'],
    )} '_start_indent'],
    'BLANK' => [
        [0],
        [1, 'puts'], [2],
        [3, 'puts'],
        [4, 'puts'], [5],
        [6, 'puts'], [0], [8],
        [9, 'puts'], [10],
        [11, 'puts'], [12],
    ],
    (map {
        $_->[0] => [
            [1, '_start_p', $_->[1]],
            [1, $_->[1]], [1, $_->[1]],
            [3, $_->[1]],
            [4, $_->[1]], [4, $_->[1]],
            [6, $_->[1]], [0], [1, '_end_table', '_start_p', $_->[1]],
            [9, $_->[1]], [9, $_->[1]],
            [11, $_->[1]], [11, $_->[1]],
        ]
    }   ['PHRASE' => '_insert_phrase'], ['BREAK' => '_insert_br'],
        ['NOWIKI' => '_insert_nowiki'], ['BRACKETED' => '_insert_bracketed'],
        ['BRACED' => '_insert_braced'], ['PLACEHOLDER' => '_insert_placeholder'],
        ['PLUGIN' => '_insert_plugin'],   ['FREESTAND' => '_insert_freestand'],
        ['ESCAPE' => '_insert_escaped'], ['TEXT' => 'put'],
    ),
};

my @S = qw(
   block paragraph paranext heading list listnext
   table tdnext tablenext indent indentnext
   term termnext
);
my $B = [];
my @K = qw(
    EOF EOL VERBATIM HRULE HEADING JUSTLIST MAYBELIST TD ENDTR TERM DESC QUOTE
    BLANK PHRASE BREAK NOWIKI BRACKETED BRACED PLACEHOLDER PLUGIN FREESTAND
    ESCAPE TEXT
);
print "my \$TOKEN_DISPATCH = {\n";
for my $i (0 .. $#{$TOKEN_DISPATCH->{$K[0]}}) {
    print "    '$S[$i]' => {\n";
    for my $k (@K) {
        print "        '$k' => [";
        my($succ, @actions) = @{$TOKEN_DISPATCH->{$k}[$i]};
        if (defined $succ) {
            $succ = $S[$succ];
        }
        print join q{, }, map { defined $_ ? "'$_'" : 'undef' } $succ, @actions;
        print "],\n";
    }
    print "    },\n";
}
print "};\n";

