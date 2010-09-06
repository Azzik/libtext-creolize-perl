use Test::More;

eval {
    require Test::Perl::Critic;
    my @arg;
    push @arg, -profile => 't/perlcriticrc' if -e 't/perlcriticrc';
    Test::Perl::Critic->import(@arg, -theme => 'pbp')
}; if ($@) {
    plan skip_all => 'Test::Perl::Critic is not installed.';
}

Test::Perl::Critic::all_critic_ok();

