use Test::More;

eval {
    require Test::Pod::Coverage;
}; if ($@) {
    plan skip_all => "Test::Pod::Coverage 1.04 required for testing POD coverage";
}
Test::Pod::Coverage::all_pod_coverage_ok();

