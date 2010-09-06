use strict;
use warnings;
use ExtUtils::MakeMaker;

WriteMakefile(
    NAME                => 'Text::Creolize',
    AUTHOR              => 'MIZUTANI Tociyuki <tociyuki@gmail.com>',
    VERSION_FROM        => 'lib/Text/Creolize.pm',
    ABSTRACT_FROM       => 'lib/Text/Creolize.pm',
    PL_FILES            => {},
    PREREQ_PM => {
        'Test::Base' => 0,
        'Encode' => 0,
        'Digest::MurmurHash' => 0,
    },
    dist                => { COMPRESS => 'gzip -9f', SUFFIX => 'gz', },
);
