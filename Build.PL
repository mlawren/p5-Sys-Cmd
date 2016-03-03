#!perl
use strict;
use warnings;

use Module::Build;

my $builder = Module::Build->new(
    module_name       => 'Sys::Cmd',
    create_license    => 1,
    create_readme     => 1,
    license           => 'perl',
    dist_author       => q{Mark Lawrence <mlawren@cpan.org>},
    dist_version_from => 'lib/Sys/Cmd.pm',
    requires          => {
        'Carp'            => 0,
        'Exporter::Tidy'  => 0,
        'File::Which'     => 0,
        'File::chdir'     => 0,
        'IO::Handle'      => 0,
        'Log::Any'        => 0,
        'Proc::FastSpawn' => 0,
    },
    test_requires => {
        'Cwd'          => 0,
        'Data::Dumper' => 0,
        'File::Temp'   => 0,
        'File::Spec'   => 0,
        'Test::More'   => 0,
    },
    configure_requires => {
        'Module::Build' => '0.4004',
    },
    recursive_test_files => 1,
    add_to_cleanup       => ['Sys-Cmd-*'],
    meta_merge           => {
        resources => {
            bugtracker => 'https://github.com/mlawren/p5-Sys-Cmd/issues',
            repository => 'git://github.com/mlawren/p5-sys-cmd.git',
            homepage   => 'https://github.com/mlawren/p5-Sys-Cmd',
        },
        keywords => [],
    },
);

$builder->create_build_script();
