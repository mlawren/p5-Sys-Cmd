# Load the Module::Install bundled in ./inc/
use inc::Module::Install;

name 'Sys-Cmd';
license 'Perl';
version '0.81.0';
all_from 'lib/Sys/Cmd.pm';
readme_from 'lib/Sys/Cmd.pod';

homepage 'https://github.com/mlawren/p5-Sys-Cmd';
bugtracker 'https://github.com/mlawren/p5-Sys-Cmd/issues';
repository 'git://github.com/mlawren/p5-Sys-Cmd.git';

requires 'Carp'           => 0;
requires 'Exporter::Tidy' => 0;
requires 'IO::Handle'     => 0;
requires 'Log::Any'       => 0;
requires 'POSIX'          => 0;
requires 'File::Which'    => 0;
requires 'File::chdir'    => 0;

test_requires 'Cwd'          => 0;
test_requires 'Data::Dumper' => 0;
test_requires 'File::Temp'   => 0;
test_requires 'File::Spec'   => 0;
test_requires 'Test::More'   => 0;

tests_recursive 't';

auto_install;

WriteAll;

