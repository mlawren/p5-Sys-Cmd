# Load the Module::Install bundled in ./inc/
use inc::Module::Install;

name 'Sys-Cmd';
license 'Perl';
all_from 'lib/Sys/Cmd.pm';

requires 'Moo'           => 0;
requires 'Carp'          => 0;
requires 'Cwd'           => 0;
requires 'IO::Handle'    => 0;
requires 'IPC::Open3'    => 0;
requires 'Log::Any'      => 0;
requires 'Sub::Exporter' => 0;
requires 'File::Which'   => 0;

build_requires 'Test::More' => 0;

repository 'http://github.com/mlawren/sys-cmd.git/';

auto_install;

WriteAll;

