# hix

## develop commands

stack build
stack exec hix template.cs.hix Person.json

## highlight syntax commands

stack exec hix -- --gen-grammar util/syntaxes/hix.tmLanguage.json
vsce package
code --install-extension hix-syntax-0.0.7.vsix

## testing commands

stack test

## documentation commands

python -m mkdocs serve
python -m mkdocs gh-deploy

## installation commands

### Build and install locally
stack build
stack install

### Windows installer
Run inno app to build the installer (utils/installer/hix-installer.iss)
Output: utils/installer/output/hix-setup-0.3.5.4.exe

### Fedora/RPM package
cd utils/packaging
./build-rpm.sh
Output: utils/packaging/rpmbuild/RPMS/x86_64/hix-0.3.5.4-1.x86_64.rpm

## version update files
Files that need version updates:
- hix.cabal (source of truth)
- test/data/version.golden
- utils/installer/hix-installer.iss
- utils/packaging/hix.spec
- src\CLI\Help.hs
- test\HelpSpec.hs

# features
lets add a new feature to the project, help me follow the due process. first let me explain how that should work. 

i want to understand the process i need to follow before coding. for example rising an issue in github. help me with that
