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
stack build
stack install
Run inno app to build the installer

## version update files
Files that need version updates:
- hix.cabal (source of truth)
- test/data/version.golden
- utils/installer/hix-installer.iss

# features
lets add a new feature to the project, help me follow the due process. first let me explain how that should work. 

i want to understand the process i need to follow before coding. for example rising an issue in github. help me with that
