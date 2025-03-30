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
