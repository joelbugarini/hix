# hix
stack build
stack exec hix template.cs.hix Person.json

stack exec hix -- --gen-grammar util/syntaxes/hix.tmLanguage.json
vsce package
code --install-extension hix-syntax-0.0.7.vsix

stack test