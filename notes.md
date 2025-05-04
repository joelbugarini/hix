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

# features
lets add a new feature to the project, help me follow the due process. first let me explain how that should work. I want the cli to take a model and the config file to generate all the files based of on the listed templates. also it will be possible to just pass a layer name to just generate those files. 

i want to understand the process i need to follow before coding. for example rising an issue in github. help me with that
