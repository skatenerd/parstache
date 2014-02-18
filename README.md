# parstache

An implementation of Mustache Templates using EBNF

## Usage
```
 (render
   "immediate families are:  {{#parents}}{{name}}: {{#children}}{{name}} {{/children}}{{/parents}}"
   {"parents" [{"name" "laura" "children" [{"name" "logan"} {"name" "katie"}]}
               {"name" "linda" "children" [{"name" "rutledge"} {"name" "walter"}]}]})

```

Will Yield:
```
 "immediate families are:  laura: logan katie linda: rutledge walter "
```

## Missing Features
*  HTML Escaping
*  HTML Unescaping
*  Inverted Sections
*  Lambdas

## What's with this parser generator thing?
It's used to generate a function which parses mustache input.  But you can give it whatever grammar you want to give it.

####Todo:  
* support nested rules(!!!).
* make it faster
* write rules to parse actual EBNF input


## WHY??
For fun.  And Because I can.

## License

Copyright © 2014

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
