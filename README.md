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

## Parser Generator

Todo:  experiment with using a closure to encapsulate "rules" and "remaining program".  extract rule records to own namespace.  support nested rules(!!!).  stop pointing nodes at rule records, instead give them rule names.

## License

Copyright © 2014 FIXME

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
