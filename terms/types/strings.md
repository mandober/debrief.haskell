# Strings

- string type, `String`, is just a type alias for `[Char]`, list of `Char`s
- String is thus entangled with two types, `Char` and list `[]`
- strings are lists, i.e. sequences of characters implemented as linked-list
- the empty string: `"" == []`
- character literals ('') may not be empty
- convert Char to String: `[ch]` if `ch` is a var, `['w']` with a literal
- convert String to Char: PM with (x:xs), or fns like `head "abc"`
- `type String = [Char]` so no conversion is necessary, just select an element
- Strings under concatenation form a Monoid, `(++) == (<>)`, with ϵ as identity
- Chars don't because there's no empty Char; '' is disallowed syntax
- Strings are impl as linked list so they're inefficient.
- A better type is `Text` that uses unicode encodings, commonly UTF-8.
