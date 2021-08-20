# 2.6 Character and String Literals

```
escape  = \ ( charesc | ascii | decimal | o octal | x hexadecimal )
charesc = `a b f n r t v \ " ' &`
cntrl   = ascLarge | `@ [ \ ] | ^ _`
ascii   = ^cntrl
| NUL | SOH | STX | ETX | EOT | ENQ | ACK | BEL     
| BS  | HT  | LF  | VT  | FF  | CR  | SO  | SI      
| DLE | DC1 | DC2 | DC3 | DC4 | NAK | SYN | ETB     
| CAN | EM  | SUB | ESC | FS  | GS  | RS  | US    
| SP     
| DEL     
(0-31, 32=SP, 127=DEL)
```

* Character literals are written between single quotes, and strings between double quotes.

* Escape codes may be used in characters and strings to represent special characters. Note that a single quote ' may be used in a string, but must be escaped in a character; similarly, a double quote " may be used in a character, but must be escaped in a string. \\ must always be escaped. The category charesc also includes portable representations for the characters "alert" (\\a), "backspace" (\\b), "form feed" (\\f), "new line" (\\n), "carriage return" (\\r), "horizontal tab" (\\t), and "vertical tab" (\\v).

Escape characters for the Unicode character set, including control characters such as \\^X, are also provided. Numeric escapes such as \\137 are used to designate the character with decimal representation 137; octal (e.g. \\o137) and hexadecimal (e.g. \\x37) representations are also allowed.

Consistent with the "maximal munch" rule, numeric escape characters in strings consist of all consecutive digits and may be of arbitrary length. Similarly, the one ambiguous ASCII escape code, "\\SOH", is parsed as a string of length 1. The escape character \\& is provided as a "null character" to allow strings such as "\\137\\&9" and "\\SO\\&H" to be constructed (both of length two). Thus "\\&" is equivalent to "" and the character '\\&' is disallowed. Further equivalences of characters are defined in Section 6.1.2.

A string may include a "gap"-two backslants enclosing white characters-which is ignored. This allows one to write long strings on more than one line by writing a backslant at the end of one line and at the start of the next. For example,

"Here is a backslant \\\\ as well as \\137, \\    
    \\a numeric escape character, and \\^X, a control character."

String literals are actually abbreviations for lists of characters (see Section 3.7).
