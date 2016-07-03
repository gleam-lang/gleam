Tokenization
============

Tokenization is breaking down a body of text into the smallest possible parts.

If we were tokenizing English "Hello, world!" could become

- word "Hello"
- comma ","
- space " "
- word "world"
- bang "!"

Potion will have various different types of token:

- keywords: `module`, `public`, `private`
- delimiters: `(`, `)`, `{`, `}`, `[`, `]`, `.`, `,`
- numbers: `10`, `43.21`
- strings: `"Hello!"`
- indetifiers: `lists`, `maps`
- atoms: `:ok`, `:error`
