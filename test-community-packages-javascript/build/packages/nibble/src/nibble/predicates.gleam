///
pub fn is_digit (grapheme: String) -> Bool {
    case grapheme {
        "0" | "1" | "2" | "3" | "4" | 
        "5" | "6" | "7" | "8" | "9" ->
            True
        
        _ ->
            False
    }
}

///
pub fn is_whitespace (grapheme: String) -> Bool {
    case grapheme {
        " " | "\t" | "\n" | "\r" ->
            True
        
        _ ->
            False
    }
}

///
pub fn is_upper (grapheme: String) -> Bool {
    case grapheme {
        "A" | "B" | "C" | "D" | "E" | 
        "F" | "G" | "H" | "I" | "J" | 
        "K" | "L" | "M" | "N" | "O" | 
        "P" | "Q" | "R" | "S" | "T" | 
        "U" | "V" | "W" | "X" | "Y" | 
        "Z" ->
            True
        
        _ ->
            False
    }
}

///
pub fn is_lower (grapheme: String) -> Bool {
    case grapheme {
        "a" | "b" | "c" | "d" | "e" | 
        "f" | "g" | "h" | "i" | "j" | 
        "k" | "l" | "m" | "n" | "o" | 
        "p" | "q" | "r" | "s" | "t" | 
        "u" | "v" | "w" | "x" | "y" | 
        "z" ->
            True
        
        _ ->
            False
    }
}

///
pub fn is_alphanum (grapheme: String) -> Bool {
    is_digit(grapheme) || is_upper(grapheme) || is_lower(grapheme)
}
