module Form (Piece) where
data Exposition  = Exposition
data Development = Development
data Recap  = Recap
data Section= Section
data Var    = Var

data Piece = Sonata Exposition Development Recap
            | Rondo Section Section Section
            | ThemeVar Section Var Var Var
