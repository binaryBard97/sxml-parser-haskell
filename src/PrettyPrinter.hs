-- Pretty Printer
{-# OPTIONS -Wno-name-shadowing #-}

{-
References::

Wadler, Phil.  (2003) "A Prettier Printer."  In _The Fun of
Programming_, Gibbons, Jeremy and de Moor, Oege (eds).
http://homepages.inf.ed.ac.uk/wadler/topics/language-design.html#prettier

Text.PrettyPrint.Leijen
http://hackage.haskell.org/package/wl-pprint

-}

-- ###################################################################
-- ###################################################################

{- ORMOLU_DISABLE -}
module PrettyPrinter (
  Doc {- instance Semigroup, Monoid -},
  empty, (<+>), text, nest, line, lbreak, group,
  sline, slbreak, (<@>), (<#>), (<##>), (<!>), (<!!>),
  pack, folddoc, hsep, spread, vsep, stack, sep, fillSep, hcat, vcat, cat, fillCat, sepLBy, sepRBy,
  render, prender, pretty, pprint,
) where
{- ORMOLU_ENABLE -}

-- ###################################################################
-- ###################################################################

-- Core --

data Doc
  = -- `Empty`: the empty document.
    Empty
  | -- `Text s d`: the string `s` followed by the document `d`.
    -- invariant: text does not contain '\n'
    Text String Doc
  | -- `Line b i d`: possible line break before the document `d`.
    -- If line break taken, then indent document `d` by `i` spaces.
    -- If line break not taken, then `b` indicated whether or not to prefix
    -- document `d` by a single space.
    Line Bool Int Doc
  | -- `Union d1 d2`: choice between document `d1` and document `d2`
    -- Informally, `d1` and `d2` should describe the same content,
    -- but with different layouts; `d1` is preferred over `d2`
    -- when both fit within the available width.
    -- invariant: `flatten d1 == flatten d2`
    -- and first lines of `d1` no shorter than first lines of `d2`
    Union Doc Doc
  deriving (Eq, Show)

-- The empty document; equivalent to `text ""`
-- Wadler's `nil`.
empty :: Doc
empty = Empty

-- The document `x <+> y` concatenates document `x` and document `y`.
-- It is an associative operation having `empty` as a left and right unit.
-- Wadler's `(<>)` (and provided under that name by the `Semigroup` instance).
infixr 6 <+>

(<+>) :: Doc -> Doc -> Doc
Empty <+> d' = d'
(Text s d) <+> d' = Text s (d <+> d')
(Line b j d) <+> d' = Line b j (d <+> d')
(Union d1 d2) <+> d' = Union (d1 <+> d') (d2 <+> d')

-- `empty` and `(<+>)` make `Doc` an instance of `Semigroup` and `Monoid`.
instance Semigroup Doc where
  (<>) = (<+>)

instance Monoid Doc where
  mempty = empty

-- The document `text s` is the literal `s`.
-- The string should not contain any newline ('\n') characters.
text :: String -> Doc
text "" = Empty
text s = Text s Empty

-- The document `nest i d` adds indentation `i` to the document `d`.
-- Alternatively, `nest i d` will print `i` spaces after every line break in document `d`.
nest :: Int -> Doc -> Doc
nest _ Empty = Empty
nest i (Text s d) = Text s (nest i d)
nest i (Line b j d) = Line b (i + j) (nest i d)
nest i (Union d1 d2) = Union (nest i d1) (nest i d2)

-- The document `line` denotes a line break.
-- The document `line` behaves like `text " "` if the line break is undone by `group`.
line :: Doc
line = Line True 0 Empty

-- The document `lbreak` denotes a line break.
-- The document `lbreak` behaves like `empty` if the line break is undone by `group`.
lbreak :: Doc
lbreak = Line False 0 Empty

-- The combinator `group`  is used to "choose" between alternative layouts.
-- The document `group d` chooses the "prettier" of
--  * the document `d` with all line breaks removed, and
--  * the document `d` (with any specified line breaks).
-- NOTE: `Union` invariant is satisfied.
group :: Doc -> Doc
group d = Union (flatten d) d

-- The document `flatten d` is the document `d` with all line breaks removed.
-- NOTE: `flatten` is not exported by the library.
-- NOTE: `Union` invariant implies it suffices to flatten `d1`.
flatten :: Doc -> Doc
flatten Empty = Empty
flatten (Text s d) = Text s (flatten d)
flatten (Line b _ d) = if b then Text " " (flatten d) else flatten d
flatten (Union d1 _) = flatten d1

-- `best w k d` calculates the "prettiest" version of document `d` that fits in
-- the available width `w`, where `k` characters have already been placed on the
-- current line.
--
-- "Prettiest" is defined by specifing an ordering relation between lines and
-- extending this lexically to an ordering between versions of a document.
--
-- The ordering relation between lines is with respect to an available width.
--
-- If both lines are shorter than the available width, then the longer one is
-- better.
--
-- If one line fits in the available width and the other does not,
-- then the one that fits is better.
-- If both lines are longer than the available width,
-- then the shorter one is better.
-- [A "pretty" layout may exceed the available width, but only if unavoidable.]
--
-- NOTE: `best w k d` returns a document with no `Union`s (all "choices" have been made).
-- NOTE: The `Union` invariant is critical to efficiently choose the better version.
--
-- See Wadler's paper for an even more efficient implementation of documents.
best :: Int -> Int -> Doc -> Doc
best _ _ Empty = Empty
best w k (Text s d) = Text s (best w (k + length s) d)
best w _ (Line b j d) = Line b j (best w j d)
best w k (Union d1 d2) = better w k (best w k d1) (best w k d2)
  where
    better w k d1 d2 = if fits (w - k) d1 then d1 else d2

    fits w _ | w < 0 = False
    fits _ Empty = True
    fits w (Text s d) = fits (w - length s) d
    fits _ (Line _ _ _) = True
    fits w (Union _ d) = fits w d -- but, `fits` will never be applied to `Union`

-- `render d` converts document `d` (with any specified line breaks) to a string.
-- Wadler's `layout`.
render :: Doc -> String
render Empty = ""
render (Text s d) = s ++ render d
render (Line _ i d) = "\n" ++ replicate i ' ' ++ render d
render (Union _ d2) = render d2 -- the rdoc is "unflattened" (retains line breaks)

-- The document `pack ds` collapses a list of documents into a document.  It puts a space
-- between two documents when this leads to reasonable layout and a linebreak otherwise.
-- Wadler's `fill`.
-- NOTE: `Union` invariant is satisfied.
pack :: [Doc] -> Doc
pack [] = empty
pack [d] = d
pack (d1 : d2 : ds) =
  Union
    (flatten d1 <+> text " " <+> pack (flatten d2 : ds))
    (d1 <+> line <+> pack (d2 : ds))

-- ###################################################################
-- ###################################################################

-- Derived --

-- `pretty w d` converts document `d` to a `String` with page width `w`.
pretty :: Int -> Doc -> String
pretty w d = render (best w 0 d)

-- `pprint w d` prints the document `d` to stdout with page width `w`.
pprint :: Int -> Doc -> IO ()
pprint w d = putStrLn (pretty w d)

-- `prender d` prints the document `d` (with any specified line breaks) to stdout.
prender :: Doc -> IO ()
prender d = putStrLn (render d)

-- ###################################################################

-- The document `sline` ("soft line") behaves like `text " "`
-- if the resulting output fits the page, otherwise it behaves like `line`.
sline :: Doc
sline = group line

-- The document `slbreak` ("soft lbreak") behaves like `empty`
-- if the resulting output fits the page, otherwise it behaves like `break`.
slbreak :: Doc
slbreak = group lbreak

-- The document `x <@> y` concatenates document `x` and `y` with `text " "` in between.
-- Mnemonic: "@" is "wide" (a space).
infixr 6 <@>

(<@>) :: Doc -> Doc -> Doc
d1 <@> d2 = d1 <+> text " " <+> d2

-- The document `x <#> y` concatenates document `x` and `y` with `line` in between.
-- Mnemonic: "#" is "wide" (a line break or a space).
infixr 5 <#>

(<#>) :: Doc -> Doc -> Doc
d1 <#> d2 = d1 <+> line <+> d2

-- The document `x <##> y` concatenates document `x` and `y` with `sline` in between.
-- Mnemonic: "##" is "wide" (a line break or a space).
-- This effectively puts `x` and `y` either next to each other (with a space in between) or underneath each other.
-- Wadler's `(<+/>)`.
infixr 5 <##>

(<##>) :: Doc -> Doc -> Doc
d1 <##> d2 = d1 <+> sline <+> d2

-- The document `x <!> y` concatenates document `x` and `y` with `lbreak` in between.
-- Mnemonic: "!" is "thin" (a line break or empty).
infixr 5 <!>

(<!>) :: Doc -> Doc -> Doc
d1 <!> d2 = d1 <+> lbreak <+> d2

-- The document `x <!!> y` concatenates document `x` and `y` with `slbreak` in between.
-- Mnemonic: "!!" is "thin" (a line break or empty).
-- This effectively puts `x` and `y` either right next to each other or underneath each other.
infixr 5 <!!>

(<!!>) :: Doc -> Doc -> Doc
d1 <!!> d2 = d1 <+> slbreak <+> d2

-- Combine a list of documents in a right-fold-like manner.
folddoc :: (Doc -> Doc -> Doc) -> [Doc] -> Doc
folddoc _ [] = empty
folddoc _ [d] = d
folddoc f (d : ds) = d `f` folddoc f ds

-- The document `hsep ds` concatenates all documents `ds` horizontally with `(<@>)`.
-- Wadler's `spread`.
hsep, spread :: [Doc] -> Doc
hsep = folddoc (<@>)
spread = hsep

-- The document `vsep ds` concatenates all documents `ds` vertically with `(<#>)`.
-- If a `group` undoes the line breaks inserted by `vsep`, all documents are separated with a space.
-- Wadler's `stack`.
vsep, stack :: [Doc] -> Doc
vsep = folddoc (<#>)
stack = vsep

-- The document `sep ds` concatenates all documents `ds` either horizontally with `(<@>)`, if it fits the page, or vertically.
sep :: [Doc] -> Doc
sep = group . vsep

-- The document `fillSep ds` concatenates documents `ds` horizontally with `(<@>)` as long as its fits the page,
-- then inserts a `line` and continues doing that for all remaining documents in `ds`.
fillSep :: [Doc] -> Doc
fillSep = folddoc (<##>)

-- The document `hcat ds` concatenates all documents `ds` horizontally with `(<+>)`.
hcat :: [Doc] -> Doc
hcat = folddoc (<+>)

-- The document `vcat ds` concatenates all documents `ds` vertically with `(<!>)`.
-- If a `group` undoes the line breaks inserted by vcat, all documents are directly concatenated.
vcat :: [Doc] -> Doc
vcat = folddoc (<!>)

-- The document `cat ds` concatenates all documents xs either horizontally with `(<+>)`, if it fits the page, or vertically.
cat :: [Doc] -> Doc
cat = group . vcat

-- The document `fillCat ds` concatenates documents `ds` horizontally with `(<+>)` as long as its fits the page,
-- then inserts a `lbreak` and continues doing that for all remaining documents in `ds`.
fillCat :: [Doc] -> Doc
fillCat = folddoc (<!!>)

-- The document `sepLBy s ds` concatenates all documents in `ds` with `s` on the left, except the first document.
sepLBy :: Doc -> [Doc] -> [Doc]
sepLBy _ [] = []
sepLBy _ [d] = [d]
sepLBy s (d : ds) = d : ((s <+>) <$> ds)

-- The document `sepRBy s ds` concatenates all documents in `ds` with `s` on the right, except the last document.
sepRBy :: Doc -> [Doc] -> [Doc]
sepRBy _ [] = []
sepRBy _ [d] = [d]
sepRBy s (d : ds) = (d <+> s) : sepRBy s ds
