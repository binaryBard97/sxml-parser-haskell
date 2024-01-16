**SXML Parser and Pretty Printer**

**Overview:**
A simple XML-like language parser (`SXML`) and a corresponding pretty printer. The `SXML` data type represents the structured XML-like content, and the code includes parsers for elements, attributes, and text content. The pretty printer formats the parsed `SXML` back into a readable and formatted XML-like representation.

**Module Structure:**
- `SXML` module exports `SXML`, `sxmlP`, `sxmlD`, and `main`.
- `SXML` type represents the parsed SXML content.
- `sxmlP` is the parser for SXML content.
- `sxmlD` is the pretty printer for SXML content.
- `main` is the entry point for the program, reading input from the command line, parsing it, and pretty printing the result.

**Functionality:**
- Parsing: The code provides parsers for elements (`eltP`), attributes (`attP`), and text content (`textP`).
- Pretty Printing: The code defines functions (`sxmlD`, `eltD`, `tagD`, `attD`, `itemsD`, `itemD`) to format parsed SXML content into a readable representation.
- The code includes utility functions (`valid`, `validName`, `validAttValue`, `validText`) to validate different components of SXML.
- The `getArgs` function is used to read command line arguments, and the `getContents` function reads input from standard input.


- The project uses various Haskell modules, including `Control.Monad`, `System.Environment`, `Text.Read`, and custom parser modules (`Parser` and `PrettyPrinter`).