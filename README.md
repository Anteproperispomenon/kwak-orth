# kwak-orth

## Introduction

`kwak-orth` is a library designed to convert between the various
orthographies of the Kwak'wala language. The library also comes
with a simple command-line/terminal program that allows simple
conversion of plain-text files encoded in UTF-8.

## Usage

### Program Usage

To use the simple conversion, first you'll have to compile it
(see below). Then, you have two options:

   1. Look through ".stack-work" until you find
      "kwak-orth-exe(.exe)", and then copy that
      file to an easy-to use location. If
      necessary, copy the ".bat" files from
      "batch_files" to that location as well
      to allow drag-and-drop operation. This
      option should work even if you copy the program
      to other computers of the same architecture.
      If on Mac OS, look at `command_files/README.md`
      for info on how to use drag-and-drop operation.

   2. Run the program through stack. This has the
      advantage of re-compiling the program each
      time the code changes, ensuring that the 
      program is completely up-to-date. To do this,
      run `stack run -- ` followed by the commands
      you want to run in the program. For example,
      to see the input options, run
      `stack run -- --help`. If that doesn't work,
      try running `stack run kwak-orth-exe -- ...`
      instead.

For information on how to run the program in command-line/terminal/bash
mode, run `stack run -- --help` in the library or else run
`kwak-orth-exe --help` if on Windows, or `./kwak-orth-exe --help`
if on Unix-like systems.

Note: On Windows, you may have to run `chcp 65001` before running
the program. The batch files take care of this themselves.

### Compiling

To compile this library, you will need the Haskell Stack
tool. Information on how to install GHC, Cabal, and Stack
can be found on the [Haskell Website](https://www.haskell.org/downloads/).
After ensuring you have GHC, Cabal, and Stack installed, navigate
to the `kwak-orth` directory in your terminal/command prompt/etc...
and run `stack build`. This will install all necessary packages
to compile the library and program, and then compile them.
Afterwards, you can use `stack run` or `stack run kwak-orth-exe`
to ensure that the porgram works properly.

### Library Usage

The library works by first parsing text data, and then converting
it into an internal representation consisting of a string of
"sounds" and plaintext strings. This internal representation is
then run through an "outputter" that outputs the "sounds" according
to the orthography and just outputs the plaintext as input.

The parsers can be found in the "Kwakwala.Parsers" directory,
while the output functions can be found in the "Kwakwala.Output"
directory.

For more information on the individual functions and modules,
run `stack haddock`, which will produce a set of HTML files
with information on the code.

## Acknowledgements

### Orthography Information

Information on the various orthographies used comes from
a variety of sources.

The main source, used for both U'mista and NAPA, is from
Chris Harvey's [Language Geek](https://www.languagegeek.com/index.html)
website, which contains information on [Kwak'wala Orthographies](https://www.languagegeek.com/wakashan/kwakwala.html).

Wikipedia was the main source used for the [IPA](https://en.wikipedia.org/wiki/Kwak%CA%BCwala),
[traditional NAPA](https://en.wikipedia.org/wiki/Americanist_phonetic_notation), and
[Georgian](https://en.wikipedia.org/wiki/Georgian_scripts) orthographies.

The Boas orthography is based on a sample of his works.

### Testing

The test suite contains example sentences taken from the FirstVoices
phrase database. It can be found at
[FirstVoices Kwak'wala](https://www.firstvoices.com/explore/FV/sections/Data/Kwak'wala/Kwak%CC%93wala/Kwak%CC%93wala).