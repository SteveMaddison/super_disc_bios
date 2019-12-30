# Super Disc BIOS

This is an complete\*, annotated disassembly of the prototype BIOS cartridge
for the unreleased SFX-100 "Nintendo Playstation" console or SNES CD-ROM
add-on. All\* variables have been identified, as have data areas which have
been reverse engineered into a (more) readable format (with the exception of
graphics characters/tiles which are left in text format). Effort has been taken
to remove many "magic" numbers and replace these with constants to aid
readability.

The code is in such a state that it may be reassembled; not only to recreate
the original ROM image, but also to include alterations and fixes, apply
language translations, and to remove what looks to be unused cruft. It may not
be the original source code, but it's plenty close enough for development
purposes.


\* Such absolute terms are of course difficult to defend. There could always be
more lurking in the areas which look like padding and noise. Suffice it to say
that the disassembly is self-consistent and there are no** unknown references
in the code.

\*\* Also to be taken with a pinch of salt...


## Building

If your build environment is set up correctly, simply issue the `make` command
to assemble and link the BIOS. The code uses a fair few directives specific to
the [WLA-DX](http://www.villehelin.com/wla.html) 65816 assembler, so expect to
have to make changes if using anything else.

Use the `config.inc` file to configure the build, for example to include fixes
or apply a language translation.


## Fixes

The following fixes may be enabled by building without `ORIGINAL` defined.

- Running the "SELF CHECK" procedure more than once would cause this to hang at
  the sound check phase. This was caused by trying to program the APU after it
  has already been programmed, so the APU never entered the "ready" state. The
  fix checks for the "programmed" state and skips trying to program the APU on
  subsequent runs.

- The procedure used to lock the SRAM during/after running diagnostics was
  different to that of the main program. The former has now been changed to
  match the latter.

- The "BACKUP RAM" menu had a subtle graphical glitch resulting in the last
  tile in the set having an incorrect background colour. This was due to a few
  missing words at the end of the encoded tile data, so the appropriate title
  screen tiles were not fully overwritten. These missing words have been added.

- When saving a new file to SRAM, the number of files was not checked against
  the allowed maximum. This check was however unnecessarily performed when
  overwriting an existing file. The check is now only done when saving a new
  file.

- Having zero files as well as zero free space was originally permitted. This
  causes problems if the whole SRAM is zeroed out; it makes it impossible to
  create new files, as the program incorrectly concludes there is not enough
  free space. Now, if both the number of files and free space are zero, this is
  treated as an error and the SRAM is re-initialised to a valid state.


## Translations

Check the `i18n` directory for examples. New translations can best be added by
copying an existing file and editing as necessary, then defining `LANG` in
`config.inc`. Note that all macros must be defined and some things must have a
specific length for the translation to assemble correctly.


## Legal

Of course, this disassembly will by its very nature allow the recreation of
what remains a copyrighted program; strings in the code even attest to the
original copyright holder. One may argue that the ROM image is trivially
accessible online via a cursory search, has been abandoned by its original
author, has little to no commercial value and ultimately serves very little
purpose.

In any case, the "author" (for as far as the creator of this disassembly can be
termed as such) does not condone software piracy nor copyright infringement
and presents this code solely for the purposes of study and the historical
preservation of the knowledge found within.

No claims are made to copyright of the original code/image nor the disassembly,
annotations and other derived works which may be considered to be in the public
domain, as far as this is possible. The program is provided "as-is" without
warranty of any kind, either expressed or implied, including, but not limited
to, the implied warranties of merchantability and fitness for a particular
purpose.
