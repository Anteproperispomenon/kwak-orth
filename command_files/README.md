# Command Files

This directory contains files that make
it easier to use `kwak-orth` on Mac OS.
Unfortunately, using them is not as simple
as it is on Windows, as you can't simply
drag-and-drop files.

## Setting Up the Files

First of all, copy the contents of this
directory to a folder you have access to,
and then also copy in `kwak-orth-exe`, which
can be found in `./stack-work/install/.../bin`.

## Making Files Runnable

If, when trying to run the .command files,
you get an error window about not being able to
run them, you'll have to manually make them
runnable. 

To do so, start Terminal and navigate to the
directory where the files are located (use the
command `cd ./directory-name` to navigate to
the next directory until you reach the right
directory. If you're in the wrong directory,
use `cd ..` to go to the parent directory of
the current directory). Once you're there,
you need to run the command

```chmod u+x ./*.command```

which will make it so you can run the .command
files directly from Finder.

## Actually using the Files

Once all the .command files can be run correctly,
you can then use them to convert text files. To do
so, click on the relevant converter file (e.g
`Grubb2Napa.command` or `FixUmista.command`). This
will open a terminal window where you choose which
file to convert. To do so, you can either type in 
the name of the file you want to convert (if it's 
in the same directory), or just drag and drop the
file from another finder window onto the terminal
window and press return/enter. This should create
a new file with a name like

``` original_file.orth.ext ```

where `original_file` is the original file name,
`orth` is a shortened version of the output 
orthography, and `ext` is the original file
extension, usually `txt`.