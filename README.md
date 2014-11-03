VBMD5
=====

This is my attempt at a Visual Basic .NET port of [my Monkey based MD5 implementation](https://github.com/Regal-Internet-Brothers/hash). This is pretty functional, but currently isn't so great with large files. With **small files** it beats the standard implementation, though. Most of the source code was written for somewhat constrained memory environments (Though, [my Monkey implementation](https://github.com/Regal-Internet-Brothers/hash) might do better at that). Overall, I don't think this was half bad for my first major program in VB.

This source code, along with its project files is released here as both a public repository for my **occasional** development of this implementation, as well as an example for others to implement the routines themselves. Because of the example aspect of this project's development, I'm attempting to make the code as configurable and testable as possible.

All project files are standard Visual Studio 2013 (For Desktop) project files, and should work out of the box. I have no reason to update the project, so unless I find myself wanting to at a later date, I make no promises. Visual Studio gets far too many updates and releases for me to keep up with them all.

If you're wondering why the indenting is weird, that's probably Visual Studio up to its old tricks again; replacing tab characters with spaces and the like. Either that, or you're not used to the way I write Visual Basic.

This project has been released under the "MIT License", so you can pretty much do with it as you please.

I'll optimize this further when I get the chance, but don't expect much more than this.

*REFERENCES:*
This project took a number of sources as examples, I couldn't possible list every single one individually, but here's a small list:

* ["MD5 Shootout" by BitBucket user Rory "rplaire" Plaire.](https://bitbucket.org/rplaire/md5-shootout/)
* [RosettaCode's MD5 implementation page. (And the people behind it)](http://rosettacode.org/wiki/MD5/Implementation)
* [Wikipedia's MD5 pseudocode example.](http://en.wikipedia.org/wiki/MD5#Pseudocode)
* [My 'hash' module](https://github.com/Regal-Internet-Brothers/hash) for the [Monkey programming language](https://github.com/blitz-research/monkey), and [its references / special thanks](https://github.com/Regal-Internet-Brothers/hash/blob/master/README.md).

*OTHER:*
At the time I'm writing this, there are two "Hello world.txt" files, don't worry about those, their the same file. They just contain the string "Hello world.".
