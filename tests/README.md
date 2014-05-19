Tests directory
===============

Some tests are automatically done here.

 - If a ```.denemo``` file is present in the ```examples``` directory above, or in ```fixtures```, it will be opened, saved, and the saved file will be compared to the original one.
 - If a ```.mxml``` is present in the ```fixtures``` directory, it will be opened and saved. If denemo file with the same name exists (e.g. ```foobar.mxml``` and ```foobar.denemo```), it will be compared to the saved file.
 - If a ```.scm``` file exists in the ```fixtures``` directory, it will be opened and the scheme will be executed on a blank score and saved. If denemo file with the same name exists (e.g. ```foobar.scm``` and ```foobar.denemo```), it will be compared to the saved file.

