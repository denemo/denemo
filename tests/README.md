Tests directory
===============

Some tests are automatically done here.

 - If a ```.denemo``` file is present in the ```examples``` directory above, or in ```integration-data```, it will be opened, saved, and the saved file will be compared to the original one.
 - If a ```.mxml``` is present in the ```integration-data``` directory, it will be opened and saved. If denemo file with the same name exists (e.g. ```foobar.mxml``` and ```foobar.denemo```), it will be compared to the saved file.
 - If a ```.scm``` file exists, it will be opened and the scheme will be executed on a blank score.
