Tests directory
===============

Some tests are automatically done here.

 - If a ```.denemo``` file is present in the ```examples``` directory above, or in ```fixtures/denemo```, it will be opened, saved, and the saved file will be compared to the file with the same name in ```references/denemo``` if it exists, or the original one if not (e.g ```examples/foobar.denemo``` will be opened, saved, and the saved file should be equal to ```references/denemo/foobar.denemo```).
 - If a ```.mxml``` is present in the ```fixtures/mxml``` directory, it will be opened and saved. If a ```.denemo``` file with the same name exists in ```references/mxml``` (e.g. ```fixtures/mxml/foobar.mxml``` and ```references/mxlm/foobar.denemo```), it will be compared to the saved file.
 - If a ```.scm``` file exists in the ```fixtures/scm``` directory, it will be opened and the scheme code will be executed on a blank score and saved. If a ```.denemo``` file with the same name exists in ```references/scm``` (e.g. ```fixtures/scm/foobar.scm``` and ```references/scm/foobar.denemo```), it will be compared to the saved file.
