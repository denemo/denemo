#!/bin/sh

###### Denemo: run the tests that are found in the directory $1 and reference files in $2
###### This script should be run from the toplevel denemo directory after the make
 
if [[ $# -ne 2 ]]; then
  echo Usage: $0 testdirectory referencedirectory 1>&2
  echo example:
  echo $0 tests reference
  exit 1
fi



tests=$1
reference=$2
toplevel=$(pwd)

if [[ ! -f denemo.conf ]] ; then
    echo $pwd: this does not seem to be the top level denemo directory
    exit 1
fi
if [[ ! -d $toplevel/$tests ]] ; then
    echo no directory $toplevel/$tests found
    exit 1
fi

failedtests=$toplevel/failed$tests
if [[  -d $failedtests ]] ; then
    rm -rf $failedtests
fi
mkdir $failedtests
#### set $HOME so that the test can expect a known set of keybindings
export HOME=$toplevel/$tests
echo home directory set to $HOME
if [[ -d $HOME/.denemo ]] ; then
    echo found $HOME/.denemo
else
    echo creating $HOME/.denemo
    mkdir $HOME/.denemo
fi

unset EDITOR
#FIXME -other things like locale need standardizing for test run?

if [[ -d testtemp ]] ; then
    cd testtemp
    file=$(ls)
    for f in $file; do
      echo removing old test file $f
      rm $f
    done
    cd ..
else
    echo make testtemp
    mkdir testtemp
fi

cd $tests
testfiles=$(ls)
echo "**************************"
echo TESTS TO BE DONE: $testfiles
echo "**************************"
for f in $testfiles; do
        echo ********PERFORMING TEST $f *****************
        cd ../testtemp
#make two file entries to mimic presence of fifos that were there during recording
#ignore the permission bits 
	touch gttfifo1
	touch gttfifo2
#restore the environment after previous test run
	cp $toplevel/denemo.keymaprc $HOME/.denemo/keymaprc
	cp $toplevel/denemo.conf $HOME/.denemo/denemorc

	gnome-test-tool-play  -e ../$tests/$f  -r ../$reference/denemo-play.res ../src/denemo
	/bin/rm gttfifo1
	/bin/rm gttfifo2
	results=$(ls)
	for result in $results; do
	        echo Comparing $result with $reference
		diff $result ../$reference/$f
		if [[ $? -eq 0 ]] ; then
		    echo test passed   
		else
		    cp ../$tests/$f $failedtests
		    echo test $f failed to repeat 
		    echo To retry run tests on directory failed$tests
		fi
		rm $result
	done
	echo ********END OF TEST $f *****************
done

	echo ********END OF ALL TESTS *****************
echo Failures are in $failedtests - here is the listing:
ls $failedtests

exit 0
