#!/bin/sh

###### Denemo: make a test called $2 in directory $1
###### This script should be run from the toplevel denemo directory

if [[ $# -ne 3 ]]; then
  echo Usage: $0 testdirectory referencedirectory testname 1>&2
  exit 1
fi

tests=$1
reference=$2
testname=$3

toplevel=$(pwd)

if [[ ! -f denemo.conf ]] ; then
    echo $pwd: this does not seem to be the top level denemo directory
    exit 1
fi

if [[ ! -d $tests ]] ; then
mkdir $tests
fi
if [[ ! -d $reference ]] ; then
mkdir $reference
fi
if [[ ! -d $reference/$testname ]] ; then
mkdir $reference/$testname
fi

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
cp $toplevel/denemo.keymaprc $HOME/.denemo/keymaprc
cp $toplevel/denemo.conf $HOME/.denemo/denemorc

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

cd testtemp
gnome-test-tool-record  --nowait -e $testname ../src/denemo
echo moving test $testname to $tests directory
mv $testname ../$tests

results=$(ls)
for result in $results; do
    echo moving $result to reference directory
    mv $result ../$reference/$testname   
done

echo ********END OF CREATING TEST *****************
exit 0

