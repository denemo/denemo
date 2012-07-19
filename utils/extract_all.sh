echo extracting scheme scripts
cd utils
export PATH=$PATH:`pwd`
echo $PATH
cd ../actions/commandscripts

export DENEMO_COMMANDSCRIPTS_DIR=`pwd`
echo extracting to $DENEMO_COMMANDSCRIPTS_DIR

cd ../menus && find $1 -name "*" -exec extract_scheme '{}' \;

cd ../../ 
ls -A1 actions/commandscripts/*.scm > po/SCHEME_POTFILES.in
ls -A1 src/*.[ch] > po/POTFILES.in.in
cd po
cat POTFILES.in.in SCHEME_POTFILES.in > POTFILES.in
