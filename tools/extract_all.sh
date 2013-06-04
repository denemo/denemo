echo extracting scheme scripts
cd utils
export PATH=$PATH:`pwd`
echo $PATH
cd ../actions/commandscripts

export DENEMO_COMMANDSCRIPTS_DIR=`pwd`
echo extracting to $DENEMO_COMMANDSCRIPTS_DIR

cd ../menus && find $1 -name "*" -exec extract_scheme '{}' \;

