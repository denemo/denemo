echo extracting scheme scripts to $1
cd utils
export PATH=$PATH:`pwd`
echo $PATH

cd ../actions/menus && find $1 -name "*" -exec extract_scheme '{}' $1 \;

#cd ../../src && ./denemo
#ls -A1 ../actions/commandscripts/*.c > SCHEME_POTFILES.in
#ls -A1 ../src/*.[ch] > POTFILES.in.in
#cd ../po && cat POTFILES.in.in SCHEME_POTFILES.in >POTFILES.in
