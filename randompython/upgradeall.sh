#!/bin/sh # this will loop over all the installed packages
for i in `python -c "for dist in __import__('pkg_resources').working_set: print dist.project_name"`:
do
echo "`sudo easy_install -U $i`" # run easy_install and echo the results
echo "----------------------------------------" #
done
