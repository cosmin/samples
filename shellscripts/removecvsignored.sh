for x in `find . -name ".cvsignore"`; do for i in `cat $x`; do echo $(dirname $x)/$i; done; done | xargs rm -rf
