for f in *.ad?; do cat COPYRIGHT.txt $f > $f.PLUS ; rm $f; mv $f.PLUS $f; done;
