
if [ -z "$1" ]; then
  echo "Usage $0 <source> <target>"
  exit 1;
fi

if [ -z "$2" ]; then
  echo "Usage $0 <source> <target>"
  exit 1;
fi

inputfile=$1;
outputfile=$2;

if [ ! -f $inputfile ]; then
     echo "Invalid file : $inputfile"
     exit 2;
fi 

if [ ! -f $outputfile ]; then
     echo "Invalid file : $outputfile"
     exit 2;
fi

startlnum=`grep -nE "\<START\>" $inputfile | awk -F\: '{print $1;}'`
endlnum=`grep -nE "\<END\>" $inputfile | awk -F\: '{print $1;}'`
filesz=`wc -l $inputfile | awk '{print $1;}'`
endlnum=`expr $filesz - $endlnum`
endlnum=`expr $endlnum + 1`

touch /tmp/out.$$
head -$startlnum $inputfile >> /tmp/out.$$
##
cat ../bspricers.r >> /tmp/out.$$
cat ../barrierpricers.r >> /tmp/out.$$
cat ../hedgesteps.r >> /tmp/out.$$
cat ../display.r >> /tmp/out.$$

##
tail -$endlnum $inputfile >> /tmp/out.$$

cp /tmp/out.$$ $outputfile
dos2unix $outputfile
rm -f /tmp/out.$$


