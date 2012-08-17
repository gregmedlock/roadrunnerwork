#! /usr/bin/bash

start=$1
end=$2
compiler=$3

tempFolder="/cygdrive/r/RRTesting"
dataOutputRoot=$tempFolder"/DataOutput/"$compiler
logFile=$tempFolder/"testLog_$compiler.txt"
logTable=$tempFolder/"testLogTable_$compiler.txt"
failed=$tempFolder/"failedTests_$compiler.txt"
binFolder="/cygdrive/r/rrInstalls/"$compiler"/bin/"
simulator=$binFolder"/rr_ts_tester.exe"
zipper="/cygdrive/r/roadrunnerwork/Misc/zipper/7za.exe"
ModelsDir="R://SBML_l2v4"

#empty files
echo "" > $logFile
echo "" > $logTable
echo "" > $failed

cd $binFolder

echo "Current working folder is"`pwd`

#Remove previous data files
find $dataOutputRoot -name '*l2v4.csv' -exec rm -fv {} \;

for ((i=$start; i<=$end; i++ ));
do
	echo "Running $i" ;
	echo "Case "$i >> $logFile;
    winPath=`cygpath -w $dataOutputRoot`
    $simulator -m$ModelsDir -n$i -d$winPath -vError > $logFile &
    #$simulator -m$ModelsDir -n$i -d$winPath -vError > $logFile
    sleep .0015
done

echo "Waiting for background jobs to finish..."
wait

##Create a table
#
#make_table -f$logFile -o$logTable -w"Next"
##filter failed ones
#
#cat $logTable | grep "failed" > $failed
#

cd $tempFolder
echo "Current folder is"`pwd`

#Copy files and zip them up
dataFiles="dataFiles_$compiler.txt"

echo "Find data files in $dataOutputRoot . Save in $dataFiles"
find  $dataOutputRoot  -name '*l2v4.csv' > $dataFiles

cygpath -w `cat $dataFiles` > $dataFiles"_dos"


zipFile="data_`date +"%m%d%Y"`_$compiler.zip"
rm -v $zipFile

echo "Zip it up..."
$zipper a $zipFile `cat $dataFiles_dos`
echo "Done"

