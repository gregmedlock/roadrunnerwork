#! /usr/bin/bash

dataFolder="/cygdrive/r/DataOutput/"
tempFolder="/cygdrive/r/rrTemp"
logFile=$tempFoler"/testLog.txt"
logTable=$tempFolder"/testLogTable.txt"
failed=$tempFolder"/failedTests.txt"
binFolder="/cygdrive/r/rrInstalls/xe/bin/"
simulator=$binFolder"/rr_ts_tester.exe"
zipper="/cygdrive/r/roadrunnerwork/Misc/zipper/7za.exe"
ModelsDir="R://SBML_l2v4"
start=$1
end=$2

#empty files
echo "" > $logFile
echo "" > $logTable
echo "" > $failed

excludeTest="1 2 3"

cd $binFolder

echo "Current working folder is"`pwd`

#Remove previous data files
find $dataFolder -name '*l2v4.csv' -exec rm -fv {} \;

for ((i=$start; i<=$end; i++ ));
do
	echo "Running $i" ;
	echo $i >> $logFile;
#    echo "$simulator -n$i -v0 >> $logFile "
	$simulator -m$ModelsDir -n$i -vError >> $logFile
	echo "Next" >> $logFile;
    #sleep .15
done

echo "Waiting for background jobs to finish..."
#wait

##Creeate a table
#
#make_table -f$logFile -o$logTable -w"Next"
##filter failed ones
#
#cat $logTable | grep "failed" > $failed
#

#Copy files and zip them up
dataFiles="dataFiles.txt"
find $dataFolder  -name '*l2v4.csv' > $dataFiles
cygpath -d `cat dataFiles.txt` > $dataFiles
zipFile="data_`date +"%m%d%Y"`.zip"
rm -v $zipFile
$zipper a $zipFile `cat $dataFiles`
echo "Done"

