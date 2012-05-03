#! /bin/bash 

dataFolder="/cygdrive/c/DataOutput/"
tmpFolder="/cygdrive/c/temp"
logFile="testLog.txt"
logTable="testLogTable.txt"
failed="failedTests.txt"
simulator="/cygdrive/c/rrw/installs/xe/bin/simulate.exe"
start=$1
end=$2

#empty files
echo "" > $logFile
echo "" > $logTable
echo "" > $failed

excludeTest="1 2 3"

#Remove previous data files
#find $dataFolder -name '*l2v4.csv' -exec rm -fv {} \;

for ((i=$start; i<=$end; i++ )); 
do 
	echo "Running $i" ; 
	echo $i >> $logFile; 
#	$simulator -n$i -v3 >> $logFile &
	$simulator -n$i -v3 >> $logFile 
	echo "Next" >> $logFile;
#	sleep .005
done

#echo "Waiting for background jobs to finish..."
#wait
#
##Creeate a table
#
#make_table -f$logFile -o$logTable -w"Next"
##filter failed ones
#
#cat $logTable | grep "failed" > $failed
#
#Copy files and zip them up
dataFiles="dataFiles.txt"
find '/cygdrive/c/DataOutput'  -name '*l2v4.csv' > $dataFiles
cygpath -d `cat dataFiles.txt` > $dataFiles
zipFile="data_`date +"%m%d%Y"`.zip"
rm -v $zipFile
7za a $zipFile `cat $dataFiles`
echo "Done" 
