#! /bin/bash 

dataFolder="/cygdrive/c/DataOutput/"
tmpFolder="/cygdrive/c/temp"
echo "Hello";
logFile="testLog.txt"
logTable="testLogTable.txt"
failed="failedTests.txt"
simulator="/cygdrive/c/rrw/installs/xe/bin/simulate.exe"
start=$1
end=$2

echo "" > $logFile
echo "" > $logTable
echo "" > $failed

#Remove previous data files
find $dataFolder -name '*l2v4.csv' -exec rm -fv {} \;


for ((i=$start; i<=$end; i++ )); 
do 
	echo "Running $i" ; 
	echo $i >> $logFile; 
	$simulator -n$i -v0 >> $logFile &
	echo "Next" >> $logFile;
	sleep .08
done

echo "Waiting for background jobs to finish..."
wait

#Creeate a table

make_table -f$logFile -o$logTable -w"Next"
filter failed ones

cat $logTable | grep "failed" > $failed

#Copy files and zip them up
dataFiles="dataFiles.txt"
find '/cygdrive/c/DataOutput'  -name '*l2v4.csv' > $dataFiles
cygpath -d `cat dataFiles.txt` > $dataFiles
echo "zipFile="data_`date | cut -d' ' -f4 | tr ':' '-'`.zip"
"
zipFile="data_`date | cut -d' ' -f4 | tr ':' '-'`.zip"
rm -v $zipFile
echo "executing 7za  $zipFile `cat $dataFiles`"
7za a $zipFile `cat $dataFiles`
echo "Done" 
