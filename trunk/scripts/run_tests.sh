#! /bin/bash 

echo "Hello";
logFile="testLog.txt"
logTable="testLogTable.txt"
failed="failedTests.txt"
simulator="/cygdrive/c/rrw/installs/xe/bin/simulate.exe"
start=$1
end=$2

echo "" > $logFile
for ((i=$start; i<=$end; i++ )); 
do 
	echo "Running $i" ; 
	echo $i >> $logFile; 
	$simulator -c -n$i -v3 >> $logFile;
	echo "Next" >> $logFile;
done

#Creeate a table

make_table -f$logFile -o$logTable -w"Next"
#filter failed ones

cat $logTable | grep "failed" > $failed
echo "Done" 
