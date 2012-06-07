#! /bin/bash

#Check current host file
currentDLLs="currentDLLs"

echo "Current DLLs file:"$currentDLLs;
current=`cat $currentDLLs`
echo "=== content ==== : "$current

if [ $current == "XE" ] ; then
	echo "====== Switching to VS DLL's ========";
	echo "VS" > currentDLLs
	rm -v bins/*.dll;
	cp -v vs/bins/* bins;
else
	echo "====== Switching to XE DLLS ========";
	echo "XE" >  currentDlls
	rm -v bins/*.dll;
	cp -v xe/bins/* bins;
fi

exit 0;

