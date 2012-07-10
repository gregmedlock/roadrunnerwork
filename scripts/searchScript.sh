#! /usr/bin/bash

echo "start"
find . -name *l2v4.xml -exec grep -inH "boundaryCondition=false" {} \;

echo "done"
