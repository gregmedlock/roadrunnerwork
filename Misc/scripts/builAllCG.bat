echo Building RoadRunner packages
cd r:\rrBuilds\xe\c_api
make clean
make install package

cd r:\rrBuilds\xe\cxx_api
make clean
make install package

cd r:\rrBuilds\xe\rr_full
make clean
make install package

echo "Done....."
