echo Building RoadRunner packages
cd r:\rrBuilds\vs\full
make clean
make install package

cd r:\rrBuilds\vs\cxx_api
make clean
make install package

cd r:\rrBuilds\vs\c_api
make clean
make install package

echo "Done....."
