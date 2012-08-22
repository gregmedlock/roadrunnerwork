echo Building RoadRunner packages
cd r:\rrBuilds\vs\ThirdParty
msbuild /p:Configuration=Release INSTALL.vcxproj

cd r:\rrBuilds\vs\full
msbuild /p:Configuration=Release RoadRunner.sln /t:clean
msbuild /p:Configuration=Release PACKAGE.vcxproj

cd r:\rrBuilds\vs\cxx_api
msbuild /p:Configuration=Release RoadRunner.sln /t:clean
msbuild /p:Configuration=Release PACKAGE.vcxproj

cd r:\rrBuilds\vs\c_api
msbuild /p:Configuration=Release RoadRunner.sln /t:clean
msbuild /p:Configuration=Release PACKAGE.vcxproj

echo "Done....."
PAUSE
