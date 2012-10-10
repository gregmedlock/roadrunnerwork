import sys
import subprocess

print 'RR BUILD'

sandBoxRoot='r:/roadrunnerwork'
buildFolder='r:/builds/vs/release'

buildConfig='Release'
rrSLN='RoadRunner.sln'

rrBuilds=["all", "cpp", "c_api", "python_api"]
rrUpdates=[]

#ThirdParty
print 'Installing 3Rd Party'
try:
#    output = subprocess.check_output(['msbuild', '/p:Configuration='+buildConfig, buildFolder +'/ThirdParty/INSTALL.vcxproj'], shell=True)
    print 'ThirdParty Install Succeeded'
except subprocess.CalledProcessError, e:
    print "Third Party Build Failed with output:\n", e.output

#Cleaning....
for build in rrBuilds:
    try:
#        output = subprocess.check_output(['msbuild', '/p:Configuration='+buildConfig, buildFolder +'/'+ build + '/RoadRunner.sln', '/t:clean'], shell=True)
        print 'Cleaning build \"' + build + '\" succeded'
    except subprocess.CalledProcessError, e:
        print "Build Failed with output:\n", e.output

#Create Packages
for build in rrBuilds:
    try:
#        output = subprocess.check_output(['msbuild', '/p:Configuration='+buildConfig, buildFolder +'/'+ build + '/PACKAGE.vcxproj'], shell=True)
        print 'Creating package for \"' + build + '\" succeded'        
        rrUpdates.append(build)
    except subprocess.CalledProcessError, e:
        print "Build Failed with output:\n", e.output

try:
    output = subprocess.check_output(['svn', 'commit ' + sandBoxRoot + '/releases', '-m"Build Script Commit'], shell=True)
except subprocess.CalledProcessError, e:
    print "Failed svn commit:\n", e.output

print "done..."
print

