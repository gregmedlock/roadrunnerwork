import sys
import subprocess
import datetime

def updateDownloadsWiki(rrUpdates, svn_revision):
    template = "r:/roadrunnerwork/wiki/Downloads.template"
    wiki = "r:/roadrunnerwork/wiki/Downloads.wiki"
    print 'Updating Revision: ' + svn_revision
    date = datetime.datetime.now().strftime("%Y-%m-%d")
    time = datetime.datetime.now().strftime("%H:%M")
    print date + time
    substs =['DATE', 'TIME', 'SVN_REV']
    #parse the downloads wiki page template
    newText = ""
    tFile  = open(template, 'r' ).readlines()
    for line in tFile:
        for word in line.split():
            if word not in substs:
                newText = newText + ' ' + word
            elif word == 'DATE':
                    newText = newText + date.rstrip()
            elif word == 'TIME':
                    newText = newText + time.rstrip()
            elif word == 'SVN_REV':
                    newText = newText + svn_revision.rstrip()

        newText = newText + '\n'

    newText = newText.split('\n')
    #Clean lines
    saveText = ""
    for line in newText:
        line = line.lstrip()
        line = line.rstrip()
        saveText = saveText + line + '\n'

    #write to wiki
    wikiF = open(wiki, 'w')
    wikiF.writelines(saveText)

    #Finally commit
    try:
        output = subprocess.check_output([tsvn, 'commit', 'r:/roadrunnerwork/wiki', '-m\"Build Script Commit of wiki\"'], shell=True)
        print "Commit of wiki succeded.."
    except subprocess.CalledProcessError, e:
        print "Failed svn commit of wiki:\n", e.output


now = datetime.datetime.now()
print 'Build started at: ' + now.strftime("%Y-%m-%d %H:%M")

print 'RR BUILD'
tsvn="C:\\Program Files\\TortoiseSVN\\bin\\svn.exe"
sandBoxRoot='r:/roadrunnerwork'
buildFolder='r:/builds/vs/release'

buildConfig='Release'
rrSLN='RoadRunner.sln'

rrBuilds=["all", "cpp", "c_api", "python_api"]
rrUpdates=[]

#ThirdParty
print 'Installing 3Rd Party'
try:
    output = subprocess.check_output(['msbuild', '/p:Configuration='+buildConfig, buildFolder +'/ThirdParty/INSTALL.vcxproj'], shell=True)
    print 'ThirdParty Install Succeeded'
except subprocess.CalledProcessError, e:
    print "Third Party Build Failed with output:\n", e.output

#Cleaning....
for build in rrBuilds:
    try:
        output = subprocess.check_output(['msbuild', '/p:Configuration='+buildConfig, buildFolder +'/'+ build + '/RoadRunner.sln', '/t:clean'], shell=True)
        print 'Cleaning build \"' + build + '\" succeded'
    except subprocess.CalledProcessError, e:
        print "Build Failed with output:\n", e.output

#Create Packages
for build in rrBuilds:
    try:
        output = subprocess.check_output(['msbuild', '/p:Configuration='+buildConfig, buildFolder +'/'+ build + '/PACKAGE.vcxproj'], shell=True)
        print 'Creating package for \"' + build + '\" succeded'
        rrUpdates.append(build)
    except subprocess.CalledProcessError, e:
        print "Build Failed with output:\n", e.output

try:
    output = subprocess.check_output([tsvn, 'commit', 'r:/roadrunnerwork/releases/vs', '-m\"Build Script Commit\"'], shell=True)
    print "Commit succeded"
except subprocess.CalledProcessError, e:
    print "Failed svn commit:\n", e.output

#Get svn revision
try:
    output = subprocess.check_output([tsvn, 'info', 'r:/roadrunnerwork/releases/vs'], shell=True)
    lines = output.split('\n')
    for line in lines:
        if "Revision: " in line:
            svn_revision = line.split(" ")[-1]
except subprocess.CalledProcessError, e:
    print "Failed svn commit:\n", e.output

updateDownloadsWiki(rrUpdates, svn_revision)

print "done..."
print

