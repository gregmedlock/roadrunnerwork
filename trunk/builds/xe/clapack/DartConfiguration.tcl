# This file is configured by CMake automatically as DartConfiguration.tcl
# If you choose not to use CMake, this file may be hand configured, by
# filling in the required variables.


# Configuration directories and files
SourceDirectory: C:/rrw/ThirdParty/clapack/3.2.1
BuildDirectory: C:/rrw/builds/xe/clapack

# Where to place the cost data store
CostDataFile: 

# Site is something like machine.domain, i.e. pragmatic.crd
Site: CREATOR

# Build name is osname-revision-compiler, i.e. Linux-2.4.2-2smp-c++
BuildName: Win32-

# Submission information
IsCDash: TRUE
CDashVersion: 
QueryCDashVersion: 
DropSite: my.cdash.org
DropLocation: /submit.php?project=CLAPACK
DropSiteUser: 
DropSitePassword: 
DropSiteMode: 
DropMethod: http
TriggerSite: 
ScpCommand: c:/cygwin/bin/scp.exe

# Dashboard start time
NightlyStartTime: 00:00:00 EDT

# Commands for the build/test/submit cycle
ConfigureCommand: "C:/Program Files (x86)/CMake 2.8/bin/cmake.exe" "C:/rrw/ThirdParty/clapack/3.2.1"
MakeCommand: make -i
DefaultCTestConfigurationType: Release

# CVS options
# Default is "-d -P -A"
CVSCommand: CVSCOMMAND-NOTFOUND
CVSUpdateOptions: -d -A -P

# Subversion options
SVNCommand: c:/cygwin/bin/svn.exe
SVNUpdateOptions: 

# Git options
GITCommand: GITCOMMAND-NOTFOUND
GITUpdateOptions: 
GITUpdateCustom: 

# Generic update command
UpdateCommand: 
UpdateOptions: 
UpdateType: 

# Compiler info
Compiler: 

# Dynamic analysis and coverage
PurifyCommand: 
ValgrindCommand: 
ValgrindCommandOptions: 
MemoryCheckCommand: MEMORYCHECK_COMMAND-NOTFOUND
MemoryCheckCommandOptions: 
MemoryCheckSuppressionFile: 
CoverageCommand: c:/cygwin/bin/gcov.exe

# Cluster commands
SlurmBatchCommand: SLURM_SBATCH_COMMAND-NOTFOUND
SlurmRunCommand: SLURM_SRUN_COMMAND-NOTFOUND

# Testing options
# TimeOut is the amount of time in seconds to wait for processes
# to complete during testing.  After TimeOut seconds, the
# process will be summarily terminated.
# Currently set to 25 minutes
TimeOut: 1500

UseLaunchers: 
CurlOptions: 
# warning, if you add new options here that have to do with submit,
# you have to update cmCTestSubmitCommand.cxx

# For CTest submissions that timeout, these options
# specify behavior for retrying the submission
CTestSubmitRetryDelay: 5
CTestSubmitRetryCount: 3
