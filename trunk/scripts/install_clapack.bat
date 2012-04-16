echo Copying clapack libs

REM copy %rr_root%\ThirdParty\libstruct\dependencies\blas.lib %rr_root%\installs\vs-2010\lib
copy %rr_root%\ThirdParty\libstruct\dependencies\lib\clapack.lib %rr_root%\installs\vs-2010\lib
REM copy %rr_root%\ThirdParty\libstruct\dependencies\libf2c.lib  %rr_root%\installs\vs-2010\lib

REM copy %rr_root%\builds\vs-2010\clapack\BLAS\SRC\Debug\blas.lib %rr_root%\installs\vs-2010\lib
REM copy %rr_root%\builds\vs-2010\clapack\SRC\Debug\lapack.lib %rr_root%\installs\vs-2010\lib
REM copy %rr_root%\builds\vs-2010\clapack\F2CLIBS\libf2c\Debug\libf2c.lib  %rr_root%\installs\vs-2010\lib


