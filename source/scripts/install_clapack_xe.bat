
echo copying XE CLAPACK libs
REM Depending on winx64 or win32
copy %rr_root%\builds\xe\clapack\SRC\lapack.lib %rr_root%\installs\xe\lib
copy %rr_root%\builds\xe\clapack\BLAS\SRC\blas.lib %rr_root%\installs\xe\lib
copy %rr_root%\builds\xe\clapack\F2CLIBS\libf2c\libf2c.lib %rr_root%\installs\xe\lib

copy %rr_root%\builds\xe\clapack_32\SRC\lapack.lib %rr_root%\installs\xe\lib
copy %rr_root%\builds\xe\clapack_32\BLAS\SRC\blas.lib %rr_root%\installs\xe\lib
copy %rr_root%\builds\xe\clapack_32\F2CLIBS\libf2c\libf2c.lib %rr_root%\installs\xe\lib


PAUSE
