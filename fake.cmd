.paket\paket.exe restore -v
if errorlevel 1 (
  exit /b %errorlevel%
)
dotnet restore build.proj
dotnet fake %*