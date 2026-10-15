@echo off
cd /d "%~dp0"
where Rscript >nul 2>nul
if errorlevel 1 (
  echo Rscript not found. Install R from https://cran.r-project.org and re-run.
  pause
  exit /b 1
)
Rscript R\install_packages.R
Rscript R\render_reports.R
echo Done. Open reports\01_ab_test_analysis.html and reports\02_power_and_decision.html
pause
