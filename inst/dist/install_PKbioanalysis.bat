@echo off

Rscript -e "install.packages(c('gtools'))"
Rscript -e "if (!requireNamespace('remotes', quietly=TRUE)) install.packages('remotes', repos='https://cran.r-project.org'); remotes::install_github('OmarAshkar/PKbioanalysis', force = TRUE, ref='dev'); PKbioanalysis::study_app()"

curl -o study_app.bat https://github.com/OmarAshkar/PKbioanalysis/blob/main/inst/dist/study_app.bat
curl -o chrom_app.bat https://github.com/OmarAshkar/PKbioanalysis/blob/main/inst/dist/chrom_app.bat
curl -o quant_app.bat https://github.com/OmarAshkar/PKbioanalysis/blob/main/inst/dist/quant_app.bat

pause
