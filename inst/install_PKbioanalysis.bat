@echo off

Rscript -e "install.packages(c('gtools', 'reactable.extras'))"
Rscript -e "if (!requireNamespace('remotes', quietly=TRUE)) install.packages('remotes', repos='https://cran.r-project.org'); remotes::install_github('OmarAshkar/PKbioanalysis', force = TRUE); PKbioanalysis::plate_app()"

pause
