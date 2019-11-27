### if you've changed a package, paste in the relevant bit of this to recompile it

setwd('c:/research/aafunct')
devtools::install()

devtools::install('f1admin')
devtools::document('f1data')
devtools::install('f1data')
devtools::install('model code/f1qualifying')
#usethis::use_package('data.table')
devtools::install('model code/f1smoothing')
devtools::install('model code/f1stretching')
devtools::install('model code/f1outlier')

devtools::install('model code/f1validity')
devtools::install('model code/f1laptimelm')
devtools::install('model code/f1gaptrafficpitstop')
devtools::install('model code/f1carproblem')
devtools::document('f1plot')
devtools::install('f1plot')

devtools::install('model code/f1yearlycoef')
devtools::install('model code/f1messystint')
devtools::install('model code/f1blockedovertakingmodel')
devtools::install('model code/f1simulation')
