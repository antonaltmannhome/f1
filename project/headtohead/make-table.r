
myFilePath = 'c:/git/f1/project/headtohead/'
myFileName = 'make-table.r'
myOutputFile= 'table.html'

DoRender = function() {
  rmarkdown::render(input = paste0(myFilePath, myFileName),
                    output_file = myOutputFile,
                    output_dir = myFilePath)
}

myDF = ViewComparisonByYear(2019)
badName = c('fuelTyreAdj', 'meanTyreLap1', 'meanTyreLap2')
goodName = c('adjusting for tyres', 'avg.tyre.lap1', 'avg.tyre.lap2')
for (j in 1:length(badName)) {
  names(myDF)[names(myDF) == badName[j]] = goodName[j]
}
knitr::kable(myDF, 'html') %>%
  kableExtra::kable_styling(bootstrap_options = c("striped", "hover"),
                            full_width = F)
