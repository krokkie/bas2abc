# install.packages("hyphenatr")
devtools::install_github("hrbrmstr/hyphenatr")
library(hyphenatr)

# hyphenatr::list_dicts()
hyphenatr::switch_dict("af_ZA")

dat <- readLines(system.file("extdata/top10000en.txt", package="hyphenatr"))
microbenchmark(out1 <- hyphenate(dat))
