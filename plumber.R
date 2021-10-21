library(plumber)

plumber::plumb("progControl.R")$run(port = 5762)