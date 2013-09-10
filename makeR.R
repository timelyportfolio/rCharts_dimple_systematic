require(slidify)
slidify("cluster_weights.Rmd")

path <- "assets/fig"
files <- list.files(path)

for ( f in 1:length(files)){
  contents <- readLines(paste0(path,"/",files[f]), n = -1)
  contents <- gsub(x=contents,
                   pattern="C:\\\\Users\\\\Kent.TLEAVELL_NT\\\\Dropbox\\\\development\\\\r\\\\rCharts_nvd3_systematic\\\\",
                   replacement="../../")
  contents <- gsub(x=contents,
                    pattern = "libraries\\\\widgets\\\\nvd3",
                    replacement = "libraries/widgets/nvd3"
  )
  writeLines(contents, paste0(path,"/",files[f]))
}