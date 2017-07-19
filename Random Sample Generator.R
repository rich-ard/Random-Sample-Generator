# Make sure necessary R packages are installed; if not, do so, and load 'em,
# and wipe variables
packages <- c("tools", "tcltk", "stringr")
if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(packages, rownames(installed.packages())))
}

library(tools)
library(tcltk)
library(stringr)

rm(list=ls(all=TRUE))

filename <- ''
header_bool <- ''
howmany <- ''
sorting_bool <- ''
save_bool <- ''

# Select the single-column .csv populated with the input values to be randomized
print("Select single-column .csv with input values")
while(file_ext(filename) != "csv"){
  filename <- file.choose()
}

# Read in .csv; if it has more than one column, end script.
df <- read.csv(filename, header = FALSE)
if(ncol(df) > 1){
  print("The selected .csv has more than one column.")
  break
}

# Is there a header row? .csv
while(grepl('^[YyNn].*', header_bool)==FALSE){
  header_bool <- readline(prompt="Is there a header row? ")
}

# Select the number of values to be returned; confirm that this is less than the
# number of available values
while(grepl('[0-9]+', howmany)==FALSE){
  howmany <- readline(prompt=paste("How many of the",nrow(df),"values are to be returned? "))
  while (howmany > nrow(df)){
    howmany <- readline(prompt="That is more than the number of input values. Try again or hit ENTER to exit: ")
    if (howmany == ''){break}
  }
}

# If the input are numbers, should the output be sorted from smallest to largest?
if((all(grepl('.*[0-9\\.].*', df[,1]))==TRUE) & (grepl('^[YyNn].*', sorting_bool)==FALSE)){
  while(grepl('^[YyNn].*', sorting_bool)==FALSE){
    sorting_bool <- readline(prompt="Should returned values be sorted from smallest to largest? ")
  }
}

# Take the random sample
sample.df <- as.data.frame(df[sample(nrow(df), howmany),])

# Sort, if required
if(grepl('^[Yy].*', sorting_bool)){sample.df[,1] <- sort(sample.df[,1])}

# View and Save output file, if required
View(sample.df)
while(grepl('^[YyNn].*', save_bool)==FALSE){
  save_bool <- readline(prompt="Would you like to save your returned values as a .csv? ")
}
if(grepl('^[Yy].*', save_bool)){
  filename.export <- readline(prompt = "Save output .csv as: ")
  setwd(tclvalue(tkchooseDirectory()))
  write.table(sample.df, paste(str_replace_all(filename.export, "[^[:alnum:]]", " "),'.csv', sep=''), sep=',', row.names = FALSE, col.names = FALSE)
  
}
