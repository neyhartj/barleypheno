### Sandbox


# Set working directory
wd <- "C:/Users/Jeff/Google Drive/Barley Lab/Projects/Genomic Selection/Phenotypic Data/Raw/2015/Malt Quality/"

# List the available files
files <- list.files(wd)
# Append the working direcotyr
files <- paste(wd, files, sep = "")
# Pick out the relavant ones
files <- grep(pattern = "15 Smith MN Group[0-9]* Data.xlsx", x = files, value = T)

# Separate into S2C1 and S2TP
files.s2c1 <- files[!grepl(pattern = "22", x = files)]
files.s2tp <- files[grepl(pattern = "22", x = files)]

s2c1.maltq <- barleypheno::read.maltq(files = files.s2c1)


maltq.df <- s2c1.maltq$data
lab.to.plot <- read.csv("C:/Users/Jeff/Google Drive/Barley Lab/Projects/Genomic Selection/Phenotypic Data/Raw/2015/Malt Quality/malt_quality_2015_labno_plot.csv",
                        as.is = T)
field.book <- read.csv("C:/Users/Jeff/Google Drive/Barley Lab/Projects/Genomic Selection/Plot Design/2015/Field.Book/S2C1 Yield/S2C1_CR15_field.book.csv",
                       as.is = T)
