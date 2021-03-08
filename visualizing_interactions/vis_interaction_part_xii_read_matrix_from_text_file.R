library("tseries")
library('plot.matrix')

table_1 <- read.table("data/1981-05-03.txt")

table_2 <- read.table("data/1981-05-02.txt")

t_3 <- table_1 * table_2

# Now write 't_3' into downloads
write.table(t_3, "/Users/jeewantha/Downloads/t_3_output.txt",col.names = F, row.names = F)

read_in <- read.table("/Users/jeewantha/Downloads/t_3_output.txt")

aa <- as.matrix(read.table("data/1981-05-02.txt"))

image(aa)


# Read the folder contents

# my_files <- Sys.glob(file.path("/Volumes/JB_DRIVE/RESEARCH/ROMS/DATA/", "do", "*.txt"))
my_files <- Sys.glob(file.path("/Volumes/JB_DRIVE/RESEARCH/ROMS/DATA/do", "*.txt"))

df <- data.frame(filename=my_files, stringsAsFactors = FALSE)

table_4 <- read.table(df$filename[1])



######################## 
