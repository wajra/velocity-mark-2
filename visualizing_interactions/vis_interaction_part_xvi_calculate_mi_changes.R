# Read the MI from 1980 to 1990 and then 2000 to 2010
# Then calculate the difference and see

library("tseries")
library('plot.matrix')

# Read Fall 1980s

fall_1980s <- as.matrix(read.table("/Users/jeewantha/Downloads/mi_1980s_fall.txt"))

# Read Fall 2000s

fall_2000s <- as.matrix(read.table("/Users/jeewantha/Downloads/mi_2000s_fall.txt"))

delta_mi_fall <- fall_2000s - fall_1980s

# Replace NaN with 0
delta_mi_fall[is.nan(delta_mi_fall)] <- 0

# Write delta_mi

write.table(delta_mi_fall,
            "/Users/jeewantha/Downloads/delta_mi_fall.txt",
            col.names=F, row.names=F)


################# Now for Spring ##########################

# Read Spring 1980s

spring_1980s <- as.matrix(read.table("/Users/jeewantha/Downloads/mi_1980s_spring.txt"))

# Read Spring 2000s

spring_2000s <- as.matrix(read.table("/Users/jeewantha/Downloads/mi_2000s_spring.txt"))

delta_mi_spring <- spring_2000s - spring_1980s

# Replace NaN with 0
delta_mi_spring[is.nan(delta_mi_spring)] <- 0

# Write delta_mi

write.table(delta_mi_spring,
            "/Users/jeewantha/Downloads/delta_mi_spring.txt",
            col.names=F, row.names=F)
