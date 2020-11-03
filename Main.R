# install.packages('ggplot2')
library("ggplot2")

# import file, path may vary from pc to pc
data = read.csv(file="D:/Downloads (HDD)/APU Courses/Degree Year 2/Sem 1/PFDA/Assignment/Program/weatherdata.csv", header=TRUE, sep=",")
options(max.print=1000000)
print(colnames(data))


