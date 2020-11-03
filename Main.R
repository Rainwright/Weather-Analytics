# install.packages('ggplot2')
# install.packages('dplyr')
# install.packages('shiny')
library("ggplot2")
library("dplyr")
library("shiny")

# import file, path may vary from pc to pc
data = read.csv(file="D:/Downloads (HDD)/APU Courses/Degree Year 2/Sem 1/PFDA/Assignment/Program/weatherdata.csv", header=TRUE, sep=",")
options(max.print=1000000)
print(colnames(data))
View(data)

# set up server and ui
# ui <- fluidPage(
#   h1("My Shiny App"),
#   p(style = "font-family:Impact",
#     "See other apps in the",
#     a("Shiny Showcase",
#       href = "http://www.rstudio.com/
#       products/shiny/shiny-user-showcase/")
#   )
# )
# 
# server <- function(input, output){}
# 
# shinyApp(ui = ui, server = server)
