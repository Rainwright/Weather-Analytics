# rstudio theme
# rstudioapi::addTheme("https://raw.githubusercontent.com/batpigandme/night-owlish/master/rstheme/night-owlish.rstheme", apply = TRUE)

# necessary library installation
# install.packages('ggplot2')
# install.packages('dplyr')
# install.packages('shiny')

library("ggplot2")
library("dplyr")
library("shiny")

# functions
loadData = function() {
  # import file, path may vary from pc to pc
  # data = read.csv(file="D:/Downloads (HDD)/APU Courses/Degree Year 2/Sem 1/PFDA/Assignment/Program/weatherdata.csv", header=TRUE, sep=",")
  data = read.csv(file="E:/Documents/R Projects/Weather-Analytics/weatherdata.csv", header=TRUE, sep=",")
  options(max.print=1000000)
  print(colnames(data))
  return (data)
}

farenheitToCelcius = function(farenheit){
  # convert temperature of unit farenheit to celcius
  return (round((farenheit - 32) * 5/9, 2))
}

# Analysis 1
analysis1 = function() {
  temp_temperature = 0
  temperature_counter = 0
  origin_column = c()
  date_column = c()
  average_temperature_column = c()
  
  for(i in 1:nrow(data)) {
    if(i == 1) {
      currentDate = data$new_time[i]
    } else {
      if(data$new_time[i] == currentDate) {
        # add temperature to cumulative
        temp_temperature = temp_temperature + farenheitToCelcius(data$temp[i])
        temperature_counter = temperature_counter + 1
      } else {
        origin_column = append(origin_column, data$origin[i - 1])
        date_column = append(date_column, currentDate)
        average_temperature_column = append(average_temperature_column, round(temp_temperature / temperature_counter, 2))
        # reset variables
        temp_temperature = 0
        temperature_counter = 0
        # change time to next day
        currentDate = data$new_time[i]
      }
    }
  }
  
  average_daily_temp = data.frame("Origin"=origin_column, "Date"=date_column, "AverageTemperature"=average_temperature_column)
  plot = ggplot(average_daily_temp, aes(Date, AverageTemperature, color=Origin)) + geom_point()
  print(plot)
  
  print(average_daily_temp[which.min(average_daily_temp$AverageTemperature),])
  print(average_daily_temp[which.max(average_daily_temp$AverageTemperature),])
  print(average_daily_temp[(average_daily_temp$Origin == "JFK"),][which.min(average_daily_temp$AverageTemperature),])
  print(average_daily_temp[(average_daily_temp$Origin == "JFK"),][which.max(average_daily_temp$AverageTemperature),])
  print(average_daily_temp[(average_daily_temp$Origin == "LGA"),][which.min(average_daily_temp$AverageTemperature),])
  print(average_daily_temp[(average_daily_temp$Origin == "LGA"),][which.max(average_daily_temp$AverageTemperature),])
}

# Analysis2
analysis2 = function() {
  # something about visibilty
  plot = ggplot(data, aes(new_time_hour, visib, color=origin)) + 
    geom_histogram() + 
    scale_x_datetime(limits=as.POSIXct(c("2013-01-01 01:00:00","2013-01-01 23:00:00")))
  print(plot)
}

data = loadData()

# change old time to proper date time format
data$new_time = as.Date(strptime(data$time_hour, "%d/%m/%Y %H:%M"))
data$new_time_hour = as.POSIXct(strptime(data$time_hour, "%d/%m/%Y %H:%M"))
View(data)
analysis1()
analysis2()

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
