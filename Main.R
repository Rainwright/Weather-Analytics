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
  # print(colnames(data))
  return (data)
}

farenheitToCelcius = function(farenheit){
  # convert temperature of unit farenheit to celcius
  return (round((farenheit - 32) * 5/9, 2))
}

# Analysis 1
analysis1 = function(data) {
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
  
  # print(average_daily_temp[which.min(average_daily_temp$AverageTemperature),])
  # print(average_daily_temp[which.max(average_daily_temp$AverageTemperature),])
  # print(average_daily_temp[(average_daily_temp$Origin == "JFK"),][which.min(average_daily_temp$AverageTemperature),])
  # print(average_daily_temp[(average_daily_temp$Origin == "JFK"),][which.max(average_daily_temp$AverageTemperature),])
  # print(average_daily_temp[(average_daily_temp$Origin == "LGA"),][which.min(average_daily_temp$AverageTemperature),])
  # print(average_daily_temp[(average_daily_temp$Origin == "LGA"),][which.max(average_daily_temp$AverageTemperature),])
  
  # return (plot);
  return(list(plot,
              average_daily_temp[which.min(average_daily_temp$AverageTemperature),],
              average_daily_temp[which.max(average_daily_temp$AverageTemperature),],
              average_daily_temp[(average_daily_temp$Origin == "JFK"),][which.min(average_daily_temp$AverageTemperature),],
              average_daily_temp[(average_daily_temp$Origin == "JFK"),][which.max(average_daily_temp$AverageTemperature),],
              average_daily_temp[(average_daily_temp$Origin == "LGA"),][which.min(average_daily_temp$AverageTemperature),],
              average_daily_temp[(average_daily_temp$Origin == "LGA"),][which.max(average_daily_temp$AverageTemperature),]
              ))
}

# Analysis2
analysis2 = function(data, origin) {
  data = switch(origin, 
                "1"=data[data$origin == "JFK",], 
                "2"=data[data$origin == "LGA",], 
                data)

  plot = ggplot(na.omit(data), aes(wind_speed, wind_gust, color=origin)) + 
    geom_jitter() + 
    geom_smooth(method='lm')
  
  return (plot)
}

main = function() {
  data = loadData()
  
  # change old time to proper date time format
  data$new_time = as.Date(strptime(data$time_hour, "%d/%m/%Y %H:%M"))
  data$new_time_hour = as.POSIXct(strptime(data$time_hour, "%d/%m/%Y %H:%M"))
  # View(data)
  # max(data$wind_speed, na.rm = TRUE)
  # max(data$wind_gust, na.rm = TRUE)
  # max(data$wind_dir, na.rm = TRUE)

  # set up server and ui
  ui <- fluidPage(
    navlistPanel(
      tabPanel(title = "Analysis 1",
               plotOutput("diagram1"),
               tags$div(id="txt", analysis1(data)[2])
      ),
      tabPanel(title = "Analysis 2",
               plotOutput("diagram2"),
               selectInput("origin", h3("Select Origin"),
                           choices = list("JFK" = 1,
                                          "LGA" = 2,
                                          "JFK and LGA" = 3),
                           selected = 1)
      )
    )
  )

  server <- function(input, output){
    output$diagram1 <- renderPlot({
      analysis1(data)[1]
    })
    
    output$diagram2 <- renderPlot({
      analysis2(data,input$origin)
    })
  }

  shinyApp(ui = ui, server = server)
}

main()

