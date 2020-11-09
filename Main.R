# rstudio theme
# rstudioapi::addTheme("https://raw.githubusercontent.com/batpigandme/night-owlish/master/rstheme/night-owlish.rstheme", apply = TRUE)

# necessary packages/library installation
# install.packages('ggplot2')
# install.packages('dplyr')
# install.packages('shiny')
# install.packages('scales')

library("ggplot2")
library("dplyr")
library("shiny")
library("scales")

# functions
# load data from csv file
loadData = function() {
  # import file, path may vary from pc to pc
  # data = read.csv(file="D:/Downloads (HDD)/APU Courses/Degree Year 2/Sem 1/PFDA/Assignment/Program/weatherdata.csv", header=TRUE, sep=",")
  data = read.csv(file="E:/Documents/R Projects/Weather-Analytics/weatherdata.csv", header=TRUE, sep=",")
  options(max.print=1000000)
  print(colnames(data))
  return (data)
}

# convert farenheit to celcius
farenheitToCelcius = function(farenheit){
  # convert temperature of unit farenheit to celcius
  return (round((farenheit - 32) * 5/9, 2))
}

filterOrigin = function(data, origin) {
  data = switch(origin, 
                "2"=data[data$origin == "JFK",], 
                "3"=data[data$origin == "LGA",], 
                data)
  return (data)
}

filterSeason = function(data, season) {
  data = switch(season,
                "2"=data[(data['new_time'] >= "2013-03-20") & (data['new_time'] <= "2013-06-21"),],
                "3"=data[(data['new_time'] >= "2013-06-21") & (data['new_time'] <= "2013-09-22"),],
                "4"=data[(data['new_time'] >= "2013-09-22") & (data['new_time'] <= "2013-12-21"),],
                "5"=data[(data['new_time'] <= "2013-03-20") | (data['new_time'] >= "2013-12-21"),],
                data)
  return (data)
}

calculateAverageAgainstTime = function(data, subject) {
  temp_subject = 0
  subject_counter = 0
  origin_column = c()
  date_column = c()
  average_subject_column = c()
  
  for(i in 1:nrow(data)) {
    if(i == 1) {
      currentDate = data$new_time[i]
    } else {
      if(data$new_time[i] == currentDate) {
        # add subject to cumulative
        temp_subject = temp_subject + subject[i]
        subject_counter = subject_counter + 1
      } else {
        origin_column = append(origin_column, data$origin[i - 1])
        date_column = append(date_column, currentDate)
        average_subject_column = append(average_subject_column, round(temp_subject / subject_counter, 2))
        # reset variables
        temp_subject = 0
        subject_counter = 0
        # change time to next day
        currentDate = data$new_time[i]
      }
    }
  }
  
  average_daily_subject = data.frame("Origin"=origin_column, "Date"=date_column, "AverageValue"=average_subject_column)
  return (average_daily_subject)
}

# Analysis 1
analysis1 = function(data, season, origin) {
  data = filterSeason(data, season)
  
  original_data = data
  
  data = filterOrigin(data, origin)
  
  average_daily_temp = calculateAverageAgainstTime(data, data$temp)
  average_daily_temp$AverageValue = farenheitToCelcius(average_daily_temp$AverageValue)
  
  plot1 = ggplot(average_daily_temp, aes(Date, AverageValue, color=Origin)) + 
    geom_point() + 
    geom_line()
  
  data$temp = farenheitToCelcius(data$temp)
  mean_temp = mean(data$temp)
  median_temp = median(data$temp)
  sd_temp = sd(data$temp)
  
  plot1.1 = ggplot(data, aes(x = temp, fill=origin)) + 
    geom_histogram() + 
    geom_vline(aes(xintercept = mean_temp), colour="red") +
    geom_vline(aes(xintercept = median_temp), colour="blue", linetype="dashed")

  print(plot1)
  return(list(plot1,
              plot1.1,
              # mean value
              round(mean_temp, 2),
              # median value
              round(median_temp, 2),
              # sd value
              round(sd_temp, 2),
              # overall min value and date
              min(average_daily_temp$AverageValue),
              # overall max value and date
              max(average_daily_temp$AverageValue),
              # jfk min value and date
              farenheitToCelcius(min(original_data$temp[original_data$origin == "JFK"])),
              # jfk max value and date
              farenheitToCelcius(max(original_data$temp[original_data$origin == "JFK"])),
              # lga min value and date
              farenheitToCelcius(min(original_data$temp[original_data$origin == "LGA"])),
              # lga max value and date
              farenheitToCelcius(max(original_data$temp[original_data$origin == "LGA"]))
              ))
}

# Analysis2
analysis2 = function(data, season, origin) {
  data = filterSeason(data, season)
  
  original_data = na.omit(data)
  
  data = na.omit(filterOrigin(data, origin))
  
  average_daily_winds = na.omit(calculateAverageAgainstTime(data, data$wind_speed))
  
  plot1 = ggplot(average_daily_winds, aes(Date, AverageValue, color=Origin)) + 
    geom_point() + 
    geom_line()
  
  mean_winds = mean(data$wind_speed)
  median_winds = median(data$wind_speed)
  sd_winds = sd(data$wind_speed)
  
  plot1.1 = ggplot(data, aes(x = wind_speed, fill=origin)) + 
    geom_histogram() + 
    geom_vline(aes(xintercept = mean_winds), colour="red") +
    geom_vline(aes(xintercept = median_winds), colour="blue", linetype="dashed") + 
    scale_x_continuous(breaks = scales::pretty_breaks(n = 20))
  
  print(plot1)
  return (list(plot1,
               plot1.1,
               # mean value
               round(mean_winds, 2),
               # median value
               round(median_winds, 2),
               # sd value
               round(sd_winds, 2),
               # overall min value and date
               min(average_daily_winds$AverageValue),
               # overall max value and date
               max(average_daily_winds$AverageValue),
               # jfk min value and date
               round(min(original_data$wind_speed[original_data$origin == "JFK"]), 2),
               # jfk max value and date
               round(max(original_data$wind_speed[original_data$origin == "JFK"]), 2),
               # lga min value and date
               round(min(original_data$wind_speed[original_data$origin == "LGA"]), 2),
               # lga max value and date
               round(max(original_data$wind_speed[original_data$origin == "LGA"]), 2)
               ))
}

analysis2.1 = function(data, season, origin) {
  data = filterSeason(data, season)
  
  original_data = na.omit(data)
  
  data = na.omit(filterOrigin(data, origin))
  
  average_daily_windg = na.omit(calculateAverageAgainstTime(data, data$wind_gust))
  
  plot1 = ggplot(average_daily_windg, aes(Date, AverageValue, color=Origin)) + 
    geom_point() + 
    geom_line()
  
  mean_windg = mean(data$wind_gust)
  median_windg = median(data$wind_gust)
  sd_windg = sd(data$wind_gust)
  
  plot1.1 = ggplot(data, aes(x = wind_gust, fill=origin)) + 
    geom_histogram() + 
    geom_vline(aes(xintercept = mean_windg), colour="red") +
    geom_vline(aes(xintercept = median_windg), colour="blue", linetype="dashed") + 
    scale_x_continuous(breaks = scales::pretty_breaks(n = 20))
  
  print(plot1)
  return (list(plot1,
               plot1.1,
               # mean value
               round(mean_windg, 2),
               # median value
               round(median_windg, 2),
               # sd value
               round(sd_windg, 2),
               # overall min value and date
               min(average_daily_windg$AverageValue),
               # overall max value and date
               max(average_daily_windg$AverageValue),
               # jfk min value and date
               round(min(original_data$wind_gust[original_data$origin == "JFK"]), 2),
               # jfk max value and date
               round(max(original_data$wind_gust[original_data$origin == "JFK"]), 2),
               # lga min value and date
               round(min(original_data$wind_gust[original_data$origin == "LGA"]), 2),
               # lga max value and date
               round(max(original_data$wind_gust[original_data$origin == "LGA"]), 2)
  ))
}

analysis2.2 = function(data) {
  # plot1 = ggplot(data, aes(x = wind_dir, fill=origin)) + 
  #   geom_histogram()
  # 
  plot1 = ggplot(data, aes(y = wind_dir, x = 1, fill=origin)) + 
    geom_boxplot() + 
    scale_y_continuous(breaks = scales::pretty_breaks(n = 18))
  
  print(plot1)
  return(list(plot1,
              summary(data$wind_dir[data$origin == "JFK"]),
              summary(data$wind_dir[data$origin == "LGA"])
              ))
}

# Analysis3
analysis3 = function(data, season, origin) {
  data = filterSeason(data, season)
  
  original_data = na.omit(data)
  
  data = na.omit(filterOrigin(data, origin))
  
  average_daily_precip = na.omit(calculateAverageAgainstTime(data, data$precip))
  
  plot1 = ggplot(average_daily_precip, aes(Date, AverageValue, color=Origin)) + 
    geom_point() + 
    geom_line()
  
  # mean_precip = mean(data$precip)
  # median_precip = median(data$precip)
  # sd_precip = sd(data$precip)
  
  mean_precip = mean(data$precip[data$precip > 0])
  median_precip = median(data$precip[data$precip > 0])
  sd_precip = sd(data$precip[data$precip > 0])
  
  plot1.1 = ggplot(data[data$precip > 0,], aes(x = precip, fill=origin)) + 
    geom_histogram() + 
    geom_vline(aes(xintercept = mean_precip), colour="red") +
    geom_vline(aes(xintercept = median_precip), colour="blue", linetype="dashed") + 
    scale_x_continuous(breaks = scales::pretty_breaks(n = 20))
  
  print(plot1)
  return (list(plot1,
               plot1.1,
               # mean value
               round(mean_precip, 2),
               # median value
               round(median_precip, 2),
               # sd value
               round(sd_precip, 2),
               # overall min value and date
               min(average_daily_precip$AverageValue),
               # overall max value and date
               max(average_daily_precip$AverageValue),
               # jfk min value and date
               round(min(original_data$precip[original_data$origin == "JFK"]), 2),
               # jfk max value and date
               round(max(original_data$precip[original_data$origin == "JFK"]), 2),
               # lga min value and date
               round(min(original_data$precip[original_data$origin == "LGA"]), 2),
               # lga max value and date
               round(max(original_data$precip[original_data$origin == "LGA"]), 2)
  ))
}

analysis4 = function(data, season, origin) {
  data = filterSeason(data, season)
  
  original_data = data
  
  data = filterOrigin(data, origin)
  
  average_daily_visib = calculateAverageAgainstTime(data, data$visib)
  
  plot1 = ggplot(average_daily_visib, aes(Date, AverageValue, color=Origin)) + 
    geom_point() + 
    geom_line()
  
  # mean_precip = mean(data$precip)
  # median_precip = median(data$precip)
  # sd_precip = sd(data$precip)
  
  mean_visib = mean(data$visib[data$visib < 10])
  median_visib = median(data$visib[data$visib < 10])
  sd_visib = sd(data$visib[data$visib < 10])
  
  plot1.1 = ggplot(data[data$visib < 10,], aes(x = visib, fill=origin)) + 
    geom_histogram() + 
    geom_vline(aes(xintercept = mean_visib), colour="red") +
    geom_vline(aes(xintercept = median_visib), colour="blue", linetype="dashed") + 
    scale_x_continuous(breaks = scales::pretty_breaks(n = 20))
  
  print(plot1)
  return (list(plot1,
               plot1.1,
               # mean value
               round(mean_visib, 2),
               # median value
               round(median_visib, 2),
               # sd value
               round(sd_visib, 2),
               # overall min value and date
               min(average_daily_visib$AverageValue),
               # overall max value and date
               max(average_daily_visib$AverageValue),
               # jfk min value and date
               round(min(original_data$visib[original_data$origin == "JFK"]), 2),
               # jfk max value and date
               round(max(original_data$visib[original_data$origin == "JFK"]), 2),
               # lga min value and date
               round(min(original_data$visib[original_data$origin == "LGA"]), 2),
               # lga max value and date
               round(max(original_data$visib[original_data$origin == "LGA"]), 2)
  ))
}

# Analysis 5
analysis5 = function(data, season, origin) {
  data = filterSeason(data, season)
  
  original_data = data
  
  data = filterOrigin(data, origin)
  
  average_daily_dewp = calculateAverageAgainstTime(data, data$dewp)
  average_daily_dewp$AverageValue = farenheitToCelcius(average_daily_dewp$AverageValue)

  plot1 = ggplot(average_daily_dewp, aes(Date, AverageValue, color=Origin)) + 
    geom_point() + 
    geom_line()
  
  data$dewp = farenheitToCelcius(data$dewp)
  mean_dewp = mean(data$dewp)
  median_dewp = median(data$dewp)
  sd_dewp = sd(data$dewp)

  plot1.1 = ggplot(data, aes(x = dewp, fill=origin)) + 
    geom_histogram() + 
    geom_vline(aes(xintercept = mean_dewp), colour="red") +
    geom_vline(aes(xintercept = median_dewp), colour="blue", linetype="dashed") + 
    scale_x_continuous(breaks = scales::pretty_breaks(n = 20))
  
  print(plot1)
  return (list(plot1,
               plot1.1,
               # mean value
               round(mean_dewp, 2),
               # median value
               round(median_dewp, 2),
               # sd value
               round(sd_dewp, 2),
               # overall min value and date
               min(average_daily_dewp$AverageValue),
               # overall max value and date
               max(average_daily_dewp$AverageValue),
               # jfk min value and date
               farenheitToCelcius(min(original_data$dewp[original_data$origin == "JFK"])),
               # jfk max value and date
               farenheitToCelcius(max(original_data$dewp[original_data$origin == "JFK"])),
               # lga min value and date
               farenheitToCelcius(min(original_data$dewp[original_data$origin == "LGA"])),
               # lga max value and date
               farenheitToCelcius(max(original_data$dewp[original_data$origin == "LGA"]))
  ))
}

# Analysis 6
analysis6 = function(data, season, origin) {
  data = filterSeason(data, season)
  
  original_data = data
  
  data = filterOrigin(data, origin)
  
  average_daily_humid = calculateAverageAgainstTime(data, data$humid)
  
  # plot1 = ggplot(data, aes(new_time, humid, color=origin)) + 
  #   geom_point() + 
  #   geom_line()
  
  plot1 = ggplot(average_daily_humid, aes(Date, AverageValue, color=Origin)) +
    geom_point() +
    geom_line()
  
  mean_humid = mean(data$humid)
  median_humid = median(data$humid)
  sd_humid = sd(data$humid)
  
  plot1.1 = ggplot(data, aes(x = humid, fill=origin)) + 
    geom_histogram() + 
    geom_vline(aes(xintercept = mean_humid), colour="red") +
    geom_vline(aes(xintercept = median_humid), colour="blue", linetype="dashed") + 
    scale_x_continuous(breaks = scales::pretty_breaks(n = 20))
  
  print(plot1)
  return (list(plot1,
               plot1.1,
               # mean value
               round(mean_humid, 2),
               # median value
               round(median_humid, 2),
               # sd value
               round(sd_humid, 2),
               # overall min value and date
               min(average_daily_humid$AverageValue),
               # overall max value and date
               max(average_daily_humid$AverageValue),
               # jfk min value and date
               round(min(original_data$humid[original_data$origin == "JFK"]), 2),
               # jfk max value and date
               round(max(original_data$humid[original_data$origin == "JFK"]), 2),
               # lga min value and date
               round(min(original_data$humid[original_data$origin == "LGA"]), 2),
               # lga max value and date
               round(max(original_data$humid[original_data$origin == "LGA"]), 2)
  ))
}

# analysis5 = function(data, quartile, origin) {
#   # plot = ggplot(na.omit(data[data$visib < 10,]), aes(x =visib, group=origin, fill=origin)) +
#   #   geom_histogram() +
#   #   scale_x_continuous(breaks = round(seq(min(data$visib), max(data$visib), by = 0.5),1))
#   
#   min_visib = 10
#   visib_counter = 0
#   origin_column = c()
#   date_column = c()
#   min_visib_column = c()
#   
#   for(i in 1:nrow(data)) {
#     if(i == 1) {
#       currentDate = data$new_time[i]
#     } else {
#       if(data$new_time[i] == currentDate) {
#         if(data$visib[i] < min_visib) {
#           min_visib = data$visib[i]
#         }
#       } else {
#         origin_column = append(origin_column, data$origin[i - 1])
#         date_column = append(date_column, currentDate)
#         min_visib_column = append(min_visib_column, min_visib)
#         # reset variables
#         min_visib = 10
#         visib_counter = 0
#         # change time to next day
#         currentDate = data$new_time[i]
#       }
#     }
#   }
#   
#   min_daily_visib = data.frame("Origin"=origin_column, "Date"=date_column, "MinVisib"=min_visib_column)
#   
#   min_daily_visib = switch(quartile,
#                           "1"=min_daily_visib[(min_daily_visib['Date'] >= "2013-01-01") & (min_daily_visib['Date'] <= "2013-04-01"),],
#                           "2"=min_daily_visib[(min_daily_visib['Date'] >= "2013-04-01") & (min_daily_visib['Date'] <= "2013-07-01"),],
#                           "3"=min_daily_visib[(min_daily_visib['Date'] >= "2013-07-01") & (min_daily_visib['Date'] <= "2013-10-01"),],
#                           "4"=min_daily_visib[(min_daily_visib['Date'] >= "2013-10-01") & (min_daily_visib['Date'] <= "2013-12-30"),],
#                           min_daily_visib)
#   
#   min_daily_visib = switch(origin, 
#                 "1"=min_daily_visib[min_daily_visib$Origin == "JFK",], 
#                 "2"=min_daily_visib[min_daily_visib$Origin == "LGA",], 
#                 min_daily_visib)
#   
#   plot = ggplot(min_daily_visib[min_daily_visib$MinVisib < 10,], aes(Date, MinVisib, color=Origin)) +
#     geom_jitter() +
#     geom_line() +
#     scale_y_continuous(breaks = round(seq(min(min_daily_visib$MinVisib), max(min_daily_visib$MinVisib), by = 0.5),1))
#   return (plot)
# }
#  
# Analysis 6
# analysis6 = function(data) {
#   # plot = ggplot(na.omit(data[(data$wind_dir >= 45) & (data$wind_dir <= 135),]), aes(x = new_time, group=origin, fill=origin)) + 
#   #   geom_histogram() + 
#   #   # scale_x_date(breaks="month", labels=date_format("%b"))
#   #   scale_x_date(breaks = breaks_pretty(10))
#   plot = ggplot(na.omit(data[(data$wind_dir > 0) & (data$wind_dir < 180),]), aes(x = new_time, group=origin, fill=origin)) + 
#     geom_histogram() + 
#     # scale_x_date(breaks="month", labels=date_format("%b"))
#     scale_x_date(breaks = breaks_pretty(10))
#   return (plot)
# }

main = function() {
  data = loadData()
  
  # change old time to proper date time format
  data$new_time = as.Date(strptime(data$time_hour, "%d/%m/%Y %H:%M"))
  data$new_time_hour = as.POSIXct(strptime(data$time_hour, "%d/%m/%Y %H:%M"))

  # set up server and ui
  ui <- fluidPage(
    navlistPanel(
      tabPanel(title = "Analysis 1",
               fluidRow(
                 column(4,
                        selectInput("season_1", h3("Select Season"),
                                    choices = list("Overall" = 1,
                                                   "Spring" = 2,
                                                   "Summer" = 3,
                                                   "Autumn" = 4,
                                                   "Winter" = 5),
                                    selected = 1)
                        ),
                 column(4,
                        selectInput("origin_1", h3("Select Origin"),
                                    choices = list("JFK and LGA" = 1,
                                                   "JFK" = 2,
                                                   "LGA" = 3),
                                    selected = 1)
                        )
               ),
               
               h3("Average Temperature against Time (\u00B0C)"),
               plotOutput("diagram1"),
               
               h3("Mean and Median of Temperature (\u00B0C)"),
               
               fluidRow(
                 column(4,
                        h5(textOutput("text1_3"))
                 ),
                 column(4,
                        h5(textOutput("text1_4"))
                 ),
                 column(4,
                        h5(textOutput("text1_5"))
                 ),
               ),
               
               plotOutput("diagram1_1"),
               
               h4("Overall Temperatures:"),
               fluidRow(
                 column(3,
                        h5(textOutput("text1_6"))
                 ),
                 column(3,
                        h5(textOutput("text1_7"))
                 ),
               ),
               
               h4("JFK Temperatures:"),
               fluidRow(
                 column(3,
                        h5(textOutput("text1_8"))
                 ),
                 column(3,
                        h5(textOutput("text1_9"))
                 ),
               ),
               
               h4("LGA Temperatures:"),
               fluidRow(
                 column(3,
                        h5(textOutput("text1_10"))
                 ),
                 column(3,
                        h5(textOutput("text1_11"))
                 ),
               ),
      ),
      tabPanel(title = "Analysis 2",
               fluidRow(
                 column(4,
                        selectInput("season_2", h3("Select Season"),
                                    choices = list("Overall" = 1,
                                                   "Spring" = 2,
                                                   "Summer" = 3,
                                                   "Autumn" = 4,
                                                   "Winter" = 5),
                                    selected = 1)
                 ),
                 column(4,
                        selectInput("origin_2", h3("Select Origin"),
                                    choices = list("JFK and LGA" = 1,
                                                   "JFK" = 2,
                                                   "LGA" = 3),
                                    selected = 1)
                        )
               ),
               
               h3("Average Wind Speed against Time (mph)"),
               plotOutput("diagram2"),
               
               h3("Mean and Median of Wind Speed (mph)"),
               
               fluidRow(
                 column(4,
                        h5(textOutput("text2_3"))
                 ),
                 column(4,
                        h5(textOutput("text2_4"))
                 ),
                 column(4,
                        h5(textOutput("text2_5"))
                 ),
               ),
               
               plotOutput("diagram2_1"),
               
               h4("Overall Wind Speed:"),
               fluidRow(
                 column(3,
                        h5(textOutput("text2_6"))
                 ),
                 column(3,
                        h5(textOutput("text2_7"))
                 ),
               ),
               
               h4("JFK Wind Speed:"),
               fluidRow(
                 column(3,
                        h5(textOutput("text2_8"))
                 ),
                 column(3,
                        h5(textOutput("text2_9"))
                 ),
               ),
               
               h4("LGA Wind Speed:"),
               fluidRow(
                 column(3,
                        h5(textOutput("text2_10"))
                 ),
                 column(3,
                        h5(textOutput("text2_11"))
                 ),
               ),
      ),
      tabPanel(title = "Analysis 2.1",
               fluidRow(
                 column(4,
                        selectInput("season_2.1", h3("Select Season"),
                                    choices = list("Overall" = 1,
                                                   "Spring" = 2,
                                                   "Summer" = 3,
                                                   "Autumn" = 4,
                                                   "Winter" = 5),
                                    selected = 1)
                 ),
                 column(4,
                        selectInput("origin_2.1", h3("Select Origin"),
                                    choices = list("JFK and LGA" = 1,
                                                   "JFK" = 2,
                                                   "LGA" = 3),
                                    selected = 1)
                 )
               ),
               
               h3("Average Wind Gusts against Time (mph)"),
               plotOutput("diagram2.1"),
               
               h3("Mean and Median of Wind Gusts (mph)"),
               
               fluidRow(
                 column(4,
                        h5(textOutput("text2.1_3"))
                 ),
                 column(4,
                        h5(textOutput("text2.1_4"))
                 ),
                 column(4,
                        h5(textOutput("text2.1_5"))
                 ),
               ),
               
               plotOutput("diagram2.1_1"),
               
               h4("Overall Wind Gusts:"),
               fluidRow(
                 column(3,
                        h5(textOutput("text2.1_6"))
                 ),
                 column(3,
                        h5(textOutput("text2.1_7"))
                 ),
               ),
               
               h4("JFK Wind Gusts:"),
               fluidRow(
                 column(3,
                        h5(textOutput("text2.1_8"))
                 ),
                 column(3,
                        h5(textOutput("text2.1_9"))
                 ),
               ),
               
               h4("LGA Wind Gusts:"),
               fluidRow(
                 column(3,
                        h5(textOutput("text2.1_10"))
                 ),
                 column(3,
                        h5(textOutput("text2.1_11"))
                 ),
               ),
      ),
      tabPanel(title = "Analysis 2.2",
               h3("Wind Direction Count (mph)"),
               plotOutput("diagram2.2"),
               
               h4("JFK Wind Direction Summary:"),
               h5(textOutput("text2.2_1")),
               h4("LGA Wind Direction Summary:"),
               h5(textOutput("text2.2_2"))
      ),
      tabPanel(title = "Analysis 3",
               fluidRow(
                 column(4,
                        selectInput("season_3", h3("Select Season"),
                                    choices = list("Overall" = 1,
                                                   "Spring" = 2,
                                                   "Summer" = 3,
                                                   "Autumn" = 4,
                                                   "Winter" = 5),
                                    selected = 1)
                 ),
                 column(4,
                        selectInput("origin_3", h3("Select Origin"),
                                    choices = list("JFK and LGA" = 1,
                                                   "JFK" = 2,
                                                   "LGA" = 3),
                                    selected = 1)
                 )
               ),
               h3("Average Precipitation against Time (inches)"),
               plotOutput("diagram3"),
               
               h3("Mean and Median of Precipitation (inches)"),
               h4("Removed 0 values as it is majority"),
               
               fluidRow(
                 column(4,
                        h5(textOutput("text3_3"))
                 ),
                 column(4,
                        h5(textOutput("text3_4"))
                 ),
                 column(4,
                        h5(textOutput("text3_5"))
                 ),
               ),
               
               plotOutput("diagram3_1"),
               
               h4("Overall Precipitation:"),
               fluidRow(
                 column(3,
                        h5(textOutput("text3_6"))
                 ),
                 column(3,
                        h5(textOutput("text3_7"))
                 ),
               ),
               
               h4("JFK Precipitation:"),
               fluidRow(
                 column(3,
                        h5(textOutput("text3_8"))
                 ),
                 column(3,
                        h5(textOutput("text3_9"))
                 ),
               ),
               
               h4("LGA Precipitation:"),
               fluidRow(
                 column(3,
                        h5(textOutput("text3_10"))
                 ),
                 column(3,
                        h5(textOutput("text3_11"))
                 ),
               ),
      ),
      tabPanel(title = "Analysis 4",
               fluidRow(
                 column(4,
                        selectInput("season_4", h3("Select Season"),
                                    choices = list("Overall" = 1,
                                                   "Spring" = 2,
                                                   "Summer" = 3,
                                                   "Autumn" = 4,
                                                   "Winter" = 5),
                                    selected = 1)
                 ),
                 column(4,
                        selectInput("origin_4", h3("Select Origin"),
                                    choices = list("JFK and LGA" = 1,
                                                   "JFK" = 2,
                                                   "LGA" = 3),
                                    selected = 1)
                 )
               ),
               h3("Average Visibility against Time (miles)"),
               plotOutput("diagram4"),

               h3("Mean and Median of Visibility (miles)"),
               h4("Removed 10 values as it is majority"),

               fluidRow(
                 column(4,
                        h5(textOutput("text4_3"))
                 ),
                 column(4,
                        h5(textOutput("text4_4"))
                 ),
                 column(4,
                        h5(textOutput("text4_5"))
                 ),
               ),
               
               plotOutput("diagram4_1"),
               
               h4("Overall Visibility:"),
               fluidRow(
                 column(3,
                        h5(textOutput("text4_6"))
                 ),
                 column(3,
                        h5(textOutput("text4_7"))
                 ),
               ),
               
               h4("JFK Visibility:"),
               fluidRow(
                 column(3,
                        h5(textOutput("text4_8"))
                 ),
                 column(3,
                        h5(textOutput("text4_9"))
                 ),
               ),
               
               h4("LGA Visibility:"),
               fluidRow(
                 column(3,
                        h5(textOutput("text4_10"))
                 ),
                 column(3,
                        h5(textOutput("text4_11"))
                 ),
               ),
      ),
      tabPanel(title = "Analysis 5",
               fluidRow(
                 column(4,
                        selectInput("season_5", h3("Select Season"),
                                    choices = list("Overall" = 1,
                                                   "Spring" = 2,
                                                   "Summer" = 3,
                                                   "Autumn" = 4,
                                                   "Winter" = 5),
                                    selected = 1)
                 ),
                 column(4,
                        selectInput("origin_5", h3("Select Origin"),
                                    choices = list("JFK and LGA" = 1,
                                                   "JFK" = 2,
                                                   "LGA" = 3),
                                    selected = 1)
                 )
               ),
               h3("Average Dewpoint against Time (\u00B0C)"),
               plotOutput("diagram5"),
               
               h3("Mean and Median of Dewpoint (\u00B0C)"),
               
               fluidRow(
                 column(4,
                        h5(textOutput("text5_3"))
                 ),
                 column(4,
                        h5(textOutput("text5_4"))
                 ),
                 column(4,
                        h5(textOutput("text5_5"))
                 ),
               ),
               
               plotOutput("diagram5_1"),
               
               h4("Overall Dewpoint:"),
               fluidRow(
                 column(3,
                        h5(textOutput("text5_6"))
                 ),
                 column(3,
                        h5(textOutput("text5_7"))
                 ),
               ),
               
               h4("JFK Dewpoint:"),
               fluidRow(
                 column(3,
                        h5(textOutput("text5_8"))
                 ),
                 column(3,
                        h5(textOutput("text5_9"))
                 ),
               ),
               
               h4("LGA Dewpoint:"),
               fluidRow(
                 column(3,
                        h5(textOutput("text5_10"))
                 ),
                 column(3,
                        h5(textOutput("text5_11"))
                 ),
               ),
      ),
      tabPanel(title = "Analysis 6",
               fluidRow(
                 column(4,
                        selectInput("season_6", h3("Select Season"),
                                    choices = list("Overall" = 1,
                                                   "Spring" = 2,
                                                   "Summer" = 3,
                                                   "Autumn" = 4,
                                                   "Winter" = 5),
                                    selected = 1)
                 ),
                 column(4,
                        selectInput("origin_6", h3("Select Origin"),
                                    choices = list("JFK and LGA" = 1,
                                                   "JFK" = 2,
                                                   "LGA" = 3),
                                    selected = 1)
                 )
               ),
               h3("Relative Humidity against Time"),
               plotOutput("diagram6"),
               
               h3("Mean and Median of Dewpoint"),
               
               fluidRow(
                 column(4,
                        h5(textOutput("text6_3"))
                 ),
                 column(4,
                        h5(textOutput("text6_4"))
                 ),
                 column(4,
                        h5(textOutput("text6_5"))
                 ),
               ),
               
               plotOutput("diagram6_1"),
               
               h4("Overall Relative Humidity:"),
               fluidRow(
                 column(3,
                        h5(textOutput("text6_6"))
                 ),
                 column(3,
                        h5(textOutput("text6_7"))
                 ),
               ),
               
               h4("JFK Relative Humidity:"),
               fluidRow(
                 column(3,
                        h5(textOutput("text6_8"))
                 ),
                 column(3,
                        h5(textOutput("text6_9"))
                 ),
               ),
               
               h4("LGA Relative Humidity:"),
               fluidRow(
                 column(3,
                        h5(textOutput("text6_10"))
                 ),
                 column(3,
                        h5(textOutput("text6_11"))
                 ),
               ),
      )
      
      # tabPanel(title = "Analysis 3",
      #          plotOutput("diagram3"),
      # ),
      # tabPanel(title = "Analysis 4",
      #          plotOutput("diagram4"),
      #          
      # ),
      # tabPanel(title = "Analysis 5",
      #          plotOutput("diagram5"),
      #          selectInput("datequartile", h3("Select Quartile"),
      #                      choices = list("1st" = 1,
      #                                     "2nd" = 2,
      #                                     "3rd" = 3,
      #                                     "4th" = 4,
      #                                     "Overall" = 5),
      #                      selected = 5),
      #          selectInput("origin_5", h3("Select Origin"),
      #                      choices = list("JFK" = 1,
      #                                     "LGA" = 2,
      #                                     "JFK and LGA" = 3),
      #                      selected = 3)
      # ),
      # tabPanel(title = "Analysis 6",
      #          plotOutput("diagram6"),
      # )
    )
  )
  
  current_analysis = list()
  server <- function(input, output){

    output$diagram1 <- renderPlot({
      current_analysis = analysis1(data, input$season_1, input$origin_1)
      current_analysis[1]
      
      output$diagram1_1 <- renderPlot({
        current_analysis[2]
      })
      
      output$text1_3 <- renderText({
        paste("Mean :", current_analysis[3])
      })
      
      output$text1_4 <- renderText({
        paste("Median :", current_analysis[4])
      })
      
      output$text1_5 <- renderText({
        paste("Standard Deviation :", current_analysis[5])
      })
      
      output$text1_6 <- renderText({
        paste("Min :", current_analysis[6])
      })
      
      output$text1_7 <- renderText({
        paste("Max :", current_analysis[7])
      })
      
      output$text1_8 <- renderText({
        paste("Min :", current_analysis[8])
      })
      
      output$text1_9 <- renderText({
        paste("Max :", current_analysis[9])
      })
      
      output$text1_10 <- renderText({
        paste("Min :", current_analysis[10])
      })
      
      output$text1_11 <- renderText({
        paste("Max :", current_analysis[11])
      })
    })
    
    output$diagram2 <- renderPlot({
      current_analysis = analysis2(data, input$season_2, input$origin_2)
      current_analysis[1]
      
      output$diagram2_1 <- renderPlot({
        current_analysis[2]
      })
      
      output$text2_3 <- renderText({
        paste("Mean :", current_analysis[3])
      })
      
      output$text2_4 <- renderText({
        paste("Median :", current_analysis[4])
      })
      
      output$text2_5 <- renderText({
        paste("Standard Deviation :", current_analysis[5])
      })
      
      output$text2_6 <- renderText({
        paste("Min :", current_analysis[6])
      })
      
      output$text2_7 <- renderText({
        paste("Max :", current_analysis[7])
      })
      
      output$text2_8 <- renderText({
        paste("Min :", current_analysis[8])
      })
      
      output$text2_9 <- renderText({
        paste("Max :", current_analysis[9])
      })
      
      output$text2_10 <- renderText({
        paste("Min :", current_analysis[10])
      })
      
      output$text2_11 <- renderText({
        paste("Max :", current_analysis[11])
      })
    })
    
    output$diagram2.1 <- renderPlot({
      current_analysis = analysis2.1(data, input$season_2.1, input$origin_2.1)
      current_analysis[1]
      
      output$diagram2.1_1 <- renderPlot({
        current_analysis[2]
      })
      
      output$text2.1_3 <- renderText({
        paste("Mean :", current_analysis[3])
      })
      
      output$text2.1_4 <- renderText({
        paste("Median :", current_analysis[4])
      })
      
      output$text2.1_5 <- renderText({
        paste("Standard Deviation :", current_analysis[5])
      })
      
      output$text2.1_6 <- renderText({
        paste("Min :", current_analysis[6])
      })
      
      output$text2.1_7 <- renderText({
        paste("Max :", current_analysis[7])
      })
      
      output$text2.1_8 <- renderText({
        paste("Min :", current_analysis[8])
      })
      
      output$text2.1_9 <- renderText({
        paste("Max :", current_analysis[9])
      })
      
      output$text2.1_10 <- renderText({
        paste("Min :", current_analysis[10])
      })
      
      output$text2.1_11 <- renderText({
        paste("Max :", current_analysis[11])
      })
    })
    
    output$diagram2.2 <- renderPlot({
      current_analysis = analysis2.2(data)
      current_analysis[1]
      
      output$text2.2_1 <- renderText({
        paste(current_analysis[2])
      })
      
      output$text2.2_2 <- renderText({
        paste(current_analysis[3])
      })
    })
    
    output$diagram3 <- renderPlot({
      current_analysis = analysis3(data, input$season_3, input$origin_3)
      current_analysis[1]
      
      output$diagram3_1 <- renderPlot({
        current_analysis[2]
      })
      
      output$text3_3 <- renderText({
        paste("Mean :", current_analysis[3])
      })
      
      output$text3_4 <- renderText({
        paste("Median :", current_analysis[4])
      })
      
      output$text3_5 <- renderText({
        paste("Standard Deviation :", current_analysis[5])
      })
      
      output$text3_6 <- renderText({
        paste("Min :", current_analysis[6])
      })
      
      output$text3_7 <- renderText({
        paste("Max :", current_analysis[7])
      })
      
      output$text3_8 <- renderText({
        paste("Min :", current_analysis[8])
      })
      
      output$text3_9 <- renderText({
        paste("Max :", current_analysis[9])
      })
      
      output$text3_10 <- renderText({
        paste("Min :", current_analysis[10])
      })
      
      output$text3_11 <- renderText({
        paste("Max :", current_analysis[11])
      })
    })
    
    output$diagram4 <- renderPlot({
      current_analysis = analysis4(data, input$season_4, input$origin_4)
      current_analysis[1]
      
      output$diagram4_1 <- renderPlot({
        current_analysis[2]
      })
      
      output$text4_3 <- renderText({
        paste("Mean :", current_analysis[3])
      })
      
      output$text4_4 <- renderText({
        paste("Median :", current_analysis[4])
      })
      
      output$text4_5 <- renderText({
        paste("Standard Deviation :", current_analysis[5])
      })
      
      output$text4_6 <- renderText({
        paste("Min :", current_analysis[6])
      })
      
      output$text4_7 <- renderText({
        paste("Max :", current_analysis[7])
      })
      
      output$text4_8 <- renderText({
        paste("Min :", current_analysis[8])
      })
      
      output$text4_9 <- renderText({
        paste("Max :", current_analysis[9])
      })
      
      output$text4_10 <- renderText({
        paste("Min :", current_analysis[10])
      })
      
      output$text4_11 <- renderText({
        paste("Max :", current_analysis[11])
      })
    })
    
    output$diagram5 <- renderPlot({
      current_analysis = analysis5(data, input$season_5, input$origin_5)
      current_analysis[1]
      
      output$diagram5_1 <- renderPlot({
        current_analysis[2]
      })
      
      output$text5_3 <- renderText({
        paste("Mean :", current_analysis[3])
      })
      
      output$text5_4 <- renderText({
        paste("Median :", current_analysis[4])
      })
      
      output$text5_5 <- renderText({
        paste("Standard Deviation :", current_analysis[5])
      })
      
      output$text5_6 <- renderText({
        paste("Min :", current_analysis[6])
      })
      
      output$text5_7 <- renderText({
        paste("Max :", current_analysis[7])
      })
      
      output$text5_8 <- renderText({
        paste("Min :", current_analysis[8])
      })
      
      output$text5_9 <- renderText({
        paste("Max :", current_analysis[9])
      })
      
      output$text5_10 <- renderText({
        paste("Min :", current_analysis[10])
      })
      
      output$text5_11 <- renderText({
        paste("Max :", current_analysis[11])
      })
    })
    
    output$diagram6 <- renderPlot({
      current_analysis = analysis6(data, input$season_6, input$origin_6)
      current_analysis[1]
      
      output$diagram6_1 <- renderPlot({
        current_analysis[2]
      })
      
      output$text6_3 <- renderText({
        paste("Mean :", current_analysis[3])
      })
      
      output$text6_4 <- renderText({
        paste("Median :", current_analysis[4])
      })
      
      output$text6_5 <- renderText({
        paste("Standard Deviation :", current_analysis[5])
      })
      
      output$text6_6 <- renderText({
        paste("Min :", current_analysis[6])
      })
      
      output$text6_7 <- renderText({
        paste("Max :", current_analysis[7])
      })
      
      output$text6_8 <- renderText({
        paste("Min :", current_analysis[8])
      })
      
      output$text6_9 <- renderText({
        paste("Max :", current_analysis[9])
      })
      
      output$text6_10 <- renderText({
        paste("Min :", current_analysis[10])
      })
      
      output$text6_11 <- renderText({
        paste("Max :", current_analysis[11])
      })
    })
    # 
    # output$diagram3 <- renderPlot({
    #   analysis3(data)
    # })
    # 
    # output$diagram4 <- renderPlot({
    #   analysis4(data,input$season)
    # })
    # 
    # output$diagram5 <- renderPlot({
    #   analysis5(data,input$datequartile, input$origin_5)
    # })
    # 
    # output$diagram6 <- renderPlot({
    #   analysis6(data)
    # })
  }

  shinyApp(ui = ui, server = server)
}

main()

