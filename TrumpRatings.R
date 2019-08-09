#Checks if a user has the required packages installed. If not, installs them.
packages = c("RCurl", "XML","plotly", "tidyr", "dplyr")
if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(packages, rownames(installed.packages())))  
}

library(RCurl)
library(XML)
library(plotly)
library(tidyr)
library(dplyr)
library(stringr)

#Scrape table off the website. Use 'Normal' poll name (i.e. not mobile) Convert classes to characters and numerics.
pollurl = getURL('https://www.realclearpolitics.com/epolls/other/president_trump_job_approval-6179.html')
pollurl = gsub("<a class=\"normal_pollster_name\".*?<a class=\"mobile_pollster_name\"", "<a class=\"mobile_pollster_name\"", pollurl)
classes = c("character", "character","character","numeric","numeric","character")
pollTable = readHTMLTable(pollurl, colClasses = classes, stringsAsFactors = FALSE)[[4]]

#drop column 3 - "Sample". Drop column 5 - "Spread". Drop row 1 "RCP Average".
pollTable = pollTable[-3]
pollTable = pollTable[-5]
pollTable = pollTable[-1,]

#Renumber the order of the rows correctly. 
rownames(pollTable) = NULL

#Split dates on " - ".
pollTable = separate(pollTable, Date, into = c("From","To"), sep = " - ", extra = "merge") # Separate into from and to dates

# Add years
pollTable$To = paste(c(rep(2018,nrow(pollTable)-287),rep(2017,287)), pollTable$To, sep="/")

# the row where we want to start listing 2019 from
twentyEighteen = length(pollTable$To) - 616
pollTable$To[1:twentyEighteen] <- gsub('2018', '2019', pollTable$To[1:twentyEighteen])

pollTable$To = as.Date(pollTable$To, "%Y/%m/%d") # Format the dates into y/m/d

#Set "Assumed Date" to the earliest possible date the data could have been added to the set.
for(i in (nrow(pollTable)-1):1){
  pollTable$To[i] = max(pollTable$To[i+1],pollTable$To[i])      
}

#Drop the 'from' column. 
pollTable = pollTable[-2]

#Rename columns in our dataframe.
colnames(pollTable) = c("Poll", "Assumed Date Received", "Approve", "Disapprove")

#We now want a new dataframe with "Dates", "average approve", "average disapprove", and "average spread",
#with the 11 most recent unique pollsters for each day.
len = nrow(pollTable)
Dates = pollTable$`Assumed Date Received`[len]:Sys.Date()

averageApprove = vector(mode = "numeric", length = length(Dates))
averageDisapprove = vector(mode = "numeric", length = length(Dates))

for(i in 1:length(Dates)) {
  df2 = pollTable[pollTable$`Assumed Date Received` <= Dates[i], ] 
  df2 = df2[!duplicated(df2$Poll), ]
  df2 = head(df2,11)
  averageApprove[i] = mean(df2$Approve)
  averageDisapprove[i] = mean(df2$Disapprove)
}

#Calc spread
Spread = (averageApprove - averageDisapprove)  

#final table
finalTable = data.frame(Dates = as.Date(Dates, origin = "1970/01/01"), averageApprove, averageDisapprove, Spread, stringsAsFactors = FALSE)  

#plot
ay <- list(
  tickfont = list(color = "black"),
  side = "left",
  title = "" ,
  range = c(30,58),
  tickvals = seq(from=35,to=70,by=5)
)
ayy <- list(
  tickfont = list(color = "black"),
  overlaying = "y",
  side = "left",
  title = "Approval",
  scaleanchor = "y",
  scaleratio = 0.2,
  range = c(-20,120),
  tickvals = c(-20,-15,-10,-5,0)
)
ax <- list(
  rangeselector = list(
    buttons = list(
      list(count = 7,label = "7D",step = "day",stepmode = "backward"),
      list(count = 14,label = "14D",step = "day",stepmode = "backward"),
      list(count = 30,label = "30D",step = "day",stepmode = "backward"),
      list(count = 6,label = "6M",step = "month",stepmode = "backward"),
      list(label = "MAX",step = "all")))
  
)

plotCheck = plot_ly(data = finalTable) %>%
  add_lines(x = ~Dates, y = averageApprove, name = "Approve",line = list(color = '#F72D00', width = 1)) %>%
  add_lines(x = ~Dates, y = averageDisapprove, name = "Disapprove",line = list(color = '#0016FF', width = 1)) %>%
  add_lines(x = ~Dates, y = Spread, name = "Spread",yaxis = "y2",fillcolor='grey5', fill='tozeroy',
            line = list(color = 'grey', width = 4)) %>%  
  layout(title = "Trump Approval", yaxis = ay,yaxis2 = ayy, xaxis = ax,showlegend = F, hovermode = 'compare') %>% 
  config(displayModeBar = F, showLink = F)

plotCheck
