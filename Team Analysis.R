#Some solutions for Lab4 2022
install.packages("tidyverse")

library(dplyr) 
library(ggplot2)

#remember to set the working directory - you can browse to it under the Files pane
#then you can click on the More dropdown and set as working dir or use setwd("pathtonameofwd")
team_raw <- read.csv(file="Team.csv",  header=TRUE,  sep=",",  stringsAsFactors  =  FALSE)
#Let's take a quick look:
names(team_raw)

sapply(team_raw,class)

head(team_raw,5)	

dim(team_raw)

#the columns that have duplicated values are the 2nd and 3rd columns - Name and Surname

team <-team_raw[!duplicated(team_raw[,2:3]),] 

#How many replicated instances were there? 
nrow(team_raw) - nrow(team)

#a. Unique players:
nrow(team)

#b. Female players:
nrow(team[team$Gender=="Female",])

#Think about how you would check if some of the values for Gender were spelled incorrectly?

#Checking for Missing Values
sapply(team,  function(x)  sum(is.na(x)))

#We can also look at the ratios of missing values:
sapply(team,  function(x)  100*sum(is.na(x)/length(x)))

#Given the ratios of missing value in Muscle Mass, deleting it would be the most appropriate choice
team = select(team, -Muscle.Mass....)
head(team,1)

#how about Goals.Against?
aggregate(team$Goals.Against, by=list(team$Position), FUN  =  function(x)  sum(is.na(x)/length(x)))

#consider an imputation for Goals.Against
team[is.na(team$Goals.Against),"Goals.Against"]  =  0

# Find those empty values for age and replace them with a suitable value.
m_age  =  mean(team$Age,  na.rm  =  TRUE) 
team[is.na(team$Age),"Age"] = m_age

# 4.	Some heights, weights and speeds are missing as well. Find those and replace them with a suitable value in each case
# create a new function
# impute_data() that takes a dataframe, a field that needs to be imputed and a field that we'll use as criteria:

impute_data = function(df, field,criteria){
  values=aggregate(df[,field],  by  =  list(df[,criteria]), FUN  =  function  (x)  median(x,na.rm  =  TRUE))
  colnames(values) = c("crit", "val")
  
  for (v in values$crit){
    print(paste(v ," ", values[values$crit==v, "val"])) 
    df[is.na(df[,field] & df[,criteria]==v),field] = values[values$crit==v, "val"]
  }
  return(df)
}


#try improving this function by:
#1) removing the for loop and 
#2) making the replacement operation (median in this case) an additional parameter.

team  =  impute_data(team,"Height..m.","Gender")

team  =  impute_data(team,  "Speed..m.h.",  "Gender")

team  =  impute_data(team,  "Weight..kg.",  "Gender")

sapply(team,  function(x)  100*sum(is.na(x)/length(x)))

# Set all missing values in the "Pre.Selected" column to U for unknown

team[team$Pre.selected=="","Pre.selected"]  =  "U"

#6.	Some players have no playing position. Choose a sensible strategy and deal with them

team  =  team[team$Position!="",]

#7. Sampling
install.packages("OneR")

library(OneR)

g = team$Avg.Goals

#Let's look at the range:
range(g)

sd(g)


#Let's create 10 bins for a variable with a range of 20 units
nbs = 10

g1  =  bin(g,  labels=1:nbs,  nbins  =  nbs)
g1
#Another option is to use clustering to find the intervals, for example:

g2  =  bin(g,  method="clusters")
g2
#An additional method would be to divide according to number of samples, so that each bin has the same number of samples:
g3  =  bin(g,  method="content")

#8.	Using the equation given in class, identify any outliers in the following attributes

boxplot(team$Height..m.,  main  =  "Boxplot  of  Height")
boxplot(team$Speed..m.h.,  main  =  "Boxplot  of  Speed")

rmv_outliers = function(df,  field){ 
  m = mean(df[,field])
  s = sd(df[,field]) 
  thrs = 3*s
  out = df[df[,field]<=(m+thrs) & df[,field]>=(m-thrs),]# returning updated dataset- without outliers
  print(out)
  return(out)
}

team = rmv_outliers(team,"Height..m.") 
team = rmv_outliers(team,"Speed..m.h.")


#Let's see if that was enough:
boxplot(team$Height..m.,  main  =  "Boxplot  of  Height")
boxplot(team$Speed..m.h.,  main  =  "Boxplot  of  Speed")


#We can see that there are still some outliers in the data. 
#To solve this, you may have to call iteratively to the rmv_outliers() function until no changes are found in the data

team = rmv_outliers(team,"Height..m.")
boxplot(team$Height..m., main = "Boxplot of Height")

#Include one more attribute in the database which normalizes the speed attribute between 0 and 1.
#manually
team$norm.speed = (team$Speed..m.h. - min(team$Speed..m.h.)) /(max(team$Speed..m.h.) - min(team$Speed..m.h.))

#10.	Include one more attribute in the database, Body Mass Index (BMI), to give you more information about the player's physical condition.

team$bmi  =  team$Weight..kg./team$Height..m.^2


#11.	Analyse your data by creating a summary table overall 

#Summary of all 
#Centrality 

summary(team)


#Dispersion
apply(team[,c("Age","Height..m.","Weight..kg.","Years.active",
              "Goals.Against",  "Avg.Goals","Speed..m.h.","Salary","norm.speed","bmi")],  2, sd)

apply(team[,c("Age","Height..m.","Weight..kg.","Years.active",
              "Goals.Against",  "Avg.Goals","Speed..m.h.","Salary","norm.speed","bmi")],2,IQR)


#A  non-elegant  solution:
by_team  =  aggregate(team[,c("Age","Height..m.","Speed..m.h.")],  by  =  list(team$Team),  summary)

#One alternative is to create auxiliary functions to analyse each attribute

df_sum = function(df, criteria){
  #Numerical attributes:
  num = select_if(df, is.numeric)
  num_res  =  apply(num,  2,FUN  =  analyse_numeral,criteria)
  #Nominal attributes:
  #For nominal attributes, let's calculate the mode and, as an extra #a table of frequencies per team:
  nom   =  df[,setdiff(colnames(df),  colnames(num))]
  tabs  =  apply(nom,  2,  FUN  =  analyse_nominal,  criteria)
  return(list(num_res, tabs))
}

analyse_nominal = function(x, criteria){ 
  tab = table(criteria,x)
  u = unique(x)
  mode = u[which.max(tabulate(match(x, u)))]
  return(list(frequency = tab,mode= mode))
}

analyse_numeral = function(x, criteria){
  cent = aggregate(x, by=list(criteria), summary) 
  disp = aggregate(x, by=list(criteria), sd)
  iqr =  aggregate(x,  by=list(criteria),  IQR)
  return(list(centrality =  cent,sd  =  disp,  iqr  =  iqr))
}

#Prepare data
t = select(team, -c(Player_ID, Name, Surname, Team))
head(t,1)

criteria = team$Team 
#call function 
df_sum(t, criteria)

#Printing on screen might not be useful if we were to use this data further down the line. 
#To help with that, you could save the analysis tables in csv files, for example. 


#a. what is the team with the most players?
tot_team  =	team	%>% group_by(Team) %>% summarise(total=length(Team)) 

head(tot_team[order(-tot_team$total),],1)


#b. what is the overall mean salary?
mean(team$Salary)


#c. what is the overal median speed?
median(team$Speed..m.h.)


# Exploratory Data Analysis
#How many different teams with more men than women does Narnia have? 
#There are many ways of doing this. Let's see a solution with tables using xtabs().

more_w = function(team){
  ags = aggregate(team$Name, by=list(team$Team, team$Gender), length) 
  tab = xtabs(x~Group.1+Group.2,ags)
  return(sum(tab[,1]>tab[,2]))
}

more_w(team)

#What is the mean age and salary of male players in Dragon Island
apply(team[team$Gender=="Male"  &  team$Team=="Dragon Island",  c("Age","Salary")],  2,  mean)

#What is the median height of female forward players in Bism?
median(team[team$Gender=="Female"  &  team$Team=="Bism" & team$Position=="Forward","Height..m."])

#What is the home team of the fastest player#


#Full entry:
head(team[order(team$Speed..m.h.,decreasing  =  TRUE),],1)


#Just Team
head(team[order(team$Speed..m.h.,decreasing  =  TRUE),"Team"],1)


#Show a histogram of the frequency of the positions from the fastest 40 players

library(ggplot2)
fast  =  team[order(team$Speed..m.h.,decreasing  =  TRUE)[1:40],]
ggplot(fast)  +  geom_histogram(bins  =  30,  aes(x=fast$Speed..m.h.,  fill=Team))+ 
  xlab("Speed (m/h)") + ylab("Frequency") +
  ggtitle("Histograms from the fastest players by Team")


#What is the gender of the goalkeeper with fewer goals against them

goalkeeps = team[team$Position == "Goalkeeper",]
head(goalkeeps[order(goalkeeps$Goals.Against),"Gender"],1)

#b.	Generate a pie chart with the percentages of selected players from each region. 
#Which region has more pre-selected players? 
#This is a bit of a roundabout way of doing the piechart. There are more efficient ways to do this.


library(ggplot2)
#Format data
yes = team[team$Pre.selected=="Y",] 
players = nrow(yes)

res  =  aggregate(yes$Player_ID,  by  =list(yes$Team),length)

res$pc = round(res$x/players,3) 
colnames(res)  =  c("Team","Total","PC")

#Piechart with ggplot2
ggplot(res,  aes(x="",  y=  PC,  fill=Team))  + 
  geom_bar(width  =  1,  stat  =  "identity")  + 
  coord_polar("y") + theme_void() + 
  geom_text(aes(label = PC),
            position = position_stack(vjust = 0.5))

#What is the team which spends most on salaries? And the one which spends less
sals = aggregate(team$Salary, by = list(team$Team), sum)
colnames(sals)  =  c("Team",  "Total")
head(sals[order(-sals$Total),],1)


#Which is the team with most forwards? 

fwds =  team[team$Position=="Forward",]
ag_fwds =  aggregate(team$Name, by  = list(team$Team),  length)
colnames(ag_fwds)  =  c("Team","Forwards") 
head(ag_fwds[order(-ag_fwds$Forwards),],1)

#And the team with less defenders?
def = team[team$Position=="Defender",]
ag_def = aggregate(team$Name, by = list(team$Team), length) 
colnames(ag_def)  =  c("Team","Defender") 
head(ag_def[order(ag_def$Defender),],1)


#Which team has the biggest difference in salaries 
#(i.e. the difference between the most paid player and the least paid player is the largest).

sals = aggregate(team$Salary, by = list(team$Team), FUN = function(x) max(x)-min(x)) 
colnames(sals)  =  c("Team","Dif.Sals")
head(sals[order(-sals$Dif.Sals),],1)


#How many players were initially not selected?

sum(team$Pre.selected=="N")

#Which team has the best forwards (i.e. their average scoring is the highest)?

fwds =  team[team$Position=="Forward",]
ag_fwds = aggregate(fwds$Avg.Goals, by=list(fwds$Team), mean)
colnames(ag_fwds)  =  c("Team","Avg.Goals") 
head(ag_fwds[order(-ag_fwds$Avg.Goals),],1)