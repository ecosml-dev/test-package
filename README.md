# March 3rd, 2020 
# Dam Removal Assignment 
# Abby Walther 

# upload the data file 
read.csv(file.choose())
getwd()
setwd("/Users/abby/Downloads")
dd <- read.csv("ARDamRemovalList_Figshare_Feb2020.csv")

# things to look at the data
head(dd)
nrow(dd)
str(dd)
names(dd)

# load package 
library(plyr)
library(ggplot2)
library(dplyr)

# 1.How have the numbers of dams removed in the U.S. changed over time? 
# Create an informative figure to answer this question. 
# Hint: first draw the type of plot you would like to create

View(dd)
# first step: figuring out how many dams are removed per year

dd.year <- ddply(dd, c("Year_Removed"), summarize, 
      count = length(Year_Removed))


sum(dd.year$count)
head(dd.year)
dd.year <- dd.year[-1,]
sum(dd.year$count)

# adjust how year is called in teh directory 
str(dd.year)
dd.year$Year_Removed <- as.character(dd.year$Year_Removed) # change the character first 
str(dd.year)
dd.year$Year_Removed <- as.numeric(dd.year$Year_Removed) # changing from character to number
str(dd.year)

# now we can make a plot 
ggplot(data = dd.year, aes(x = Year_Removed, y = count)) + 
  geom_bar(stat = "identity", fill="orange") + 
  theme_classic(base_size = 16) + 
  labs(x = "Year") + 
  labs(y = "Dams Removed") + 
  scale_x_continuous(breaks = c(seq(1900, 2020, by = 10))) + 
  ggtitle("Dams Removed Per Year") 

ggsave("timetreads.pdf", width = 5, height = 5, units = c("ln"))

# Save this plot (note it will save to your working directory)
ggsave("time.trend.pdf", width = 8, height = 5, units = c("in"))
getwd() # tells you where the working directory is

# Same plot but now we've used a line graph (which is better?)
time.trend.2 <- ggplot(data=dd.year, aes(x=Year_Removed, y=count)) +
  geom_line(stat="identity") + theme_classic(base_size = 16) +
  scale_x_continuous(breaks=c(seq(1900,2020, by = 10))) +
  labs(y="Dam Removals", x = "Year") 
time.trend.2



###################################################################

# Question 2 

# we need to now find how many dam removals were in each state 

dd.state <- ddply(dd, c("State"), summarize, 
                 count = length(State))
dd.state
head(dd.state)
nrow(dd.state)
str(dd.state)
names(dd.state)


# now let's try making a plot og the states 


dd.state <- ddply(dd, c("State"), summarize, 
                  count = length(State))
head(dd.state)
dd.state <- subset(dd.state, count > 10)

state.trends <- ggplot(data = dd.state, aes(x = reorder(State, -count), y = count)) + 
  geom_bar(stat = "identity", color = "green", fill = "darkgreen") + 
   theme_classic(base_size = 16) + 
   theme(axis.text.x =  element_text(angle = 45, hjust = 1)) + 
   labs(y = "Dam Removals", x= "State") + 
  ggtitle ("Dams Removed per State")
state.trends

ggsave("state.trends.pdf", width = 8, height = 5, units = c("in"))
ggtwd()





##########################################################################

# Questoin 3 
# 

str(dd)

dd$Year_Built <- as.character(dd$Year_Built) # change the character first 
dd$Year_Built <- as.numeric(dd$Year_Built) # changing from character to number

str(dd)

# calculate mean, min, max, median 
dd$Year_Built
mean(dd$Year_Built, na.rm = TRUE)
median(dd$Year_Built, na.rm = TRUE)
min(dd$Year_Built, na.rm = TRUE)
max(dd$Year_Built, na.rm = TRUE)

# generate the plot 
dam.ages <- ggplot(data = dd, aes(x = Year_Built)) + 
  geom_histogram(binwidth = 25, color = "darkblue", fill = "lightblue") + 
  geom_vline(xintercept = 1921) + # this is showing the value we got for our MEAN 
  labs(x = "Dam Age", y = "Frequency") + 
  theme_classic(base_size = 24) +
  ggtitle("Histogram of Dam Removal Across Time") + 
  scale_x_continuous(breaks = c(seq(1700, 2000, by = 50)))
dam.ages

ggsave("dam.ages.pdf", width = 8, height = 5, units = c("in"))


###############################################################

# Question 4, convert feet into METERS , we are tyring to plot the distrubution 
# of the dam heights in meteres using a histogram
# divide your feet by 3.28 to get meteres 

str(dd)

dd$Dam_Height_ft <- as.character(dd$Dam_Height_ft) # change the character first 
dd$Dam_Height_ft <- as.numeric(dd$Dam_Height_ft) # changing from character to number

dd.height <- ddply(dd, c("Dam_Height_ft"), summarize, 
                   count = length(Dam_Height_ft))
dd.height

# calculate mean, min, max, median , excluding the NA's
dd$Dam_Height_ft
mean(dd$Dam_Height_ft, na.rm = TRUE)
median(dd$Dam_Height_ft, na.rm = TRUE)
min(dd$Dam_Height_ft, na.rm = TRUE)
max(dd$Dam_Height_ft, na.rm = TRUE)


dam.heights <- ggplot(data = dd, aes(x = Dam_Height_ft)) + 
  geom_histogram(binwidth = 10, color = "darkblue", fill = "lightblue") + 
  geom_vline(xintercept = 14.33288) + # this is showing the value we got for our MEAN 
  labs(x = "Dam Height", y = "Frequency") + 
  theme_classic(base_size = 24) +
  ggtitle("Histogram of Dam Heights") + 
  scale_x_continuous()
dam.heights
100/30.48

dam.meters <- dd$Dam_Height_ft/3.28
dam.meters


dam.heights <- ggplot(data = dd, aes(x = dam.meters)) + 
  geom_histogram(binwidth = 1, color = "darkblue", fill = "lightblue") + 
  geom_vline(xintercept = 4.3697) + # this is showing the value we got for our MEAN 
  labs(x = "Dam Height - Meters", y = "Frequency") + 
  theme_classic(base_size = 24) +
  ggtitle("Histogram of Dam Heights") + 
  scale_x_continuous()
dam.heights

ggsave("dam.heights.pdf", width = 8, height = 5, units = c("in"))



#########################################################################################

# 5, top 10 original uses of the dam removed. 

str(dd)
dd.use <- as.character(dd$Original.Use) # change the character first 

dd %>%
  group_by(Original.Use) %>% 
  
  arrange(Original.Use)
filter(dd.use, na.rm == TRUE)  
 summarise(dd, Original.Use = mean(Original.Use))
 names(dd) <- c('Original.Use','Count')
names(dd) 


root_list <- as.vector(dd$Original.Use[unlist(lapply(dd$Original.Use,function(x){length(grep(x,dd$Original.Use))>1}))])
root_list


U <- dd %>% 
  select(Original.Use, na.rm = TRUE) %>% 
  unlist() %>% 
  unique()

# Summarize the dams by the "Original.Use" column
uses <- ddply(dd, c("Original.Use"), summarise,
              count = length(Original.Use))

# Sort by the largest number within each use category
uses <- uses[order(-uses$count),]
uses
 

dam.original <- ggplot(data = dd, aes(x = uses))
dam.original