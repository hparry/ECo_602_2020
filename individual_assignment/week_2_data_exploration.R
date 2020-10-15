#week_2 data_exploration
#download data files
dat_birds<-read.csv("https://michaelfrancenelson.github.io/eco_602_634_2020/data/bird.sta.csv")
dat_habitat<-read.csv("https://michaelfrancenelson.github.io/eco_602_634_2020/data/hab.sta.csv")
#make pairs plots
pairs(dat_habitat[,c("ba.hard", "ba.snag", "ba.tot")])
pairs(dat_habitat[,c("snag.l", "elev","snag.sml")])
pairs(dat_habitat[,c ("elev","slope", "snag.l")])

#create a histogram of birds data
#see first 6 rows of data for each column
head(dat_birds)
hist(dat_birds$AMRO)
#labeled the axis and changed the breaks in data due to counts of birds being integers
#0:7 is the range of the data for number of birds counted.
hist(dat_birds$WIWA,xlab = "Number of birds counted", breaks = 0:7 - 0.5)

