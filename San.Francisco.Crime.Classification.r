
#################load required package

suppressPackageStartupMessages(require("dplyr"))
suppressPackageStartupMessages(require("readr"))
suppressPackageStartupMessages(require("ggmap"))
suppressPackageStartupMessages(require("lubridate"))
currentDate = Sys.Date()

#########Set the file dir

setwd("E:/R_Script")
filepath=getwd()
setwd(paste(filepath, "Input", sep="/"))


##read file
train_2 = read_csv("train.csv")


##let get sample of 
train = sample_n(train_2, 5000)
train = train[which(train$X!="-122.4034" & train$Y!="37.77542"), ]

str(train)

##let data coloum
train$Dates = ymd_hms(train$Dates)
train$years = year(train$Dates)
train$month = month(train$Dates)
train$hour = hour(train$Dates)
train$week = week(train$Dates)

#lets only get top 10 category
temp = train%>% group_by( Category) %>%summarise(no_sum=length(Category))
temp = temp[order(temp$no_sum, decreasing=T ), ][1:10,1]
train = left_join(temp, train)
train = train[which(train$years!='2015'),]

options(repr.plot.width=9, repr.plot.height=3)
temp = train %>% group_by( Category) %>%summarise(no_sum=length(Category))
(ggplot(temp, aes(x=reorder(Category,no_sum), y=no_sum, fill ="blue")) 
            + geom_bar(stat="identity")
            + coord_flip( )
            + ylab('Category')
            + xlab('')
            + guides(fill=FALSE)
            + ggtitle('No of Case in Top 10 Category'))     

options(repr.plot.width=9, repr.plot.height=3)
train$dummy = 1
temp = train %>% group_by(years, Category) %>% summarise(no=sum(dummy))
(ggplot(temp, aes(x=factor(years), y=no,  fill=Category, order=desc(no), colour = Category, group=Category )) 
            + geom_area(colour=NA, alpha=.7) 
            + scale_fill_brewer(palette="Paired")
            + geom_line(position="stack", size=.2)
            + ylab('')
            + xlab('Years')
            + ggtitle('No of Case by year'))

train$dummy = 1
temp = train %>% group_by(DayOfWeek) %>% summarise(no=sum(dummy))
(ggplot(temp, aes(DayOfWeek, y=no), group=1) + geom_line(aes(colour="", group=1,)) +geom_point()
            + ylab('')
            + xlab('')
            + ggtitle('No of Case by weekday')
            + theme(legend.position="none")
            + expand_limits(x = "Friday",y = 0))

temp = train %>% group_by(hour) %>% summarise(no=sum(dummy))
(ggplot(temp, aes(factor(hour), y=no), group=1) + geom_line(aes(colour="", group=1))+geom_point()
            + ylab('Category')
            + xlab('No')
            + ggtitle('No of Case by Hour')
            + theme(legend.position="none")
            + expand_limits( y = 0))

options(repr.plot.width=9, repr.plot.height=7)
temp = train %>% group_by(DayOfWeek, hour) %>% summarise(no=sum(dummy))
(ggplot(temp, aes(factor(hour), y=no), group=1) + geom_line(aes(colour="", group=1))+geom_point()
            + ylab('')
            + xlab('')
            + theme(legend.position="none")
            + expand_limits( y = 0)
            + facet_wrap(~DayOfWeek, nrow=7))

# Download the base map
map <- get_map(location = "san francisco", zoom = 13, maptype = "roadmap")

options(repr.plot.width=9, repr.plot.height=24)
temp = train %>% group_by(X,Y, Category) %>% summarise(no=sum(dummy))
(ggmap(map, extent = "device") 
    + geom_point(data = temp, aes(x = X, y = Y, size = no, colour = factor(Category),alpha=0.8) )
    + theme(legend.position="none")
    + facet_wrap(~ Category, ncol=2)
    )

options(repr.plot.width=9, repr.plot.height=24)
ggmap(map, extent = "device") +geom_point(data = train, aes(x = X, y = Y), size = 0.1, alpha=0.1)+
  geom_density2d(data = train, aes(x = X, y = Y), size = 0.3) + 
  stat_density2d(data = train, 
                 aes(x = X, y = Y, fill = ..level.., alpha = ..level.. ), size =0.1, 
                 bins = 16, geom = "polygon") + scale_fill_gradient(low = "green", high = "red") + 
  scale_alpha(range = c(0, 0.3), guide = FALSE)+  facet_wrap(~ Category, ncol=2)
