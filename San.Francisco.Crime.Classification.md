
# San Francisco Crime Classification

#### Let load all required package and read train CSV


```R
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

```

#### As dataset is large lets make a subset


```R
##let get sample of 
train = sample_n(train_2, 10000)
train = train[which(train$X!="-122.4034" & train$Y!="37.77542"), ]
```


```R
str(train)
```

    Classes 'tbl_df' and 'data.frame':	10000 obs. of  9 variables:
     $ Dates     : POSIXct, format: "2012-06-26 19:00:00" "2004-11-14 13:00:00" ...
     $ Category  : chr  "ASSAULT" "BURGLARY" "WARRANTS" "SUSPICIOUS OCC" ...
     $ Descript  : chr  "BATTERY WITH SERIOUS INJURIES" "BURGLARY OF APARTMENT HOUSE, UNLAWFUL ENTRY" "WARRANT ARREST" "INVESTIGATIVE DETENTION" ...
     $ DayOfWeek : chr  "Tuesday" "Sunday" "Friday" "Friday" ...
     $ PdDistrict: chr  "SOUTHERN" "NORTHERN" "BAYVIEW" "MISSION" ...
     $ Resolution: chr  "ARREST, BOOKED" "NONE" "ARREST, BOOKED" "NONE" ...
     $ Address   : chr  "200 Block of 8TH ST" "400 Block of STEINER ST" "0 Block of NAVY RD" "2700 Block of 24TH ST" ...
     $ X         : num  -122 -122 -122 -122 -122 ...
     $ Y         : num  37.8 37.8 37.7 37.8 37.8 ...
    

#### Lets add years, month, hour and week


```R
##let data coloum
train$Dates = ymd_hms(train$Dates)
train$years = year(train$Dates)
train$month = month(train$Dates)
train$hour = hour(train$Dates)
train$week = week(train$Dates)
```

#### Data has many category so lets only take top 10 cat for analysis


```R
#lets only get top 10 category
temp = train%>% group_by( Category) %>%summarise(no_sum=length(Category))
temp = temp[order(temp$no_sum, decreasing=T ), ][1:10,1]
train = left_join(temp, train)
train = train[which(train$years!='2015'),]
```

    Joining by: "Category"
    

### Let plot of top 10 Category


```R
options(repr.plot.width=9, repr.plot.height=3)
temp = train %>% group_by( Category) %>%summarise(no_sum=length(Category))
(ggplot(temp, aes(x=reorder(Category,no_sum), y=no_sum, fill ="blue")) 
            + geom_bar(stat="identity")
            + coord_flip( )
            + ylab('Category')
            + xlab('')
            + guides(fill=FALSE)
            + ggtitle('No of Case in Top 10 Category'))     
```



##### Lets see how crime category has chages every year


```R
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
```



#### Lets plot No of Case by weekday


```R
train$dummy = 1
temp = train %>% group_by(DayOfWeek) %>% summarise(no=sum(dummy))
(ggplot(temp, aes(DayOfWeek, y=no), group=1) + geom_line(aes(colour="", group=1,)) +geom_point()
            + ylab('')
            + xlab('')
            + ggtitle('No of Case by weekday')
            + theme(legend.position="none")
            + expand_limits(x = "Friday",y = 0))
```



#### No of case by hour per day-- to see which 


```R
temp = train %>% group_by(hour) %>% summarise(no=sum(dummy))
(ggplot(temp, aes(factor(hour), y=no), group=1) + geom_line(aes(colour="", group=1))+geom_point()
            + ylab('Category')
            + xlab('No')
            + ggtitle('No of Case by Hour')
            + theme(legend.position="none")
            + expand_limits( y = 0))
```



#### Lets make chart by hour per day and week day


```R
options(repr.plot.width=9, repr.plot.height=7)
temp = train %>% group_by(DayOfWeek, hour) %>% summarise(no=sum(dummy))
(ggplot(temp, aes(factor(hour), y=no), group=1) + geom_line(aes(colour="", group=1))+geom_point()
            + ylab('')
            + xlab('')
            + theme(legend.position="none")
            + expand_limits( y = 0)
            + facet_wrap(~DayOfWeek, nrow=7))
```



#### Let plot log and lat on SF map


```R
# Download the base map
map <- get_map(location = "san francisco", zoom = 13, maptype = "roadmap")
```

    Map from URL : http://maps.googleapis.com/maps/api/staticmap?center=san+francisco&zoom=13&size=640x640&scale=2&maptype=roadmap&language=en-EN&sensor=false
    Information from URL : http://maps.googleapis.com/maps/api/geocode/json?address=san%20francisco&sensor=false
    

###### >> let make point chart 


```R
options(repr.plot.width=9, repr.plot.height=24)
temp = train %>% group_by(X,Y, Category) %>% summarise(no=sum(dummy))
(ggmap(map, extent = "device") 
    + geom_point(data = temp, aes(x = X, y = Y, size = no, colour = factor(Category),alpha=0.8) )
    + theme(legend.position="none")
    + facet_wrap(~ Category, ncol=2)
    )
```

    Warning message:
    : Removed 1301 rows containing missing values (geom_point).



##### >> Let make heat map


```R
options(repr.plot.width=9, repr.plot.height=24)
ggmap(map, extent = "device") +geom_point(data = train, aes(x = X, y = Y), size = 0.1, alpha=0.1)+
  geom_density2d(data = train, aes(x = X, y = Y), size = 0.3) + 
  stat_density2d(data = train, 
                 aes(x = X, y = Y, fill = ..level.., alpha = ..level.. ), size =0.1, 
                 bins = 16, geom = "polygon") + scale_fill_gradient(low = "green", high = "red") + 
  scale_alpha(range = c(0, 0.3), guide = FALSE)+  facet_wrap(~ Category, ncol=2)
```

    Warning message:
    : Removed 157431 rows containing non-finite values (stat_density2d).
