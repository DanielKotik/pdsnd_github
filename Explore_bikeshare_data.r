
library(ggplot2)
library(tibble)

ny = read.csv('new_york_city.csv')
wash = read.csv('washington.csv')
chi = read.csv('chicago.csv')

head(ny)

head(wash)

head(chi)

by(chi$Trip.Duration/60, chi$Gender, summary)

by(ny$Trip.Duration/60, ny$Gender, summary)

dim(subset(chi, chi$User.Type == "Customer" & chi$Gender != ""))

dim(subset(ny, ny$User.Type == "Customer" & ny$Gender != ""))

ggplot(aes(x=Gender, y=Trip.Duration/60), data=subset(chi, chi$User.Type == "Subscriber" &
                                                      chi$Gender != "")) + 
    geom_boxplot() + 
    coord_cartesian(ylim = c(0, 40)) + ggtitle("Chicago") +
    ylab('Average travel time in minutes')

ggplot(aes(x=Gender, y=Trip.Duration/60), data=subset(ny, ny$User.Type == "Subscriber" &
                                                      ny$Gender != "")) + 
    geom_boxplot() + 
    coord_cartesian(ylim = c(0, 40)) + ggtitle("New York city") +
    ylab('Average travel time in minutes')

sum(chi$User.Type == "Customer")

df = data.frame(User.Type=rep(c("Subscriber", "Customer"), 3),
           City=rep(c("Chicago", "New York City", "Washington"), each=2),
           Count=c(sum(chi$User.Type == "Subscriber"), sum(chi$User.Type == "Customer"),
                   sum(ny$User.Type == "Subscriber"), sum(ny$User.Type == "Customer"),
                   sum(wash$User.Type == "Subscriber"), sum(wash$User.Type == "Customer")
                  )
          )
df

ggplot(data=df, aes(x=City, y=Count, fill=User.Type)) +
ggtitle("Number of rents among differetn cities in 2017") +
geom_bar(stat="identity", position=position_dodge())

from.timestamp = function(timestamp, component)
    {
    date = as.POSIXlt(timestamp)

    if (component == "hour")
        {
        return(date$hour)
    } else if (component == "mon")
        {
        return(date$mon)
    } else if (component == "wday")
        {
        return(date$wday)
    }
}

ny = add_column(ny, "Start.Week.Day"=from.timestamp(ny$Start.Time, "wday"), .after="Start.Time")

head(ny)

summary(ny$Start.Week.Day)

ggplot(aes(x=Start.Week.Day), data=ny) +
  geom_histogram() + 
  ggtitle('Histogram of Number of Bike Rents Per Weekday in NYC') +
  labs(y="Number of Bike Rents") +
  scale_x_continuous("Weekday", limits=c(-1,7), breaks = c(0, 1, 2, 3, 4, 5, 6),
                     label = c("Mon", "Tue", "Wend", "Thu", "Fri", "Sat", "Sun"))

system('python -m nbconvert Explore_bikeshare_data.ipynb')


