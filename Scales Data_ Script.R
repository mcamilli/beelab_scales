### Script used to manipulate downloaded data 
### from single Broodminder V3 hive scales


# 1 - Loading Packages
install.packages("dplyr")
install.packages("lubridate")
install.packages("ggplot2")
install.packages("rstatix")

library(dplyr)
library(lubridate)
library(ggplot2)
library(rstatix)

# 2 - Loading your raw scales data ".csv"

scale01 <- X49_03_BA
scale02 <- X49_03_BF
scale03 <- X49_03_C2
scale04 <- X49_03_B4
scale05 <- X49_03_C4
scale06 <- X49_03_C5
scale07 <- X49_03_C7
scale08 <- X49_03_B9


# 3 - Merging scales data by period

data2022 <- rbind (scale01, scale03, scale04, scale06, scale07, scale08)

# 4-  Adding Hive and Site column 
## Each scale has the unique UUID. This step adds new two columns "Hive and Site"
## based on your UUID, you can choose which hive belongs to each site

data2022_hive_site <- data2022 %>%
  mutate(Hive = case_when(
    endsWith(UUID, "A") ~ "1",
    endsWith(UUID, "2") ~ "2",
    endsWith(UUID, "B4") ~ "1",
    endsWith(UUID, "C5") ~ "2",
    endsWith(UUID,"C7") ~ "3",
    endsWith(UUID, "B9") ~ "4",
  )) %>%
  mutate(Site = case_when(
    endsWith(UUID, "A") ~ "Petrofika",
    endsWith(UUID, "2") ~ "Petrofika",
    endsWith(UUID, "B4") ~ "Goodale",
    endsWith(UUID, "C5") ~ "Goodale",
    endsWith(UUID,"C7") ~ "Goodale",
    endsWith(UUID, "B9") ~ "Goodale",
  ))

# 5 - Processing data function
## This function changes the data format and allows us to manipulate 
## the data using dplyr library

scales_func <- function(data, ftrim, btrim) {
  data %>%
    mutate(Local_TimeStamp = as.POSIXct(Local_TimeStamp,format = "%m/%d/%Y %H:%M" )) %>%
    mutate(Local_TimeStamp = lubridate::ceiling_date(Local_TimeStamp, unit = "hour"), # round Time Stamp to nearest hour
                  Time = Local_TimeStamp, #change Local_TimeStamp to time
                  Site = factor(Site), # make site a factor
                  UUID = factor(UUID), #make UUID a factor
                  Weight = Weight *0.45) %>% #transforming Weight from lbs to kg%>% # make the UUID a factor
    select(Time, Site, UUID, Hive, Weight) %>% #Select only the desired data
    filter(Time > ftrim & Time < btrim) %>% # trim data to desired start and end dates
    group_by(UUID) %>% 
    arrange(Time) # arrange chronologically
}
  
# 6 - Start processing the data
  
  data2022_scales_proc<- scales_func(data2022_hive_site,   #your data
                                         ftrim = "2022-06-01", #start date
                                         btrim = "2022-09-15"  #end date
  )
  
  View(data2022_scales_proc)
  
# 7 - Filtering midnight hours
### Get only one weight value per day per hive
  
  data2022_midnight <- data2022_scales_proc %>%
    filter(hour(Time) == 00:00:00)   
  
  View(data2022_midnight)

# 8 - Within-day hive weight changes
##  data were detrended for each day by subtracting the weight at midnight
##  from each subsequent weight value at midnight
  
  data2022_diff <- data2022_midnight %>%
    group_by(UUID) %>%
    arrange(Time) %>%
    mutate(diff = Weight - lag(Weight, default = first(Weight)))
  
  View(data2022_diff)

# 9 - Plotting raw data (hourly) graph
  
  ggplot(data2022_scales_proc, aes(Time, Weight, color = Hive)) +
    geom_point() +
    xlab("Time") +
    ylab(" Hourly Hive Weight (Kg)")+
    facet_wrap(~Site)
  
  
# 10 - Plotting only at midnight weight (daily)
  
  ggplot(data2022_midnight, aes(Time, Weight, color = Hive)) +
    geom_point() +
    xlab("Time") +
    ylab("Daily Hive Weight (Kg)")+
    facet_wrap(~Site)
  
  
# 11 -  Plot Within-day hive weight changes (diff)
  
  ggplot(data2022_diff, aes(Time, diff, color = Hive)) +
    geom_point() +
    xlab("Time") +
    ylab("Within-day Hive Weight Change (Kg)")+
    facet_wrap(~Site)  
  

#### In this pilot study we compared the within-day hive weight changes
#### at two different periods  of time (durting and after canola bloom)
####  for each experimental site
  
# 1 - Trimming data for each season
  
  ### Trimming During Blooming
  
  duringBloom <- scales_func(data2022_hive_site, ftrim = "2022-07-22", btrim = "2022-07-27")
  
  
  ### Trimming After Blooming
  
  afterBloom <- scales_func(data2022_hive_site, ftrim = "2022-08-19", btrim = "2022-08-25")
  
# 2 - ### Filtering midnight hours and adding diff weight (within-day changes)
  
  duringBloom_diff <- duringBloom %>%
    filter(hour(Time) == 00:00:00) %>%
    group_by(UUID) %>%
    arrange(Time) %>%
    mutate(diff = Weight - lag(Weight, default = first(Weight)))
  
  afterBloom_diff <- afterBloom %>%
    filter(hour(Time) == 00:00:00) %>%
    group_by(UUID) %>%
    arrange(Time) %>%
    mutate(diff = Weight - lag(Weight, default = first(Weight)))
  
# 3 - Plotting graphs
  
 ##  Plot Within-day hive weight changes (diff) for each period
  # During Bloom
  ggplot(duringBloom_diff, aes(Time, diff, color = Hive)) +
    geom_point( size = 3) +
    xlab("Time") +
    ylab("Within-day Hive Weight Change (Kg)")+
    facet_wrap(~Site)
  
  #After Bloom
  ggplot(afterBloom_diff, aes(Time, diff *0.45, color = Hive)) +
    geom_point(size = 3) +
    xlab("Time") +
    ylab("Within-day Hive Weight Change (Kg)")+
    facet_wrap(~Site)
  
  
# 4 - Statistics tests
  ## Check normality (Shapiro Wilk)
  
  shapiro.test(duringBloom_diff$diff)
  shapiro.test(afterBloom_diff$diff)
  
  ## Summarize the data by count (n), mean, sd, median, IQR and 
  ## adding Pvalue manually after T test
  ### In this pilot study, we used a simple T - test 
  ### for more than 2 sites, you can use ANOVA/Kruskal test
  
  summaryDB <- group_by(duringBloom_diff, Site) %>%
    summarise(
      count = n(),
      mean = mean(diff *0.45, na.rm = TRUE),
      sd = sd(diff*0.45, na.rm = TRUE),
      median = median(diff*0.45, na.rm = TRUE),
      IQR = IQR(diff*0.45, na.rm = TRUE)
    )%>%
    mutate (pvalue = 0.024)
  
  summaryAB <- group_by(afterBloom_diff, Site) %>%
    summarise(
      count = n(),
      mean = mean(diff*0.45, na.rm = TRUE),
      sd = sd(diff*0.45, na.rm = TRUE),
      median = median(diff*0.45, na.rm = TRUE),
      IQR = IQR(diff*0.45, na.rm = TRUE)
    )  %>%
    mutate (pvalue = 0.001)
  

  t.test(diff ~ Site, data = afterBloom_diff)
  
  t.test(diff ~ Site, data = duringBloom_diff)
  

  
  
  
  
  
