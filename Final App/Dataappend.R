
#install.packages("readxl")
setwd("C:\\Users\\achaina\\Desktop\\MS BAIM\\R\\audio features") 

library(readxl)
df1 <- read_excel("dinner_track.xlsx", sheet=1, col_names=T)
df2 <- read_excel("party_track.xlsx", sheet=1, col_names=T)
df3 <- read_excel("sleep_track.xlsx", sheet=1, col_names=T)
df4 <- read_excel("workout_track.xlsx", sheet=1, col_names=T)
df1$class <- "1" 
df1$className <- "Dinner"
df2$class <- "2" 
df2$className <- "Party"
df3$class <- "3" 
df3$className <- "Sleep"
df4$class <- "4" 
df4$className <- "Workout"
df <- rbind(df1, df2, df3, df4) 
head(df, 3)
#install.packages("xlsx")
library(xlsx)
write.xlsx(df, "c:\\Users\\achaina\\Desktop\\MS BAIM\\R\\audio features\\Attr_data.xlsx")

library(readxl)
df5 <- read_excel("dinner_audio.xlsx", sheet=1, col_names=T)
df6 <- read_excel("party_audio.xlsx", sheet=1, col_names=T)
df7 <- read_excel("sleep_audio.xlsx", sheet=1, col_names=T)
df8 <- read_excel("workout_audio.xlsx", sheet=1, col_names=T)
df5$class <- "1" 
df5$className <- "Dinner"
df6$class <- "2" 
df6$className <- "Party"
df7$class <- "3" 
df7$className <- "Sleep"
df8$class <- "4" 
df8$className <- "Workout"
dff <- rbind(df5, df6, df7, df8) 
head(df, 3)
#install.packages("xlsx")
library(xlsx)
write.xlsx(dff, "c:\\Users\\achaina\\Desktop\\MS BAIM\\R\\audio features\\Attr_audio.xlsx")
