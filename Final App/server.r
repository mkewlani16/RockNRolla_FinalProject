library(shiny)
library(shinysky)
library(plotly)
library(ggplot2)
library(sqldf)
library(devtools)
library(hexbin)
library(data.table)
library(fclust)

#install.packages("shinydashboard")
#install.packages("shinythemes")
library(shinydashboard)
library(shinythemes)

#devtools::install_github("ggrothendieck/sqldf")

#distance function
Edistance <- function(x1, x2, ... ){
  a= dist(rbind(x1, x2))
  return(a)
}

options(shiny.sanitize.errors = FALSE)

#Fuzzy Cluster function  
probCluster <- function(song_attrb,newDatapoint, NoCluster){
  
  #removing duplicate data
  #duplicated <- song_attrb[duplicated(song_attrb),]
  unique_song_attrb <- unique(song_attrb)
  
  #remove rows with NA value
  unique_song_attrb <- unique_song_attrb[complete.cases(unique_song_attrb), ]
  
  
  #convert attribute values to to z-score 
  #temp=as.data.frame(scale(unique_song_attrb[,1:13]))
  temp=as.data.frame(scale(unique_song_attrb[,3:7]))
  
  #setting time signature NA to 1
  #temp[is.na(temp)] <- 1
  
  #Calculating eucledian distance of New point from the dataset
  a=apply(temp,1,Edistance,x2=newDatapoint)
  
  #find index of minimum distance
  minIndex = which(a == min(a))
  
  
  #Apply Fuzzy Cluster on standardized data. Get Probability of fuzzy cluster in 'songs_fcult$U'
  songs_fcult <- FKM(unique_song_attrb[,3:7],NoCluster, maxit = 1e4)
  
  #return cluster probability that matched with minimum dataset
  return (songs_fcult$U[minIndex,] )
}

#reading in the required files
song_usr_data <- fread("User_scores.csv", select= c("song_title","artist","acousticness",
                                                    "danceability","speechiness","energy","liveness", "user_id","className",
                                                    "pred_score2_final","Song_played","Num_played","p_len_played","days_played"))

song_region_ranking <- fread("DaysOnChart.csv", select= c("Track","Artist","Country",
                                                          "days_on_chart"))


song_streams_plot <- fread("song_ranking_merged3.csv", select = c("Country", "Bucket1", "Bucket2", "Avg_Streams"))



#Manager

#Give 4 scores based on the sliders and dropdown





# k-means Clustering

song_data <- sqldf("select distinct song_title,artist,acousticness,danceability,speechiness,energy,liveness from song_usr_data")

song_data["song_id"]=c(1:dim(song_data)[1])

song_data <- song_data[complete.cases(song_data), ]

songs_cult <- kmeans(song_data[,3:6],5) #Cluster numbers were 5 orignally

song_data$cluster=songs_cult$cluster

#?
test <- sqldf("select s.song_id,s.song_title,s.artist,s.cluster,su.user_id,su.Song_played,su.Num_played,su.p_len_played,su.days_played 
             from song_data s,song_usr_data su 
             where s.song_title=su.song_title and s.artist=su.artist")

#?	 
clust_usr_agg <- sqldf("select cluster,sum(Song_played) ,avg(Num_played) as 'PLAYS per USER' ,avg(p_len_played) as '% LENGTH PLAYED' ,avg(days_played) as 'DAYS IN PLAYLIST' 
		from test 
		group by cluster")

test2 <- sqldf("select s.song_title,s.artist,su.Country,su.days_on_chart,s.cluster
             from song_data s,song_region_ranking su 
               where s.song_title=su.Track and s.artist=su.Artist")

test3 <- sqldf("select Country, cluster, avg(days_on_chart) AS 'DAYS IN TOP 200'
               from test2 
               group by Country, cluster" )


##### SERVER #####

# Define server logic for random distribution application
shinyServer(function(input, output,session) {
  
  

  output$prob <- renderText(input$Region)
  
  
  ##OBSERVE EVENTS ON ACTION BUTTON

  
  #Observe event on User Button - producing the datatable
  observeEvent(input$user_Button,
     {
       

       
       
       df1<- sqldf(sprintf("select song_title as TITLE,artist as ARTIST,pred_score2_final as 'PREDICTION SCORE'
                   from song_usr_data 
                           where (user_id = '%s' and className ='%s' and Song_played ='%s')
                           order by pred_score2_final desc limit 50",input$UserName,input$className,input$Song_played))
       output$Top5 <- renderTable(df1)
     })
  
  
  #Observe event on Producer Button - Plotting the bar chart
  observeEvent(input$producer_Button,
     {
       
       p=probCluster(song_data,as.numeric(c(input$acousticness,input$danceability, input$speechiness, input$energy, input$liveness)),5)      #p=probCluster(song_data,runif(5, min=0, max=1),3)
       tmp_cntry = input$Region #if(is.null(region)){"Global"} else {region}  #minfoFilter()$region       #"Argentina"    #input$Region
       estimation <- p %*% as.matrix(  cbind(100*clust_usr_agg[,3:4],250*clust_usr_agg[,5],  dplyr::filter(test3, Country== tmp_cntry)["DAYS IN TOP 200"] )  )
       colnames(estimation) <- c("PLAYS per USER", "% LENGTH PLAYED","DAYS IN PLAYLIST","DAYS IN TOP 200")
       output$estimated_length <- renderTable(estimation)
       
       ##bucket1 of estimated avg days on chart for new input song will be calculated using the above code
       x <- cut(estimation[4], breaks=c(0,20, 40, 60, 80, 100, 120, 140, 160, 180, 200, 220, 225, 425), 
                labels=c("20","40","60", "80", "100","120", "140", 
                         "160", "180", "200", "220", "230", "420"))
       
       
       x <- as.numeric(as.character(x))
       
       
       #creating a data table to plot
       plot_sltv <- sqldf(sprintf("select distinct Bucket2,Avg_Streams from song_streams_plot where Country = '%s' 
                                  and Bucket1 = %s order by Bucket2",tmp_cntry,x))
       
       #ploting the bar graph and storing it in a variable
       outplot <- ggplot(data = plot_sltv, aes(x = Bucket2, y = Avg_Streams, fill= -(Avg_Streams))) +
         theme(axis.text=element_text(size=12),plot.title = element_text(hjust = 0.5)) + 
         geom_bar(stat = "identity", size = 0.01)+ labs(x= "Days", y="#Streams", title = "Track lifetime")
       
       output$plotPut <- renderPlot(outplot)
     })
  
  ##
  
})