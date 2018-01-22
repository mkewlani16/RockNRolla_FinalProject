

setwd("")
getwd()

#distance function
Edistance <- function(x1, x2, ... ){
    a= dist(rbind(x1, x2))
    return(a)
  }

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
  
  
  
library(readxl)
library(fclust)
library(sqldf) 
library(data.table)
library(ggplot2) 
 
 
 #reading in the files
 
 song_usr_data <- fread("User_scores.csv", select= c("song_title","artist","acousticness",
                                                    "danceability","speechiness","energy","liveness", "user_id","className",
                                                    "pred_score2_final","Song_played","Num_played","p_len_played","days_played"))

 song_region_ranking <- fread("DaysOnChart.csv", select= c("Track","Artist","Country",
                                                          "days_on_chart"))


song_streams_plot <- fread("song_ranking_merged3.csv", select = c("Country", "Bucket1", "Bucket2", "Avg_Streams"))														  
														  
 
 #User
  
	#Get Top 5( Predicted Score) reeactive on the Filters selected
	
	#?paramterize user_id and className
	sqldf("select song_title,artist,pred_score2_final
      from song_usr_data 
      where (user_id = 'U1' and className ='Sleep')
      order by pred_score2_final desc limit 5")
	
	
	#Lookup Predicted Score for searching songs
	
	

	
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
		clust_usr_agg <- sqldf("select cluster,sum(Song_played),avg(Num_played),avg(p_len_played),avg(days_played) 
		from test 
		group by cluster")

		#? Fuzzy Clustering change for user INput
		p=probCluster(song_data,runif(5, min=0, max=1),5)

		#?
		test2 <- sqldf("select s.song_title,s.artist,su.Country,su.days_on_chart,s.cluster
             from song_data s,song_region_ranking su 
              where s.song_title=su.Track and s.artist=su.Artist")

		test3 <- sqldf("select Country, cluster, avg(days_on_chart) 
               from test2 
               group by Country, cluster" )

		#Change Argentina to UI selected variable
		p %*% as.matrix(  cbind(clust_usr_agg[,3:5],  dplyr::filter(test3, Country=='Argentina')["avg(days_on_chart)"] )  )

		
		#Generation of Bar Plot
		
		##bucket1 of estimated avg days on chart for new input song will be calculated using the above code
		x <- cut(119, breaks=c(0,20, 40, 60, 80, 100, 120, 140, 160, 180, 200, 220, 225, 425), 
         labels=c("20","40","60", "80", "100","120", "140", 
                  "160", "180", "200", "220", "230", "420"))
		
		
		x <- as.numeric(as.character(x))
		cntry <- 'Argentina'

		#?
		plot_sltv <- sqldf(sprintf("select distinct Bucket2,Avg_Streams from song_streams_plot where Country = '%s' 
                   and Bucket1 = %s order by Bucket2",cntry,x))

		ggplot(data = plot_sltv, aes(x = Bucket2, y = Avg_Streams, fill = Bucket2)) + geom_bar(stat = "identity", size = 0.05)   
  