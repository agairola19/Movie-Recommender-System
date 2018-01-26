#CS422 Data Mining Homework - 4 III
#Akash Gairola

#Recommender Systems

setwd("D:/MS/Sem 1/CS422 Data Mining/Homework - 4/ml-100k/ml-100k")

userRating <- read.csv(file = "u.data",sep = "",col.names=c("user id","movie id","rating","timestamp"), 
                                                             header= F)

movies <- read.csv(file = "u.item",sep = "|",col.names=c( "movie id"," movie title", "release date", "video release date",
                                                             "IMDb URL","unknown","Action","Adventure","Animation","Children's","Comedy","Crime","Documentary","Drama","Fantasy", 
                                                             "Film-Noir","Horror","Musical","Mystery","Romance","Sci-Fi","Thriller","War","Western"), header= F)

#(i)
user200 <- subset(userRating,userRating$user.id==200)
movies200 <- movies[which(movies$movie.id %in% user200$movie.id),] 
movie.matrix <- movies200[6:24]
genre200 <- apply(movie.matrix,2,mean)


user50 <- subset(userRating, userRating$user.id == 50) 
movies50 <- movies[which(movies$movie.id %in% user50$movie.id),] 
movie.matrix <- movies50[6:24]
genre50 <- apply(movie.matrix, 2, mean)  

#Item Vector with ID 127
movieid.127 <-subset(movies[127,6:24])

cosine <- function(x, y) {

  sum(x*y)/(norm(x, type="2") * norm(y, type="2"))
}

#(i) user-user similarity of user with ID 200 and user with ID 50 is 0.548
cosine(genre200,genre50)
#(ii) the user-item similarity of movie with ID 127 to user 200 is 0.553
cosine(genre200,movieid.127)
#(iii) the user-item similarity of movie with ID 127 to user 50 is 0.624
cosine(genre50,movieid.127)

#(iv) user 50 would be recommended movie with ID 127 as it has higher cosine value



#(B) Collaborative Filtering

util  <- matrix(NA, nrow=6,ncol=(11))
colnames(util) <- c("1","21","44","59","72","82","102","234","268", "409","486")
rownames(util) <- c("1","2","3","4","5","6")

for(i in 1:nrow(util)) 
{
  for(j in 1:ncol(util)) 
  {
    temp = as.numeric(colnames(util)[j]) 
    if(length(which(userRating$user.id %in% temp & userRating$movie.id %in% i)) != 0)
    {
      index <- which(userRating$user.id %in% temp & userRating$movie.id %in% i)
      util[i,j] <- userRating$rating[index]
    }
  }
}

data <- util
temp <- subset(userRating, userRating$user.id %in% c( 1, 21, 44, 59, 72, 82, 102, 234, 268,409, 486) & userRating$movie.id %in% c(1,2,3,4,5,6)) 

rMean <- rowMeans(data,na.rm = TRUE)

for(i in 1:nrow(data)) 
{
  for(j in 1:ncol(data)) 
  {
    if(!is.na(data[i,j]))
    {
      data[i,j] <- data[i,j] - rMean[[i]]
    } 
    else
    {
      data[i,j] <- 0 
    }
  }
}


sim1_5 <- cosine(as.numeric(data[1,]),as.numeric(data[5,]))
sim1_5
sim2_5 <- cosine(as.numeric(data[2,]),as.numeric(data[5,]))
sim2_5
sim3_5 <- cosine(as.numeric(data[3,]),as.numeric(data[5,]))
sim3_5
sim4_5 <- cosine(as.numeric(data[4,]),as.numeric(data[5,]))
sim4_5
sim6_5 <- cosine(as.numeric(data[6,]),as.numeric(data[5,]))
sim6_5

# sim3_5, sim2_5 and sim4_5 has the highest cosine similarity w.r.t 5 movie for N=3

#So rating user 268 will give to movie 5 is 2.61
c((util[3,9]*sim3_5 + util[2,9]*sim2_5 + util[4,9]*sim4_5)/(sim3_5+sim2_5+sim4_5))

util[5,9] = c((util[3,9]*sim3_5 + util[2,9]*sim2_5 + util[4,9]*sim4_5)/(sim3_5+sim2_5+sim4_5))



