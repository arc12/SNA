##
## Gets Twitter follower/following and user attributes and creates a network-package object
## to store the data.
## Saves this to a .RData file along with metadata to indicate date of acquisition and the run params
##
library("network")
library("twitteR")
library("RSQLite")

## !!!!!!!!!!!!
## NB
## !!!!!!!!!!!!
# NB1: The network is DIRECTED, where the edge direction is the "follows" direction.
#  Hence if A follows B then there is an edge from vertex A TO vertex B.
# NB2: This code may not fully take account of twitter API rate limits (150/hr).
#  (so may fail for large follower counts, for example)
# NB3: Does not make authenticated access, which will limit visibility (and rate limits below 350/hr)
# NB4: Caches data to a SQLite database and uses cached data if within cache.life age limit

## ****************
## RUN PARAMS
## ****************
# Run type:
# "users" - takes a set of twitter users and explores the network from there to a given depth
# "hash-tags" - takes a set of hash tags and queries twitter to find who used them
# "user-tags" - obtains a set of hash tags according to those used by a specified set of users
#                 (overrides the hash.tags parameter)
run.type<-"users"
# Use cache values if <= cache.life days old. A value less than zero disables any database use
# The cached data is a user's attributes and all followers/friends.
cache.life<--1
#name of the SQLite database file to use
cache.db.filename<-"/home/arc1/R Projects/SNA/Source Data/TwitterNet.sqlite"
# >>>> parameters if type="users"
# which twitter screen names to use as the start-point from which to explore followers/following
start.sns<- c("asimong")
#c("LornaMCampbell","asimong","mhawksey","PaulHollins","wilm","sheilmcn","dwrgi","markpower","christismart","scottbw") #cetis staff
# c("jisccetis") # CETIS comms account
# how many edges to traverse to locate nodes (i.e. depth). 0 means only use start.ids
depth<-1
# >>>> parameters if type="hash-tags"
hash.tags<-c()
# how many tweets to mine for users
ht.recent.tweets<-20
# >>>> parameters if type="user-tags"
# how many tweets to mine for hash tags
ut.recent.tweets<-20 

## ****************
## OUTPUT FILE SPEC
## ****************
run.name<-"test-1"
output.dir<-"/home/arc1/R Projects/SNA Output/TwitterNet"

## *******************
## ADDITIONAL METADATA
## *******************
run.date <- as.POSIXlt(Sys.time(), "UTC")

## 
## PRELIMINARIES
##
if(run.type!="users"){
   stop(" run.type NOT IMPLEMENTED")
}
# database prep
use.sqlite<-(cache.life>=0)
if(use.sqlite){
   # instantiate the SQLite driver in the R process
   sqlite<- dbDriver("SQLite")
   # open sqlite connection. db is a "connection"
   db<- dbConnect(sqlite, dbname=cache.db.filename)
   summary(db)
}else{
   db<-NA
}


## *******************************
## FUNCTIONS
## (including throttling according to twitter api limits)
## *******************************
# throttle will force delay when the twitter request rate limits are reached. margin is the number of remaining hits at which a delay is inserted.
throttle<-function(margin=1){
   zz<-getCurRateLimitInfo()
   while(zz$getRemainingHits()<=margin){
      print("Twitter rate limit met; waiting 10 minutes")
      Sys.sleep(600)      
      zz<-getCurRateLimitInfo()  
      print(paste("Twitter API:",zz$getRemainingHits(),"requests out of", zz$getHourlyLimit(), "remaining"))
   }
}
# lookupUsers in batches of 100 as required by the twitter API limits
batchedLookupUsers<-function(ids){
   batches<-ceiling(length(ids)/100)
   users<-xLookupUsers(ids[1:min(100,length(ids))])
   if(batches>1){
      for(b in 2:batches){
         from<-1+100*(b-1)
         to<-min(100*b,length(ids))
         users<-c(users,xLookupUsers(ids[from:to]))
      }
   }
   return(users)
}
# lookupUsers with API hit throttling and retries
xLookupUsers<-function(ids){
   throttle()
   tries<-0
   done=FALSE
   while(!done && tries<3){
      val<-tryCatch(lookupUsers(ids),
                    error=function(e) NULL)
      done<-!is.null(val)
      tries<-tries+1
      Sys.sleep(1)
   }
   if(is.null(val))stop(geterrmessage())
   return(val)
}
# get followers for a user with API hit throttling and retries
ufol<-function(x){
   throttle()
   tries<-0
   done=FALSE
   while(!done && tries<3){
      val<-tryCatch(x$getFollowerIDs(),
                    error=function(e) NULL)
      done<-!is.null(val)
      tries<-tries+1
      Sys.sleep(1)
   }
   if(is.null(val))stop(geterrmessage())
   return(val)
}
#get ids of people user is following
ufri<-function(x){
   throttle()
   tries<-0
   done=FALSE
   while(!done && tries<3){
      val<-tryCatch(x$getFriendIDs(),
                    error=function(e) NULL)
      done<-!is.null(val)
      tries<-tries+1
      Sys.sleep(1)
   }
   if(is.null(val))stop(geterrmessage())
   return(val)
}
uids<-function(x){x$getId()}

## *******************************
## MAIN - RUN.TYPE = "USERS"
## *******************************
# Prepare for 1st time. Get Twitter IDs to match the supplied screen names.
loop.ids<-sapply(lookupUsers(start.sns), uids)
## Loop to required depth
for(d in 0:depth){
   print(paste("Loop for depth = ",d,"out of", depth))
   print(paste(length(loop.ids), "users to fetch:", paste(loop.ids, collapse=",")))
   #node info as data frame (using only a subset of what is available)
   loop.users<-batchedLookupUsers(loop.ids)
   loop.users.df<-twListToDF(loop.users)[c("description","statusesCount","name","created","screenName","location","id")]
   #build a list of vectors containing twitter IDs for each loop user
   # could also be done with loop.followers<-lapply(loop.users, ufol) but I want to watch progress of API calls
   loop.following<-list()
   loop.followers<-list()
   for(u in loop.users){
      print(paste("For user id=",u[["id"]]))
      print("Getting friends")
      loop.following<-c(loop.following, list(ufri(u)))
      print("Getting followers")
      loop.followers<-c(loop.followers, list(ufol(u)))
   }
   names(loop.followers)<-loop.ids
   names(loop.following)<-loop.ids
   
   
   #accumulate loop data A - the users
   if(d==0){
      all.users.df<-loop.users.df
   }else{
      all.users.df<-rbind(all.users.df,loop.users.df)
   }
   
   # user IDs so far
   existing.ids<-all.users.df[,"id"]
   
   #accumulate loop data B - the edges (followings)
   # for the last iteration, remove any followed users that are not already known (i.e. are within the depth specified)
   if(d==depth){
      cleaned.following<-list()
      #loop over the follower
      for(ff in names(loop.following)){
         keep.ids<-as.character(na.omit(existing.ids[match(loop.following[[ff]], existing.ids)]))
         cleaned.following<-c(cleaned.following, list(keep.ids))
         
      }
      names(cleaned.following)<-names(loop.following)
      loop.following<-cleaned.following
   }
   if(d==0){
      #all.followers<-loop.followers is not needed since we get this as a "following" the next time around the loop
      all.following<-loop.following
   }else{
      #all.followers<-c(all.followers,loop.followers)
      all.following<-c(all.following, loop.following)
   }
   
   
   # merge the follower/following IDs and remove already-scanned IDs and duplicates to get the next loop ids
   if(d<depth){
      candidate.ids<-unique(unlist(c(loop.followers, loop.following), use.names=FALSE))
      omit.ids<-na.omit(match(existing.ids,candidate.ids))
      if(length(omit.ids)==0){
         loop.ids<-candidate.ids
      }else{
         loop.ids<-candidate.ids[-omit.ids]
      }
   }
}

# Twitter API Info
zz<-getCurRateLimitInfo()  
print(paste("Finished using Twitter API:",zz$getRemainingHits(),"requests out of", zz$getHourlyLimit(), "remaining"))

#close the SQLite database
if(use.sqlite){
   dbDisconnect(db)
}


## ************************
## BUILD THE NETWORK OBJECT
## (FROM NODE DATAFRAME AND FOLLOWER/FOLLOWING LISTS)
## ************************
# initialise an empty network with the right number of vertices
twitter.net<-network.initialize(length(all.users.df[,1]), directed=TRUE, hyper=FALSE, loops=FALSE, multiple=FALSE)
# add select attributes to the vertices
set.vertex.attribute(twitter.net, "name", as.character(all.users.df[,"name"]))
set.vertex.attribute(twitter.net, "created", as.character(all.users.df[,"created"]))
set.vertex.attribute(twitter.net, "statusesCount", as.character(all.users.df[,"statusesCount"]))
set.vertex.attribute(twitter.net, "location", as.character(all.users.df[,"location"]))
set.vertex.attribute(twitter.net, "id", as.character(all.users.df[,"id"]))
network.vertex.names(twitter.net)<-as.character(all.users.df[,"screenName"])
# add edges, first creating an edgelist matrix (1 row per edge, col1=sender, col2=received)
# NB the edgelist to supply to twitter.net is expressed in terms of the indexes of the vertices already
# created graph but the "following" data is in terms of twitter IDs
senders<-match(existing.ids,names(all.following))
for(s in senders){
   receivers<-match(all.following[[s]], existing.ids)
   if(length(receivers)>0){
      network.edgelist(matrix(c(rep(s,length(receivers)),receivers), nrow=length(receivers), ncol=2), twitter.net)
   }
}

#take a peek to see if it looks OK
plot(twitter.net, displaylabels=T, label.cex=0.7, boxed.labels=TRUE)

## **************************
## STORE NETWORK LEVEL METADATA IN THE OBJECT
## **************************
twitter.net %n% "run.type" <- run.type
twitter.net %n% "run.date"<-run.date
if(run.type=="users"){
   twitter.net %n% "start.ids"<-start.sns
   twitter.net %n% "depth"<-depth
}else if(run.type=="hash.tags"){
   twitter.net %n% "hash.tags"<-hash.tags
}

## *******************************
## WRITE OUT RESULTS AND METADATA
## *******************************
data.file<-paste(output.dir,paste(run.name,"RData",sep="."),sep="/")
info.file<-paste(output.dir,paste(run.name,"txt",sep="."),sep="/")
save(twitter.net, file=data.file)
sink(file=info.file, split=TRUE, type="output")
cat(paste("run.type",run.type,sep="= "))
cat(paste("\r\nrun.date",run.date,sep="= "))
cat(paste("\r\nstart.sns",start.sns,sep="= "))
cat(paste("\r\ndepth",depth,sep="= "))
cat(paste("\r\nhash.tags",hash.tags,sep="= "))
sink()#close


