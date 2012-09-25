##
## Gets Twitter follower/following and user attributes and creates a network-package object
## to store the data.
## Saves this to a .RData file along with metadata to indicate date of acquisition and the run params
##
library("network")
library("twitteR")
library("RSQLite")
library("ROAuth")

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
## OUTPUT FILE SPEC
## ****************
run.name<-"test-1"
output.dir<-"/home/arc1/R Projects/SNA Output/TwitterNet"

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
cache.life<-14
#name of the SQLite database file to use
cache.db.filename<-"/home/arc1/R Projects/SNA/Source Data/TwitterNet.sqlite"
# >>>> parameters if type="users"
# which twitter screen names to use as the start-point from which to explore followers/following
start.sns<- c("jisccetis")
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

##
## Dealing with "celebrities" and similar
##
#The problem here is that some twitter users have very large numbers of followers and if we traverse these
# to make the graph, it gets un-weildly.
# Approach taken is to force a program stop when #followers exceeds the limit below to allow the user to decide what to do UNLESS:
# A) they are listed in start.sns (when their followers/friends are included)
# B) they are listed in whitelist.sns, in which case their friends/followers are included
# C) they are listed in greylist.sns, in which case the user record is added but friends/followers are ignored
followers.limit<-5000
#whitelist - suggest include anyone who is genuinely ed tech
whitelist.sns<-c("JISC", "pgsimoes", "mikeherrity","charlieanna")
greylist.sns<-c("stephenfry","BillBailey","nationaltrust",
                "BBCTech",
                "GdnHigherEd","TEDNews", "WiredUK", "timeshighered", "guardiantech", "BBCClick",
                "OpenGov", "billt", "persdevquotes",
                "educause", "Avaaz","w3c",
                "SirKenRobinson", "st_ffen")
#always whitelist starts
whitelist.sns<-c(start.sns,whitelist.sns)

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
#Twitter OAuth prep
load(file="oAuthCred.RData")
registerTwitterOAuth(cred)
# database prep
use.cache<-FALSE
if(cache.life>=0){
   # instantiate the SQLite driver in the R process
   sqlite<- dbDriver("SQLite")
   # open sqlite connection. db is a "connection"
   db<- dbConnect(sqlite, dbname=cache.db.filename)
   summary(db)
   use.cache<-TRUE
}else{
   db<-NA
}
#cache earliest date
cache.date<-NULL
if(use.cache){
   cache.date<-as.POSIXlt(Sys.Date())
   cache.date$mday<-cache.date$mday-cache.life
}

## *******************************
## FUNCTIONS
## (including throttling according to twitter api limits)
## *******************************
#get a quoted date for SQL
q4sql<-function(d){
   return (paste("'",as.character(d),"'", sep=""))
}
# throttle will force delay when the twitter request rate limits are reached. margin is the number of remaining hits at which a delay is inserted.
throttle<-function(margin=5){
   zz<-getCurRateLimitInfo()
   print(paste("Check Limit:",zz$getRemainingHits(),"requests remaining out of", zz$getHourlyLimit(), "limit"))
   while(zz$getRemainingHits()<=margin){
      print("Twitter rate limit met; waiting 10 minutes")
      Sys.sleep(600)      
      zz<-getCurRateLimitInfo()  
      print(paste("Twitter API:",zz$getRemainingHits(),"requests remaining out of", zz$getHourlyLimit(), "limit"))
   }
}
# lookupUsers in batches of 100 as required by the twitter API limits
# this will check whether any of the passed IDS have cached data
#returns a list: "users" contains what has been looked up from API (user objects) and "cached" is the ids that were not looked up
batchedLookupUsers<-function(ids){
   cache.hit.ids<-NULL
   users<-NULL
   #check for cache hits
   if(use.cache){
      sql<-paste("SELECT id FROM user WHERE cache_date >=",q4sql(cache.date),"AND id IN(",paste(ids, collapse=","),")")
      cache.hit.ids = dbGetQuery(db,sql)[,"id"]
      #remove the cached ones from what will be looked up
      if(length(cache.hit.ids)>0){
         ids<-ids[!(ids%in%cache.hit.ids)]
      }
   }
   print(paste("Loop-up User(s):",length(cache.hit.ids),"were cached,",length(ids),"will be fetched via API"))
   if(length(ids)>0){
      batches<-ceiling(length(ids)/100)
      users<-xLookupUsers(ids[1:min(100,length(ids))])
      if(batches>1){
         for(b in 2:batches){
            from<-1+100*(b-1)
            to<-min(100*b,length(ids))
            users<-c(users,xLookupUsers(ids[from:to]))
         }
      }
   }
   list(users=users, cached=cache.hit.ids)
}
# lookupUsers with API hit throttling and retries
xLookupUsers<-function(ids){
   tries<-0
   done=FALSE
   while(!done && tries<3){
      throttle()
      val<-tryCatch(lookupUsers(ids),
                    error=function(e) NULL)
      done<-!is.null(val)
      tries<-tries+1
      Sys.sleep(1)
   }
   if(is.null(val))stop(geterrmessage())
   return(val)
}
# get followers for a user with API hit throttling and retries. as a list of 1 member, which is named as the id of the argument user
#returns NA if a "Not authorized" error is thrown
ufol<-function(x){
   tries<-0
   done=FALSE
   while(!done && tries<3){
      throttle()
      val<-tryCatch({v<-list(x$getFollowerIDs())
                     names(v)<-x$id
                     v},
                    error=function(e){
                       if(e$message=="Error: Not authorized"){
                          print(paste("Skipping:",e$message))
                          NA
                       }else{
                          NULL
                       }
                    })
      done<-!is.null(val)
      tries<-tries+1
      Sys.sleep(1)
   }
   if(is.null(val))stop(geterrmessage())
   return(val)
}
#get ids of people user is following as a list of 1 member, which is named as the id of the argument user
#returns NA if a "Not authorized" error is thrown
ufri<-function(x){
   tries<-0
   done=FALSE
   while(!done && tries<3){
      throttle()
      val<-tryCatch({v<-list(x$getFriendIDs())
                     names(v)<-x$id
                     v},
                    error=function(e){
                       if(e$message=="Error: Not authorized"){
                          print(paste("Skipping:",e$message))
                          NA
                        }else{
                           NULL
                        }
                     })
      done<-!is.null(val)
      tries<-tries+1
      Sys.sleep(1)
   }
   if(is.null(val))stop(geterrmessage())
   return(val)
}
uids<-function(x){x$getId()}

## database lookups
cache.read.users<-function(ids){
   sql<-paste("SELECT description, status_count as statusesCount, name, created, screen_name as screenName, location, id FROM user WHERE id IN(",paste(ids, collapse=","),")")
   return(dbGetQuery(db,sql))
}
cache.read.following<-function(ids){
   following.list<-list()
   for(id in ids){
      sql<-paste("SELECT followed_id as id FROM relation WHERE follower_id =",q4sql(id))
      fid<-list(dbGetQuery(db,sql)[,"id"])
      names(fid)<-id
      following.list<-c(following.list,fid)
   }
   return(following.list)
}
cache.read.followers<-function(ids){
   followed.list<-list()
   for(id in ids){
      sql<-paste("SELECT follower_id as id FROM relation WHERE followed_id =",q4sql(id))
      fid<-list(dbGetQuery(db,sql)[,"id"])
      names(fid)<-id
      followed.list<-c(followed.list,fid)
   }
   return(followed.list)
}

## database insert/update.
didFail <- function(e){
   print(paste("Caught an error. DB Exception No=",dbGetException(db)$errorNum, " ", dbGetException(db)$errorMsg, sep=""))
   print("Roll-back:")
   dbRollback(db)
}
# done as a single transaction since the "cache_date" refers to the user and all relations.
#arguments are: a single user object, vectors of friends and followers
update.cache<-function(u,ufri, ufol, greylisted){
   tryCatch({      
      dbBeginTransaction(db)
      tryCatch({
         cache.insert.user(u, greylisted)
         tryCatch({
            if(!is.na(ufol[1])){
               cache.insert.followers(u[["id"]], ufol)
            }
            tryCatch({
               if(!is.na(ufri[1])){
                  cache.insert.friends(u[["id"]], ufri)
               }
               dbCommit(db)
               }, error = didFail)
            }, error = didFail)
         }, error = didFail)

   }, error = didFail)
} 
cache.insert.user<-function(u,greylisted){
   #use a prepared query style since description may contain characters that will mess up SQL created with paste()
   sqlTemplate<-paste("INSERT OR REPLACE INTO user",
                  "(description, status_count, name, created, screen_name, location, id, cache_date, greylisted)",
                  "VALUES($description, $status_count, $name, $created, $screen_name, $location, $id, $cache_date, $greylisted)")
   df<-data.frame(description=u[["description"]],
                  status_count=u[["statusesCount"]],
                  name=u[["name"]],
                  created=u[["created"]],
                  screen_name=u[["screenName"]],
                  location=u[["location"]],
                  id=u[["id"]],
                  cache_date=as.character(Sys.Date()),
                  greylisted = as.numeric(greylisted),
                  stringsAsFactors=FALSE)
   dbSendPreparedQuery(db, sqlTemplate, bind.data = df)
}

cache.insert.followers<-function(uid, ids){
   #remove all existing followers (some people may have stopped)
   sql.clear<-paste("DELETE FROM relation WHERE followed_id =", as.character(uid))
   dbSendQuery(db,sql.clear)
   #repopulate followers
   sqlTemplate<-"INSERT INTO relation (follower_id, followed_id) VALUES ($follower, $followed)"
   df<-data.frame(follower=ids,  followed=rep(uid,length(ids)))
   dbSendPreparedQuery(db, sqlTemplate, bind.data = df)
}
cache.insert.friends<-function(uid, ids){
   #remove all existing friends (user may have stopped following some people)
   sql.clear<-paste("DELETE FROM relation WHERE follower_id =", as.character(uid))
   dbSendQuery(db,sql.clear)
   #repopulate followers
   sqlTemplate<-"INSERT INTO relation (follower_id, followed_id) VALUES ($follower, $followed)"
   df<-data.frame(follower=rep(uid,length(ids)),  followed=ids)
   dbSendPreparedQuery(db, sqlTemplate, bind.data = df)
}


## *******************************
## MAIN - RUN.TYPE = "USERS"
## *******************************
# Prepare for 1st time. Get Twitter IDs to match the supplied screen names.
throttle(length(uids)*3)
loop.ids<-sapply(lookupUsers(start.sns), uids)
## Loop to required depth
for(d in 0:depth){
   loop.users.df<-data.frame()
   loop.following<-list()
   loop.followers<-list()
   print(paste("Loop for depth = ",d,"out of", depth))
   print(paste(length(loop.ids), "users to fetch:", paste(loop.ids, collapse=",")))
   # NODE INFO as data frame (using only a subset of what is available).
   #first fetch users from the API, skipping any that are cached
   lu<-batchedLookupUsers(loop.ids)
   #deal with cached users - look up their attributes and followers/friends info
   if(length(lu$cached)>0){
      loop.users.df<-cache.read.users(lu$cached)
      loop.following<-cache.read.following(lu$cached)
      loop.followers<-cache.read.followers(lu$cached)
   }
   #now deal with uncached users - extract attributes
   loop.users.uncached<-lu$users
   if(length(loop.users.uncached)>0){
      loop.users.df<-rbind(loop.users.df,twListToDF(loop.users.uncached)[c("description","statusesCount","name","created","screenName","location","id")])
      # now look up followers/friends for uncached users
      # build a list of vectors containing twitter IDs for each loop user
      # could also be done with loop.followers<-lapply(loop.users, ufol) but I want to watch progress of API calls
      for(u in loop.users.uncached){
         print(paste("For user id=",u$id,"screen name=",u$screenName,"#followers=",u$followersCount, "#friends=",u$friendsCount))
         #check for excessive followers and consult whitelist/greylist if necessary
         do.followers<-TRUE
         if(as.numeric(u$followersCount)>followers.limit){
            if(!(u$screenName%in%whitelist.sns)){
               if(!(u$screenName%in%greylist.sns)){
                  stop("User with excessive followers detected. Add their screen name to whitelist.sns or greylist.sns and restart to process")
               }
               print("Grey-listed: adding user but skipping their network")
               do.followers<-FALSE
            }
         }
         #fallback values
         u.friends<-NA
         u.followers<-NA
         #get the friends and followers
         if(do.followers){
            if(as.numeric(u[["friendsCount"]])>0){
               u.friends<-ufri(u)
               if(!is.na(u.friends[[1]][1])){
                  loop.following<-c(loop.following, u.friends)
               }
            }else{
               print("0 friends")
            }
            if(as.numeric(u[["followersCount"]])>0){
               u.followers<-ufol(u)
               if(!is.na(u.followers[[1]][1])){
                  loop.followers<-c(loop.followers, u.followers)
               }
            }else{
               print("0 followers")
            }
         }
         #save user and associated friends/followers to cache.
         #NB: when d==depth, this caches friends/followers that will not be part of the graph BUT these may be necessary for future runs
         update.cache(u,u.friends[[1]], u.followers[[1]], !do.followers)
      }
   }
   
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
if(use.cache){
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
cat(paste("run.type",run.type,sep="= "), file=info.file, append=FALSE, sep="\n")
cat(paste("run.date",run.date,sep="= "), file=info.file, append=TRUE, sep="\n")
cat(paste("start.sns",start.sns,sep="= "), file=info.file, append=TRUE, sep="\n")
cat(paste("depth",depth,sep="= "), file=info.file, append=TRUE, sep="\n")
cat(paste("hash.tags",hash.tags,sep="= "), file=info.file, append=TRUE, sep="\n")