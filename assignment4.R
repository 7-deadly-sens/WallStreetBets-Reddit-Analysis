# George Garcia | 11.16.19
#
# "assignment4.r": Collects Reddit forum posts from the 'WallStreetBets' community to create 
# user network graphs with 'dplyr' and identify entities such as persons, organizations, 
# locations, dates, monies, and percentages with 'openNLP'.



# To prevent java.lang.OutOfMemoryError, set Java heap size
options(java.parameters = "-Xmx8g")

# Load packages
library(RedditExtractoR)
library(dplyr)
library(NLP)
library(openNLP)
library(openNLPmodels.en)


#####
# 1

# Search Reddit for threads that contain the word "stocks" and return 1 page (25 results)
stocksURL <- reddit_urls(search_terms = "stocks", subreddit = "WallStreetBets", page_threshold = 1)



#####
# 2

# Filter results by URL with the smallest amount of comments, and then we use it's URL to get replies
minThreadDF <- stocksURL %>% filter(num_comments == min(stocksURL$num_comments)) %$% URL %>% reddit_content 

# Extract the user network of replies, excluding the author of original post, and shows aggregated results
stocksNetwork <- minThreadDF %>% user_network(include_author=FALSE, agg=TRUE)

# Interactive plot of the user network, with messages
stocksNetwork$plot


#####
# 3

# Keep only post ID and post text
Reddit <- data.frame(minThreadDF$id, minThreadDF$comment)
colnames(Reddit) <- c('Post', 'Comment')
View(Reddit)

# Set up annotators for person, organization, location, date, money and percentage
person_annotator = Maxent_Entity_Annotator(kind = 'person')
organization_annotator = Maxent_Entity_Annotator(kind = 'organization')
location_annotator = Maxent_Entity_Annotator(kind = 'location')
date_annotator = Maxent_Entity_Annotator(kind = 'date')
money_annotator = Maxent_Entity_Annotator(kind = 'money')
percentage_annotator = Maxent_Entity_Annotator(kind = 'percentage')

# Create empty data frame to hold extracted entities with 4 columns of data: Post, Type, Entity, and Position
RedditEntities = data.frame(Post=numeric(), Type=character(), Entity=character(), Position=numeric(), stringsAsFactors=FALSE)

#repeat for each row in dataframe
#ensure post is string
#tokenize post
#annotate  tokens
#extract portion of post tagged as  entity and 
#append to RedditEntities dataframe
for (post in 1:nrow(Reddit))  # repeat for each row in dataframe
{
  RedditText=as.String(Reddit[post,2]) # retrieve text
  RedditTokens=annotate(RedditText, 
                        list(Maxent_Sent_Token_Annotator(), # Sentence token
                             Maxent_Word_Token_Annotator())) # Word token
  RedditPersTokens=annotate(RedditText, list(person_annotator), RedditTokens) # set up annotator for persons
  RedditOrgsTokens=annotate(RedditText, list(organization_annotator), RedditTokens) # set up annotator for organizations
  RedditLocsTokens=annotate(RedditText, list(location_annotator), RedditTokens) # set up annotator for locations
  RedditDatsTokens=annotate(RedditText, list(date_annotator), RedditTokens) # set up annotator for dates
  RedditMonsTokens=annotate(RedditText, list(money_annotator), RedditTokens) # set up annotator for monies
  RedditPctsTokens=annotate(RedditText, list(percentage_annotator), RedditTokens) # set up annotator for percentages
  
  RedditPerson=subset(RedditPersTokens,RedditPersTokens$features=='list(kind = "person")') # extract persons
  RedditOrganization=subset(RedditOrgsTokens,RedditOrgsTokens$features=='list(kind = "organization")') # extract organizations
  RedditLocation=subset(RedditLocsTokens,RedditLocsTokens$features=='list(kind = "location")') # extract locations
  RedditDate=subset(RedditDatsTokens,RedditDatsTokens$features=='list(kind = "date")') # extract dates
  RedditMoney=subset(RedditMonsTokens,RedditMonsTokens$features=='list(kind = "money")') # extract monies
  RedditPercentage=subset(RedditPctsTokens,RedditPctsTokens$features=='list(kind = "percentage")') # extract percentages
  
  # Add extracted persons to dataframe containing extracted entities
  for (i in 1:nrow(as.data.frame(RedditPerson))) # repeat for each row in the persons list
  {
    if (nrow(as.data.frame(RedditPerson))>0) {
      # add post ID, 'Person', name of person extracted, and start position in text into empty RedditEntries dataframe
      RedditEntities=rbind(RedditEntities, cbind(post, 'Person', substr(paste(RedditText, collapse=' '),
                                                      RedditPerson$start[i],RedditPerson$end[i]), # These extract entire character field from start position to finish position
                                                RedditPerson$start[i])) 
    }
  }

  # Add extracted organizations to dataframe containing extracted entities
  for (i in 1:nrow(as.data.frame(RedditOrganization)))
  {
    if (nrow(as.data.frame(RedditOrganization))>0) {
      RedditEntities=rbind(RedditEntities, cbind(post, 'Organization', substr(paste(RedditText, collapse=' '),
                                                                              RedditOrganization$start[i],RedditOrganization$end[i]),RedditOrganization$start[i]))
    }
  }
  
  # Add extracted locations to dataframe containing extracted entities
  for (i in 1:nrow(as.data.frame(RedditLocation)))
  {
    if (nrow(as.data.frame(RedditLocation))>0) {
      RedditEntities=rbind(RedditEntities, cbind(post, 'Location', substr(paste(RedditText, collapse=' '),
                                                                              RedditLocation$start[i],RedditLocation$end[i]),RedditLocation$start[i]))
    }
  }
  
  # Add extracted dates to dataframe containing extracted entities
  for (i in 1:nrow(as.data.frame(RedditDate)))
  {
    if (nrow(as.data.frame(RedditDate))>0) {
      RedditEntities=rbind(RedditEntities, cbind(post, 'Date', substr(paste(RedditText, collapse=' '),
                                                                              RedditDate$start[i],RedditDate$end[i]),RedditDate$start[i]))
    }
  }
  
  # Add extracted monies to dataframe containing extracted entities
  for (i in 1:nrow(as.data.frame(RedditMoney)))
  {
    if (nrow(as.data.frame(RedditMoney))>0) {
      RedditEntities=rbind(RedditEntities, cbind(post, 'Money', substr(paste(RedditText, collapse=' '),
                                                                      RedditMoney$start[i],RedditMoney$end[i]),RedditMoney$start[i]))
    }
  }
  
  # Add extracted percentages to dataframe containing extracted entities
  for (i in 1:nrow(as.data.frame(RedditPercentage)))
  {
    if (nrow(as.data.frame(RedditPercentage))>0) {
      RedditEntities=rbind(RedditEntities, cbind(post, 'Percentage', substr(paste(RedditText, collapse=' '),
                                                                              RedditPercentage$start[i],RedditPercentage$end[i]),RedditPercentage$start[i]))
    }
  }
}

#rename columns
colnames(RedditEntities)=c('Post', 'Type', 'Entity', 'Position')

View(RedditEntities)

#merge entity tags with posts
RedditExtratedEntities=merge(RedditEntities, Reddit, by.x='Post', by.y='Post')

View(RedditExtratedEntities)

