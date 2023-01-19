key = "Wtn7cEgiT5okpz8qljQ1nbNzJ" 
secret = "ariCoPDXMCov0SWacAjifw3zwsVE3tbkfmHkhJ5Dap7VHgSvto"

#API key: #Wtn7cEgiT5okpz8qljQ1nbNzJ
#API secret key: #ariCoPDXMCov0SWacAjifw3zwsVE3tbkfmHkhJ5Dap7VHgSvto


#===============# # AUTHORISING R # #===============#
library("rtweet") 
library("base64enc") 
library("httpuv") 
library("gridExtra") 
library("Matrix")

#Set up OAuth


create_token( app = "my_twitter_app", consumer_key = key, consumer_secret = secret)

# 1. Find 20 friends of trump that have the most followers. Use only Trump's friends, # not corporate accounts. Examine the twitter and summarize these people.
trump_friends <- get_friends("@realDonaldTrump", n = 5000)

# The function above gives us the number of friends that trump has (the people he follows). # (Use twitter definition in report).


trump_friends_info <- lookup_users(trump_friends$user_id)
trump_friends_names <- trump_friends_info$screen_name
trump_friends_names


# the last two functions have given us the screen_names of his friends. This enables us to filter # out Trump's company twitter handles.
# Get rid of "TrumpGolf", "TrumpDoral", "TrumpCharlotte", "TrumpLasVegas", "TrumpChicago", # "TrumpGolfDC", "TrumpGolfLA",
# Get indices for these names

company_names <- c("TrumpGolf", "TrumpDoral", "TrumpCharlotte", "TrumpLasVegas", "TrumpChicago", "TrumpGolfDC", "TrumpGolfLA")

N = length(company_names) indi <- c()

for (i in 1:N)
{ indi[i] <- which(trump_friends_names == company_names[i]) }

trumpfriends_filt <- trump_friends_names[-indi]

# Count how many followers the friends of trump have and count the top 20 friends with the most.

followers_of_tfriends <- lookup_users(trumpfriends_filt)$followers_count
followers_table <- cbind(trumpfriends_filt, followers_of_tfriends)
followers_table <- as.data.frame(followers_table)
followers_table[ ,1] <- as.character(followers_table[, 1]) followers_table[ ,2] <- as.numeric(as.character(followers_table[, 2]))
str(followers_table)

top_20 <- order(followers_table$followers_of_tfriends, decreasing = TRUE)[1:20] 
top_20

# top 20 friends of trump fot_followers20 <- followers_table[top_20, ] #decided to keep whitehouse in it.
#info of followers

tfriends_info <- lookup_users(fot_followers20$trumpfriends_filt)
tfriends_info$description
followers_des <- cbind(fot_followers20, tfriends_info$description)

# Summary # The peope that are friends of trump who also have the most followers tend to be political # commentators, senators, and those active in the american political landscape. Most, if not all # of his friends with the most followers are either republicans and or advocates for the party and # family members.
#2. Find the 20 people who follow Trump and have the most followers. Examine if they have a positive # or negative relationship with Trump based on their tweets. Download a dataset with a large number of # followers and select the 20 that have the greatest number of followers.
# lookup_users()$followers_count # get_followers()

followers_trump25k <- get_followers("realDonaldTrump", n = 25000)
tfollowers_followers <- as.data.frame(lookup_users(followers_trump25k$user_id))
trumpf_f <- tfollowers_followers[ , c("user_id","screen_name", "followers_count")]
top_20fol <- order(trumpf_f$followers_count, decreasing = TRUE)[1:20]

# top 20 followers of trump
top_20list <- trumpf_f[top_20fol, ]
followers_info <- lookup_users(top_20list$user_id)
followers_info$description
# negative or positive relationship

pos_follow <- c("No", "Yes", "Yes", "Yes", "No", "No", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "No", "Yes", "No", "Yes", "Yes", "No", "No")
relation_fol_list <- cbind(top_20list[ ,2], pos_follow)
colnames(relation_fol_list) <- c("Followers", "Positive Relationship?")
followers_info[, c("screen_name", "description")]

# 3. Bypassing Trump #Plot the graph containing Trump’s 20 friends and 20 followers. Identify if any of the found friends #or followers are friends with each other and add these edges to the graph. Then determine if #any of the friends and followers should be friends, based on their background, and add those edges #to the graph.
friends_and_followers <- data.frame(fof = c(fot_followers20$trumpfriends_filt, top_20list$screen_name))
friends_and_followers <- as.vector(friends_and_followers)
friends_and_followers <- apply(friends_and_followers, 1, FUN = as.character)
adj_mat <- matrix(data = 0, nrow = 40, ncol = 40)
adj_df <- as.data.frame(adj_mat)

colnames(adj_df) <- friends_and_followers 
rownames(adj_df) <- friends_and_followers
adj_df
followers_pp<- lookup_users(as.numeric(as.character(get_followers(friends_and_followers[1])$user_id)))$screen_name
# LOOK UP USERS
#lookup_friendships
adj_val <- as.data.frame(matrix(data = 0, nrow=40, ncol=40, byrow = TRUE))
for(i in 1:40){ for(j in 1:40){ if(dim(lookup_friendships(friends_and_followers[i], friends_and_followers[j]))[1] < 1){ adj_val[i ,j] <- "FALSE" }else{adj_val[i,j] <- lookup_friendships(friends_and_followers[i], friends_and_followers[j])[[11,4]]} } }
adj_val
adj_valx <- adj_val 

for(i in 1:dim(adj_val)[1]){ 
  for(j in 1:dim(adj_val)[2]){ 
    if(adj_val[i,j] == "TRUE"){ adj_valx[i, j] <- 1 }
    else{ adj_valx[i, j] <- 0} } 
  }
adj_valx <- as.matrix(adj_valx) 
adj_valx <- apply(adj_valx, 2, FUN = as.numeric)
rownames(adj_valx) <- friends_and_followers 
colnames(adj_valx) <- friends_and_followers
adj_valx <- as(adj_valx, "dgCMatrix")
ga <-graph.adjacency(adj_valx, mode = "undirected")
plot(ga)
library("igraph")
g = graph.formula("WhiteHouse" - "VP", "WhiteHouse" - "IvankaTrump", "seanhannity", "WhiteHouse" - "DonaldJTrumpJr", "WhiteHouse" - "Mike_Pence", "WhiteHouse" - "PressSec", "WhiteHouse" - "EricTrump", "WhiteHouse" - "IngrahamAngle", "TuckerCarlson", "WhiteHouse"-"KellyannePolls", "BillOReilly", "VinceMcMahon", "WhiteHouse" - "JudgeJeanine", "WhiteHouse" - "TeamTrump", "WhiteHouse" - "DiamondandSilk", "WhiteHouse" - "Jim_Jordan", "WhiteHouse" - "foxandfriends", "WhiteHouse"- "JesseBWatters","WhiteHouse"-"greta", "DonCheadle", "JohnKStahlUSA", "queensavagedoll", "Abdulmalik_fr", "WhiteHouse"- "rikomrnk", "Gideon_Lagat", "thedrsec", "VIP0552525000", "WhiteHouse"-"RightWingLawMan", "MintzGolf", "pinoyhotxxx", "Khanpk122", "rebeccabutlerm2", "maturetsceline", "maquialifraco", "ceibonacional", "AshleyJohnsonh", "InformedPatriot", "KenzieKay69", "alkrit2",
                  "VP"-"WhiteHouse", "VP"-"IvankaTrump", "VP"-"DonaldJTrumpJr", "VP"-"Mike_Pence", "VP"-"PressSec", "VP"-"EricTrump", "VP"-"IngrahamAngle", "VP"-"KellyannePolls", "VP"-"JudgeJeanine", "VP"-"TeamTrump", "VP"-"Jim_Jordan", "VP"-"foxandfriends", "VP"-"JesseBWatters", "VP"-"greta", "VP"- "RightWingLawMan",
                  "IvankaTrump"-"DonaldJTrumpJr", "IvankaTrump"-"Mike_Pence", "IvankaTrump"-"PressSec", "IvankaTrump"-"EricTrump", "IvankaTrump"-"IngrahamAngle",
                  #what i added 
                  "TuckerCarlson"-"foxandfriends", "TuckerCarlson"-"seanhannity", "TuckerCarlson"-"Mike_Pence", "TuckerCarlson"-"IngrahamAngle", "TuckerCarlson"-"VP", "TuckerCarlson"-"BillOReilly", "TuckerCarlson"-"EricTrump", "TuckerCarlson"-"DonaldJTrumpJr", "TuckerCarlson"-"IvankaTrump",
                  "MintzGolf"-"TuckerCarlson", "MintzGolf"-"seanhannity", "MintzGolf"-"BillOReilly", "MintzGolf"-"foxandfriends", "MintzGolf"-"IngrahamAngle", "MintzGolf"-"KellyannePolls",
                  "DonCheadle"-"WhiteHouse", "DonCheadle"-"VinceMcMahon",
                  "VinceMcMahon"-"WhiteHouse", "VinceMcMahon"-"IvankaTrump", "VinceMcMahon"-"DonaldJTrumpJr", "VinceMcMahon"-"Mike_Pence", "VinceMcMahon"-"PressSec", "VinceMcMahon"-"EricTrump", "VinceMcMahon"-"IngrahamAngle", "VinceMcMahon"-"KellyannePolls", "VinceMcMahon"-"JudgeJeanine", "VinceMcMahon"-"TeamTrump", "VinceMcMahon"-"Jim_Jordan", "VinceMcMahon"-"foxandfriends", "VinceMcMahon"-"JesseBWatters", "VinceMcMahon"-"greta", "VinceMcMahon"- "RightWingLawMan", "VinceMcMahon"-"TuckerCarlson",
                  "JohnKStahlUSA"-"WhiteHouse", "JohnKStahlUSA"-"IvankaTrump", "JohnKStahlUSA"-"DonaldJTrumpJr", "JohnKStahlUSA"-"Mike_Pence", "JohnKStahlUSA"-"PressSec", "JohnKStahlUSA"-"EricTrump", "JohnKStahlUSA"-"IngrahamAngle", "JohnKStahlUSA"-"KellyannePolls", "JohnKStahlUSA"-"JudgeJeanine", "JohnKStahlUSA"-"TeamTrump", "JohnKStahlUSA"-"Jim_Jordan", "JohnKStahlUSA"-"foxandfriends", "JohnKStahlUSA"-"JesseBWatters", "JohnKStahlUSA"-"greta", "JohnKStahlUSA"- "RightWingLawMan", "JohnKStahlUSA"-"TuckerCarlson",
                  "rebeccabutlerm2"-"InformedPatriot", "thedrsec"-"JohnKStahlUSA", "thedrsec"-"TuckerCarlson", "thedrsec"-"BillOReilly", "thedrsec"-"seanhannity", "thedrsec"-"foxandfriends", "thedrsec"-"greta", "thedrsec"-"KellyannePolls",
                  #sport 
                  "Gideon_Lagat"-"thedrsec",
                  #strippers/porn/spam 
                  "queensavagedoll"-"pinoyhotxxx", "queensavagedoll"-"AshleyJohnsonh", "AshleyJohnsonh"-"pinoyhotxxx")
V(g)$label.cex = 0.7
plot(g, layout = layout.fruchterman.reingold, vertex.size = 8)
# WHO WOULD BE FRIENDS?
# 4. Graph Statistics #Compute the diameter and density of the graph, and neighbourhood overlap of each edge and determine which #nodes have the greatest social capital. State if the results are obvious from the graph structure and why.
# DIAMETER # It is the shortest distance between the two most distant nodes in the network.
                  
dia.g <- diameter(g)
                  
# Density # Density is the number of actual edges as a ratio of possible maximal edges.
                  
den.g <- graph.density(g)
                  
#overlap
                  
#neighbors of both A and B/neighbors of A or B.
                  
#use ends function to get vertices of edge # intersect and neighbors
                  
n_pairs <- ends(g, E(g)) n_pairs <- as.data.frame(n_pairs)
                  
neighborhood_overlap <- function(x){ 
  node_pairs <- ends(x, E(x)) 
  numerator <- c() 
  denom <- c() 
  no <- c() 
  for(i in 1:length(E(x))){ 
    numerator[i] <- length(intersect(neighbors(x, v = node_pairs[i,1]), 
                                     neighbors(x, v = node_pairs[i, 2]))) 
    denom[i] <- length(union(neighbors(x, v=node_pairs[i, 1]), neighbors(x, v=node_pairs[i,2])))-2 
    no[i] <- numerator[i]/denom[i]
                  } return(no) }
                  
n_overlap <- neighborhood_overlap(g) 
n_pairs[ , 3] <- n_overlap 
no_edges <- n_pairs
                  
# Three Local bridges
                  
ind_lb <- which(no_edges[, 3] == min(no_edges[, 3], na.rm = TRUE)) no_edges[ind_lb, ]
                  
# Whitehouse - DiamondandSilk # Whitehouse - rikomrnk # KellyannePolls - MintzGolf
                  
#Structural Holes holes <- constraint(g, nodes = V(g), weights = NULL)
                  
#5 HOMOPHILY
                  
positive <- c("Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "No", "Yes", 
              "Yes", "Yes", "No", "No", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "No", "Yes", "No", "Yes", "Yes", "No", "No")
                  
relation_trump <- as.data.frame(friends_and_followers) 
relation_trump[ , "Positive"] <- positive 
relation_trump
                  
                
g2 <- g 

V(g2)$label <-c("Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", 
                "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", 
                "Yes", "Yes", "No", "Yes", "Yes", "Yes", "No", "No", "Yes", "Yes", 
                "Yes", "Yes", "Yes", "Yes", "Yes", "No", "Yes", "No", "Yes", "Yes",
                                          "No", "No")
                  
# create adjacency matrix # no of edges 96
                  
adj_mattrump <- get.adjacency(g2) mat <- as.matrix(adj_mattrump) class <- c("Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "No", "Yes", "Yes", "Yes", "No", "No", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "No", "Yes", "No", "Yes", "Yes", "No", "No")
                  
colnames(mat) <- class 
rownames(mat) <- class
                  
#indices of the cross-sentiment edges. N_pos <- which(class == "No") Y_pos <- which(class == "Yes")
                  
mat[N_pos, Y_pos]
                  
#Total cross sentiment edges. network_cgl <- sum(mat[N_pos, Y_pos])
                  
#replicate 1000 times and make a histogram
                  
set.seed(1) 
z2 <- replicate(1000, { 
  samp.class <- sample(class, 40, replace = FALSE) 
  N_samp <- which(samp.class == "No") 
  Y_samp <- which(samp.class == "Yes") 
  sum(mat[N_samp, Y_samp]) })
                  
hist(z2, breaks =20, col = 'lightblue', main = "Crosslink Distribution") 
abline(v=mean(z2), col = 'green')
                  
# The mean of 1000 experiments.
                  
m.sim <- mean(z2) 
sd.sim <- sd(z2)
                  
m.sim 
sd.sim
                  
# analytic method
                  
e_crosslinks <- 2*(7/40)*(33/40)*96
                  
# 27.72
                  
# HYPOTHESIS TEST alpha = 0.05
                  
mean(z2 <= network_cgl) #0.001
                  
# Reject the null hypothesis, accept the alternative.
                  
#6. Structural Balance #Finally, determine if the signed network is weakly balanced 
#(using hierarchical clustering) and identify 
#if any within or between signed relationships are not as expected. 
#To perform this analysis, first label 
#all existing edges as either positive or negative, based on their association to Trump.
                  
#1, 21 #1, 25 #13, 21 #21, 1 #25, 1 #21, 13
                  
mat2 <- mat
                  
mat2[1, 21] <- -1 
mat2[1, 25] <- -1 
mat2[13, 21] <- -1 
mat2[21, 1] <- -1
mat2[25, 1] <- -1 
mat2[21, 13] <- -1
                  
matx <- mat2 
rownames(matx) <- c(1:40) 
colnames(matx) <- c(1:40)
                  
D <- as.matrix(1-mat2) 

## create a matrix of distances # a distance of 1 if there is no edges, a distance of 2 if they are in another #group, and a distance of # 0 if there are not in the same group. image(D) ## visualise the distances ## Notice the block structure
I <- as.matrix(mat2+1) 

plot(graph_from_adjacency_matrix(I))
# look at components that are connected. 
h <- hclust(as.dist(D), method = 'single') 
plot(h) 
image(D[h$order, h$order])