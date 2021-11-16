library(shiny)

library(readxl)
library(tidyverse)
library(tidygraph)
library(ggraph)
library(visNetwork)

#setwd("C:/Users/maich/OneDrive/Bureau/Master/Stage/Fintech/Network")
#Import the data
companies <- read_excel("global_fintech.xlsx", sheet = "companies")
investors <- read_excel("global_fintech.xlsx", sheet = "investors")
rounds <- read_excel("global_fintech.xlsx", sheet = "rounds")

#Rename some of the columns
old_name <- c("Transaction Name","Organization Name","Announced Date","Investor Names")
new_name <- c("Trans_name","Organization", "Date","Investors")
colnames(rounds)[which(names(rounds) %in% old_name)] <- new_name

old_name1 <- c("Organization Name","CB Rank (Company)","Founded Date","Number of Employees")
new_name1 <- c("Name","CB_Rank","Founded_date","Nb_employees")
colnames(companies)[which(names(companies) %in% old_name1)] <- new_name1

old_name2 <- c("Organization/Person Name","Number of Investments","CB Rank (Investor)")
new_name2 <- c("Name","Nb_investments","CB_Rank_inv")
colnames(investors)[which(names(investors) %in% old_name2)] <- new_name2

#schange format of the variables
rounds$Investors <- str_split(rounds$Investors,", ")
rounds$Date <- as.Date(rounds$Date,format="%Y-%m-%d")
companies$Founded_date <- as.Date(companies$Founded_date,format="%Y-%m-%d")

filter_data <- function(start,end){
  #filter rounds
  round <- rounds %>% filter(Date >= start & Date <= end)
  return(round)
}

create_objnetwork <- function(round){
  #Create node and edge list
  rounds1 <- tibble(Company = round$Organization, Investor = round$Investors)%>% 
    unnest(Investor)%>% 
    filter(!is.na(Investor))
 
  # create the link list
  links <- rounds1 %>% group_by(Company,Investor) %>%
    summarise(weight = n()) %>%
    ungroup()

  #rename the column in order to full_join
  comp <- links %>%
    select(label = Company)

  inv <- links %>%
    select(label = Investor)

  #create node list
  nodes <- full_join(comp,inv,by=c("label")) %>%
    distinct(label) %>%
    arrange(label) %>%
    rowid_to_column("id") %>%
    mutate(type = ifelse(label %in% as.vector(t(comp)),"Company","Investor"))

  #change link list (only show companies' id)
  links <- links %>%
    left_join(nodes,by=c("Company"="label")) %>%
    select(to = id, Investor, weight) %>%
    left_join(nodes,by=c("Investor"="label")) %>%
    select(to,from = id, weight)

  #create network object
  fintech_network <- tbl_graph(nodes = nodes, edges = links, directed = TRUE)

  fintech_network <- fintech_network %>%
    activate(nodes) %>%
    mutate(degree.all = centrality_degree(mode="all"),
           indegree =  centrality_degree(mode="in"),
           outdegree =  centrality_degree(mode="out"),
           weighted.degree = centrality_degree(weights=weight,mode="all"),
           betweenness = centrality_betweenness(weights=weight),
           eigenvector = centrality_eigen())

  return(fintech_network)
}

#Plot the network
visu_network <- function(nodes,edges,choice,search){
  
  vis.nodes <- nodes %>%
    left_join(companies[new_name1],by=c("label"="Name")) %>%
    left_join(investors[new_name2],by=c("label"="Name"))
  vis.links <- edges
  
  #Nodes
  vis.nodes$shape  <- "dot"  
  vis.nodes$shadow <- TRUE # Nodes will drop shadow
  vis.nodes$label  <- vis.nodes$label # Node label
  colnames(vis.nodes)[which(names(vis.nodes) == choice)] <- "size"
  vis.nodes$size <- vis.nodes$size*3 + 10
  
  # Text on click
  vis.nodes$title <- ifelse(vis.nodes$type=="Company",
                            paste0("Nb of employees: ",vis.nodes$Nb_employees,"<br> CB Rank: ",vis.nodes$CB_Rank,"<br>",choice,": ",(vis.nodes$size-10)/3),
                            paste0("Nb of invesments: ",vis.nodes$Nb_investments,"<br> CB Rank: ",vis.nodes$CB_Rank_inv,"<br>",choice,": ",(vis.nodes$size-10)/3)
                      )
  
  
  vis.nodes$group <- vis.nodes$type
  search.names <- str_split(search, pattern=", ")
  vis.nodes[vis.nodes$label %in% search.names[[1]],"color.background"] <- "lightgreen"
  vis.nodes$color.border <- "black"
  vis.nodes$color.highlight.background <- "orange"
  vis.nodes$color.highlight.border <- "darkred"
  
  vis.nodes <- vis.nodes[!duplicated(vis.nodes$id),]
  
  #Edges
  vis.links$width <- 2 + vis.links$weight*3 # line width
  vis.links$color <-  "grey"  # line color  
  vis.links$arrows <- "middle"
  
  legendNodes <- data.frame(
    label = c("Company","Investor"),
    color.background = c("tomato","slategrey"),
    color.border = c("black", "black"),
    shape = c("dot", "dot")
  )
  
  visNetwork(vis.nodes, vis.links) %>%                 
    visGroups(groupname = "Company", color = "tomato") %>%    
    visGroups(groupname = "Investor", color = "slategrey") %>% 
    visEvents(type = "once", startStabilizing = "function() {
              this.moveTo({scale:0.25})}") %>% 
    visLegend(useGroups = FALSE, addNodes = legendNodes,position = "right",width=0.1) 
}

# Focus
id_chosen <- function(nodes,selected_items){
  id <- nodes[nodes$label %in% selected_items, 1]
  return(id)
}

# Value box
value_box <- function(nodes,chosen){
  df <- nodes %>% group_by(type) %>% summarise(count=n()) %>% 
    filter(type==chosen) %>% select(count)
  return(df[[1]])
}