
setwd(" ")
        
        if(!require(arules)){
                install.packages("arules")
        }
        library(arules)
        
        if(!require(arulesViz)){
        	    install.packages("arulesViz")
        }
        library(arulesViz)
        
        if(!require(igraph)){
                install.packages("igraph")
        }
        library(igraph)
        
        if(!require(visNetwork)){
                install.packages("visNetwork")
        }
        library(visNetwork)
        
        
        if(!require(plyr)){
                install.packages("plyr")
        }
        library(plyr)
        

        congress <- read.csv("Congressional Voting Records.csv", na.string = '?')
        
        #   check data type
        str(congress)
        
        nrow(congress)
               
        # generate association rules
        rules <- apriori(congress, parameter = list(sup = 0.35, conf = 0.8, target = "rules"),  
        						appearance = list(default = 'lhs', rhs = c('party=democrat', 'party=republican')))
        						
	rules <- sort(rules, decreasing = TRUE, by = "support")
	
	inspect(rules[1:5])     
	
	 top5_rules <- sort(rules, decreasing = TRUE, by = "support")[1:5]
  
   #   overview of rules
        plot(top5_rules, shading="lift", control=list(main = "Two-key plot of Congressional voting"))

        #   Targeting Party
             rule_D <- apriori(congress, parameter = list(sup = 0.35, conf = 0.8, target = "rules"), 
                                        appearance = list(default = 'lhs', rhs = c('party=democrat')))
                                        
             rule_D <- sort(rule_D, decreasing = TRUE, by = "confidence")
             inspect(rule_D[1:2])     
                
 			rule_R <- apriori(congress, parameter = list(sup = 0.35, conf = 0.8, target = "rules"), 
                                        appearance = list(default = 'lhs', rhs = c('party=republican')))
                                        
                rule_R <- sort(rule_R, decreasing = TRUE, by = "confidence")
                inspect(rule_R[1:2])      
        
        #   parallel coordinates plot
        plot(top5_rules, method = "paracoord", shading = "support")         

        #   create a basic graph structure
        ig <- plot(top5_rules, method = "graph", alpha=1, edgeCol="black")
        
        #   use igraph
        ig_df <- get.data.frame(ig, what = "both")
        
        #   generate nodes
        nodes <- data.frame(id = ig_df$vertices$name,
                            #   the size of nodes: could change to lift or confidence
                              value = ig_df$vertices$support,      
                              title = ifelse(ig_df$vertices$label == "", ig_df$vertices$name, ig_df$vertices$label),
                              ig_df$vertices
                            )
        #   generate edges
        edges <- ig_df$edges
        
        #   directed network    manipulate network
        network <- visNetwork(nodes, edges) %>% 
                      visOptions(manipulation = TRUE) %>%   #    manipulate network
                      visEdges(arrows = 'to', scaling = list(min = 2, max = 2)) %>%   #   directed network
                      visInteraction(navigationButtons = TRUE)   # navigation buttons
        network


