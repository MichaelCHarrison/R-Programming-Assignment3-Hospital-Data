        rankall <- function(outcome, num){
        setwd("/Users/michaelharrison/Desktop/Coursera/R Programming")
        df <- read.csv("outcome-of-care-measures.csv", na.strings = "Not Available",
                       stringsAsFactors = FALSE)
        
        outcomes <- c("heart attack" = 11, "heart failure" = 17, "pneumonia" = 23)
        
        if(outcome %in% names(outcomes)){
                sub_df <- df[, c(2,7,outcomes[outcome])]
                names(sub_df) <- c("Hospital", "State", "Outcome")
                sub_order <- sub_df[with(sub_df, order(Outcome, Hospital, na.last = NA)),]
                split_df <- split(sub_order, sub_order$State)
                
                states <- c(unique(sub_df$State))
                for(abv in states){
                        rownames(split_df[[abv]]) <- 1:nrow(split_df[[abv]])
                }
                
                rank_listing <- sapply(states, function(x) split_df[[x]][num,1])
        }
        else(stop("invalid outcome"))
        
        unlist(rank_listing)
}s
