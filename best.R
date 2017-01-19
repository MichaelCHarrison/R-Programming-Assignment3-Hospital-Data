best <- function(state, outcome){
        setwd("/Users/michaelharrison/Desktop/Coursera/R Programming")
        df <- read.csv("outcome-of-care-measures.csv", na.strings = "Not Available", 
                       stringsAsFactors = FALSE)
        
        state_list <- unique(df['State'])
        outcomes <- c("heart attack" = 11, "heart failure" = 17, "pneumonia" = 23)
        
        if(outcome %in% names(outcomes)){
        outcome_sel <- df[,c(2,7,outcomes[outcome])]
        names(outcome_sel) <- c("Hospital", "State", "Outcome")
        
                if(state %in% as.character(state_list[['State']])){
                state_sel <- subset(outcome_sel, State == state)
                order <- state_sel[with(state_sel, order(Outcome, Hospital, na.last = NA)),]
                best_hospital <- order[1,1]
                }
                else(stop("invalid state", call. = TRUE))
        }
        else(stop("invalid outcome", call. = TRUE))
        
        best_hospital
        
}