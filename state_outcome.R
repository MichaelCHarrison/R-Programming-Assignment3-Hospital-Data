best <- function(state, outcome){
        setwd("/Users/michaelharrison/Desktop/Coursera/R Programming")
        df <- read.csv("outcome-of-care-measures.csv", na.strings = "Not Available", 
                       stringsAsFactors = FALSE)
        state_list <- unique(df['State'])
        outcomes <- c("heart attack", "heart failure", "pneumonia")
        state_sel <- subset(df, State == state)
                
        if(outcome == "heart attack"){
                min_val <- min(state_sel[,11], na.rm = TRUE)
                best_hospital <- state_sel[which(state_sel[,11] == min_val), "Hospital.Name"]
        }
        if(outcome == "heart failure"){
                min_val <- min(state_sel[,11], na.rm = TRUE)
                best_hospital <- state_sel[which(state_sel[,17] == min_val), "Hospital.Name"]
        }
        if(outcome == "pneumonia"){
                min_val <- min(state_sel[,11], na.rm = TRUE)
                best_hospital <- state_sel[which(state_sel[,23] == min_val), "Hospital.Name"]
        }

        best_hospital
}