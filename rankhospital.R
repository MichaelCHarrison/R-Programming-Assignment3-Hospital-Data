rankhospital <- function(state, outcome, num){
        setwd("/Users/michaelharrison/Desktop/Coursera/R Programming")
        df <- read.csv("outcome-of-care-measures.csv", na.strings = "Not Available", 
                       stringsAsFactors = FALSE)
        
        state_list <- unique(df['State'])
        outcomes <- c("heart attack" = 11, "heart failure" = 17, "pneumonia" = 23)
        
        
        if(outcome %in% names(outcomes)){
                outcome_df <- df[,c(2,7,outcomes[outcome])]
                names(outcome_df) <- c("Hospital", "State", "Outcome")
                
                if(state %in% as.character(state_list[['State']])){
                        state_sel <- subset(outcome_df, State == state)
                        rank <- state_sel[with(state_sel, order(Outcome, Hospital, na.last = NA)),]
                        rownames(rank) <- 1:nrow(rank)
                        
                        nums <- c("best" = 1, "worst" = nrow(rank))
                        if(num %in% names(nums)){ hospital_position <- rank[nums[num], 1] }
                        else(hospital_position <- rank[num, 1])
                }
                else(stop("invalid state", call. = TRUE))
        }
        else(stop("invalid outcome", call. = TRUE))
        
        hospital_position
        
}
