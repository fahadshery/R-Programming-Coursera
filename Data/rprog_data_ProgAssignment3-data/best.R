best <- function(state, outcome) {
        
        ## Read outcome data
        data = read.csv("outcome-of-care-measures.csv", colClasses = "character")
        
        ##change case to make sure state comes in upper case
        state = toupper(state)
        
        ##change case to make sure state comes in upper case
        outcome = tolower(outcome)
        
        ## create a vector of valid outcomes
        valid.outcomes = c("heart attack", "heart failure", "pneumonia")
        
        ## Check that state and outcome are valid
        if(!(state %in% data$State)){
                stop("invalid state")                
        } else if(!(outcome %in% valid.outcomes)){
                stop("invalid outcome")
        }
        
        
        ## taking the subset of the data based on user input and ignoring Nor Available 
        mySubset = subset(data, data[,11]!="Not Available" & data[,7] == state)
        my.hospital.name = NULL
        
        ## Return hospital name in that state with lowest 30-day death
        ## rate
        
        if(outcome == "heart attack"){
                index = which.min(mySubset$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack)
                my.hospital.name = mySubset$Hospital.Name[index]
                
        }
        
        else if(outcome == "heart failure"){
                
                my.hospital.name = mySubset$Hospital.Name[which.min(mySubset$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure)]
                
        }
        else if(outcome == "pneumonia"){
                
                my.hospital.name = mySubset$Hospital.Name[which.min(mySubset$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia)]
                
        }
        my.hospital.name
        
        
}