rankhospital <- function(state, outcome, num = "best") {
        
        ## Read outcome data
        data = read.csv("outcome-of-care-measures.csv", colClasses = "character")
        
        ##change case to make sure state comes in upper case
        state = toupper(state)
        
        ##change case to make sure state comes in upper case
        outcome = tolower(outcome)        
        
        ## create a vector of valid outcomes
        valid.outcomes = c("heart attack", "heart failure", "pneumonia")
        
        ## create a vector of valid rankings
        valid.rankings = c("best", "worst")
        
        ## Check that if state, rankings and outcome are valid
        
        if(!(state %in% data$State)){
                stop("invalid state")                
        } else
        
        if(!(outcome %in% valid.outcomes)){
                stop("invalid outcome")
        } else
        
        if(!(class(num) == "numeric" | num %in% valid.rankings)){
                stop("The ranking can be either a number, best or worst catagory only!")
        }
        
        ## intialise hospital name to return
        my.hospital.name = NULL
        
        ## Return hospital name in that state with lowest 30-day death
        ## rate
        
        ## check if outcome is "heart attack"
                
        if(outcome == "heart attack"){
                
                if(class(num) == "character"){
                        if(num == "best"){
                                
                                ## subset the data based on state while removing the NAs
                                mySubset = subset(data, data[,11]!="Not Available" & data[,7] == state)
                                
                                ##convert Rates from Heart attack column to numeric
                                mySubset$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack = as.numeric(mySubset$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack)
                                
                                ## order subset based on rankings and alphabetically
                                mySubset = mySubset[order(mySubset[,11], mySubset[,2]),]
                                
                                ## select State, Hospital name, rate and rankings
                                mySubset = mySubset[,c(7,2,11)]
                                
                                ## get the first hospital name                
                                my.hospital.name = mySubset$Hospital.Name[1]
                                
                        } else {
                                
                                ## subset the data based on state while removing the NAs
                                mySubset = subset(data, data[,11]!="Not Available" & data[,7] == state)
                                
                                ##convert Rates from Heart attack column to numeric
                                mySubset$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack = as.numeric(mySubset$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack)
                                
                                ## order subset based on rankings and alphabetically
                                mySubset = mySubset[order(mySubset[,11], mySubset[,2]),]
                                
                                ## select State, Hospital name, rate and rankings
                                mySubset = mySubset[,c(7,2,11)]
                                
                                ## get the first hospital name                
                                my.hospital.name = mySubset$Hospital.Name[nrow(mySubset)]
                                
                        }
                } else if(class(num) == "numeric"){
                        
                        ## subset the data based on state while removing the NAs
                        mySubset = subset(data, data[,11]!="Not Available" & data[,7] == state)
                        
                        ##convert Rates from Heart attack column to numeric
                        mySubset$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack = as.numeric(mySubset$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack)
                        
                        ## order subset based on rankings and alphabetically
                        mySubset = mySubset[order(mySubset[,11], mySubset[,2]),]
                        
                        ## select State, Hospital name, rate and rankings
                        mySubset = mySubset[,c(7,2,11)]
                        
                        ## get the index based on user input                
                        my.hospital.name = mySubset$Hospital.Name[num]
                        
                }
        } 
        ## else if outcome is "heart failure"
        else if (outcome == "heart failure"){
                
                if(class(num) == "character"){
                        if(num == "best"){
                                
                                ## subset the data based on state while removing the NAs
                                mySubset = subset(data, data[,17]!="Not Available" & data[,7] == state)
                                
                                ##convert Rates from Heart failure column to numeric
                                mySubset$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure = as.numeric(mySubset$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure)
                                
                                ## order subset based on rankings and alphabetically
                                mySubset = mySubset[order(mySubset[,17], mySubset[,2]),]
                                
                                ## select State, Hospital name, rate and rankings
                                mySubset = mySubset[,c(7,2,17)]
                                
                                ## get the first hospital name                
                                my.hospital.name = mySubset$Hospital.Name[1]
                                
                                
                        } else {
                                
                                ## subset the data based on state while removing the NAs
                                mySubset = subset(data, data[,17]!="Not Available" & data[,7] == state)
                                
                                ##convert Rates from Heart failure column to numeric
                                mySubset$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure = as.numeric(mySubset$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure)
                                
                                ## order subset based on rankings and alphabetically
                                mySubset = mySubset[order(mySubset[,17], mySubset[,2]),]
                                
                                ## select State, Hospital name, rate and rankings
                                mySubset = mySubset[,c(7,2,17)]
                                
                                ## get the last hospital name                
                                my.hospital.name = mySubset$Hospital.Name[nrow(mySubset)]
                                
                        }
                } else if(class(num) == "numeric"){
                        
                        ## subset the data based on state while removing the NAs
                        mySubset = subset(data, data[,17]!="Not Available" & data[,7] == state)
                        
                        ##convert Rates from Heart failure column to numeric
                        mySubset$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure = as.numeric(mySubset$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure)
                        
                        ## order subset based on rankings and alphabetically
                        mySubset = mySubset[order(mySubset[,17], mySubset[,2]),]
                        
                        ## select State, Hospital name, rate and rankings
                        mySubset = mySubset[,c(7,2,17)]
                        
                        ## get the hospital name based on user input                
                        my.hospital.name = mySubset$Hospital.Name[num]
                        
                }
                
        } 
        ## else if outcome is "pneumonia"
        else if ("pneumonia" == "pneumonia"){
                
                if(class(num) == "character"){
                        if(num == "best"){
                                
                                ## subset the data based on state while removing the NAs
                                mySubset = subset(data, data[,23]!="Not Available" & data[,7] == state)
                                
                                ##convert Rates from pneumonia column to numeric
                                mySubset$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia = as.numeric(mySubset$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia)
                                
                                ## order subset based on rankings and alphabetically
                                mySubset = mySubset[order(mySubset[,23], mySubset[,2]),]
                                
                                ## select State, Hospital name, rate and rankings
                                mySubset = mySubset[,c(7,2,23)]
                                
                                ## get the first hospital name                
                                my.hospital.name = mySubset$Hospital.Name[1]
                                
                        } else {
                                
                                ## subset the data based on state while removing the NAs
                                mySubset = subset(data, data[,23]!="Not Available" & data[,7] == state)
                                
                                ##convert Rates from pneumonia column to numeric
                                mySubset$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia = as.numeric(mySubset$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia)
                                
                                ## order subset based on rankings and alphabetically
                                mySubset = mySubset[order(mySubset[,23], mySubset[,2]),]
                                
                                ## select State, Hospital name, rate and rankings
                                mySubset = mySubset[,c(7,2,23)]
                                
                                ## get the last hospital name                
                                my.hospital.name = mySubset$Hospital.Name[nrow(mySubset)]
                                
                        }
                } else if(class(num) == "numeric"){
                        
                        ## subset the data based on state while removing the NAs
                        mySubset = subset(data, data[,23]!="Not Available" & data[,7] == state)
                        
                        ##convert Rates from pneumonia column to numeric
                        mySubset$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia = as.numeric(mySubset$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia)
                        
                        ## order subset based on rankings and alphabetically
                        mySubset = mySubset[order(mySubset[,23], mySubset[,2]),]
                        
                        ## select State, Hospital name, rate and rankings
                        mySubset = mySubset[,c(7,2,23)]
                        
                        ## get the hospital name based on user input                 
                        my.hospital.name = mySubset$Hospital.Name[num]
                        
                }
                
        }

                my.hospital.name
                
        
}