rankall <- function(outcome, num = "best") {
        
        ## Read outcome data
        data = read.csv("outcome-of-care-measures.csv", colClasses = "character")
        
        ##change case to make sure state comes in upper case
        outcome = tolower(outcome)        
        
        ## create a vector of valid outcomes
        valid.outcomes = c("heart attack", "heart failure", "pneumonia")
        
        ## create a vector of valid rankings
        valid.rankings = c("best", "worst")
        
        ## Check that if rankings and outcome are valid
        
        if(!(outcome %in% valid.outcomes)){
                        stop("invalid outcome")
                } else
                        
                        if(!(class(num) == "numeric" | num %in% valid.rankings)){
                                stop("The ranking can be either a number, best or worst catagory only!")
                        }
        

        ## check if outcome is "heart attack"
        
        if (outcome == "heart attack") {
                outcome.id <- 11
                column.name <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"
        } 
        ## check if outcome is "heart failure"
        else if (outcome == "heart failure") {
                outcome.id <- 17
                column.name <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"
        } 
        ## otherwise outcome is "Pneumonia"
        else {
                outcome.id <- 23
                column.name <- "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"
        }
        
        ## get state names
        state = levels(factor(data$State))
        hospital = character()        
        
        ## For each state, find the hospital of the given rank
        for (i in state) {
                
                ## subset data according to state and select columns according to user input
                cdata = subset(data, State == i, select = c(2,7,outcome.id))
                
                ##order data based on rankings and name
                cdata = cdata[order(as.numeric(cdata[[column.name]]), cdata$Hospital.Name, na.last = NA),]
                
                ##now check user input and set the rank
                if (num == "best") {
                        rank = 1
                } else if (num == "worst") {
                        rank = nrow(cdata)
                } else {
                        rank = num
                }
                ## add to hospital.name vector
                hospital = c(hospital, cdata[rank,1])
        }
        
        ## return the resutls
        return(data.frame(hospital, state))
        
        
}


