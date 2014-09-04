rankall <- function(outcome, num = "best") {
    ## Read outcome data
    outcome.data <- read.csv("outcome-of-care-measures.csv", na.strings = c("", "Not Available"))
    
    outcome.data$Address.2 <- NULL
    outcome.data$Address.3 <- NULL
    
    outcome.mortality <- outcome.data[,1:26]
    outcome.readmission <- outcome.data[,c(1:8, 27:44)]
    outcome.mortality$Type <- factor("Mortality")
    outcome.readmission$Type <- factor("Readmission")
    names(outcome.mortality) <- gsub("Mortality..", "", names(outcome.mortality))
    names(outcome.readmission) <- names(outcome.mortality)
    outcome.data <- rbind(outcome.mortality, outcome.readmission)
    rm(outcome.mortality, outcome.readmission)
    
    outcome.heart.attack <- outcome.data[,c(1:14, 27)]
    outcome.heart.attack$Outcome <- factor("heart attack")
    outcome.heart.failure <- outcome.data[,c(1:8, 15:20, 27)]
    outcome.heart.failure$Outcome <- factor("heart failure")
    outcome.pneumonia <- outcome.data[,c(1:8, 21:27)]
    outcome.pneumonia$Outcome <- factor("pneumonia")
    names(outcome.heart.attack) <- gsub("..from.Heart.Attack", "", names(outcome.heart.attack))
    names(outcome.heart.failure) <- names(outcome.heart.attack)
    names(outcome.pneumonia) <- names(outcome.heart.attack)
    outcome.data <- rbind(outcome.heart.attack, outcome.heart.failure, outcome.pneumonia)
    rm(outcome.heart.attack, outcome.heart.failure, outcome.pneumonia)
    
    ## Check that state and outcome are valid
    if (!(outcome %in% outcome.data$Outcome)) {
        stop('invalid outcome')
    }
    
    ## For each state, find the hospital of the given rank
    
    outcome.subset <- outcome.data[outcome.data$Type=="Mortality" & outcome.data$Outcome==outcome, c("Hospital.Name", "State", "Hospital.30.Day.Death..Rate")]
    
    outcome.output <- by(outcome.subset, outcome.subset$State, function(outcome.by.state, num){
        
        outcome.by.state <- outcome.by.state[order(
            outcome.by.state$Hospital.30.Day.Death..Rate,
            outcome.by.state$Hospital.Name,
            na.last = NA
            )]
        
        if(num == "best"){
            num <- 1
        } else if (num == "worst") {
            num <- nrows(outcome.by.state)
        }
        
        return(outcome.by.state)
        
    }, num)
    
    ## Return a data frame with the hospital names and the
    ## (abbreviated) state name
    return(outcome.output)
}
