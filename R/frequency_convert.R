frequency_convert <- function(dt, frequency, data , FUN=prod2) {
    if (frequency == "month") {
        for (i in 1:ncol(dt)) {
            if (i == 1) {
                col <- apply.monthly(dt[, i], FUN) 
            } else {
                col <- cbind(col, apply.monthly(dt[, i] , FUN) )
                
            }
        }
    } else {
        for (i in 1:ncol(dt)) {
            if (i == 1) {
                col <- apply.yearly(dt[, i] , FUN) 
            } else {
                col <- cbind(col, apply.yearly(dt[, i] , FUN) )
            }
        }
    } 
    
    
    return(col)
}
