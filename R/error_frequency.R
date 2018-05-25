error_frequency <- function(frequency) {
    if (frequency != "" & frequency != "year" & frequency != "month") {
        writeLines("Error. Choose a valid option for frequency:")
        writeLines("'year' for yearly data")
        writeLines("'month' for monthly data")
        return(TRUE)
    }
    
    return(FALSE)
}