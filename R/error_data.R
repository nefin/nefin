error_data <- function(data) {
    if (data != "COE" & data != "dividend_yield"
        & data != "illiquidity_index" & data != "portfolios"
        & data != "loan_fees" & data != "risk_factors"
        & data != "short_interest" & data != "volatility_index") {
        writeLines("Error. Choose a valid option for data value:")
        writeLines("COE\tdividend_yield\tilliquidity_index\tportfolios")
        writeLines("loan_fees\trisk_factors\tshort_interest\tvolatility_index")
        return(TRUE)
    } else {
        return(FALSE)
    }
}