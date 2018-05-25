error_category <- function(data, category) {
    if (data == "COE") {
        if (any(!(category %in% c("Products" ,"Construction","Consumer","Energy"
                                  ,"Finance" ,"Manufacturing","Other")))){
            writeLines("Error. Choose a valid option for Cost of Equity category:")
            writeLines("Products\tConstruction\tConsumer\tEnergy")
            writeLines("Finance\tManufacturing\tOther")
            return(TRUE)
        }
    }

    if (data == "dividend_yield" & any(!(category %in% "unique"))) {
        writeLines("Error. Choose a valid option for Dividend Yield category:")
        writeLines("unique (default)")
        return(TRUE)
    }

    if (data == "illiquidity_index" & any(!(category %in% "unique"))) {
        writeLines("Error. Choose a valid option for Illiquidity Index category:")
        writeLines("unique (default)")
        return(TRUE)
    }

    if (data == "loan_fees" & any(!(category %in% "unique"))) {
        writeLines("Error. Choose a valid option for Loan Fees category:")
        writeLines("unique (default)")
        return(TRUE)
    }

    if (data == "short_interest" & any(!(category %in% "unique"))) {
        writeLines("Error. Choose a valid option for Short Interest category:")
        writeLines("unique (default)")
        return(TRUE)
    }

    if (data == "portfolios") {
        if (any(!(category %in% c("size" ,"momentum","btm","illiquidity"
                                  ,"size_btm" ,"size_momentum","size_illiquidity", "industry")))) {
            writeLines("Error. Choose a valid option for Portfolios category:")
            writeLines("size\tmomentum\tbtm\tilliquidity\tsize_btm")
            writeLines("size_momentum\tsize_illiquidity\tindustry")
            return(TRUE)
        }
    }

    if (data == "volatility_index") {
        if (any(!(category %in% c("IVolBR" ,"variance_premium","risk_aversion")))) {
            writeLines("Error. Choose a valid option for Volatility Index category:")
            writeLines("IVolBR\tvariance_premium\trisk_aversion")
            return(TRUE)
        }
    }

    any(!(category %in% c("MKT" ,"SMB","HML", "WML", "IML", "Risk_Free")))

    if (data == "risk_factors") {
        if (any(!(category %in% c("Rm_minus_Rf" ,"SMB","HML", "WML", "IML", "Risk_free")))) {
            writeLines("Error. Choose a valid option for Risk Factors category:")
            writeLines("MKT\tSMB\tHML\bWML")
            writeLines("IML\tRisk_Free")
            return(TRUE)
        }
    }
    return(FALSE)
}
