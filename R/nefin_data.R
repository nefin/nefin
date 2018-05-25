#' nefin_data
#'
#' nefin_data returns a DataFrame with financial data from NEFIN website.
#' @param data a string containing the type of data to download. Available options: Cost of Equity ("COE"), Dividend Yield ("dividend_yield"),
#' Illiquidity Index ("illiquidity_index"), Loan Fees ("loan_fees"), Portfolios ("portfolios"), Risk Factors ("risk_factors"),
#' Short Interest ("short_interest"), Volatility Index ("volatility_index").
#' @param category a string (or a vector of string) containing the category of data to download. Only necessary for Cost of Equity,
#' Porfolios, Risk Factors and Volatility Index data type. See 'Details'.
#' @param portfolio_weight a string. Choose "Equally" (default option) for Equally Weighted Returns and "Value" for Value Weighted Returns
#' @param frequency an optional string. Choose "month" for monthly data and "year" for annual data.
#' Only available for Cost of Equity, Risk Factors, Dividend Yield, Illiquidity Index, Loan Fees and Short Interest. See 'Details'.
#'
#' @details If you choose Cost of Equity, Portfolios, Risk Factors or Volatility Index in category,
#' you need to choice one or more available options for category. If you use the default option,
#' the function will download all available options.
#' Use a string for one option and a vector of string for more than one option. \cr
#' \emph{Cost of Equity}: Basic Products ("Products"), Construction ("Construction"), Consumer ("Consumer"), Energy ("Energy"),
#' Finance("Finance"), Manufacturing ("Manufacturin"), Other ("Other").
#' \cr See details of each category \href{http://www.nefin.com.br/cost_of_capital.html}{here} \cr
#'
#' \cr \emph{Portfolios}: 3 portfolios sorted by size ("size"), 3 portfolios sorted by book-to-market("btm"),
#' 3 portfolios sorted by momentum ("momentum"), 3 portfolios sorted by illiquidity ("illiquidity"),
#' 4 portfolios sorted by size and by book-to-market ("size_btm"), 4 portfolios sorted by size and by momentum ("size_momentum"),
#' 4 portfolios sorted by size and illiquidity ("size_illiquidity")
#' 7 portfolios sorted by industry ("industry").
#' \cr See more details \href{http://www.nefin.com.br/portfolios.html}{here} \cr
#'
#' \cr \emph{Risk Factors}: Market Factor ("Rm_minus_Rf"), Small Minus Big ("SMB"), High Minus Low ("HML"),
#' Winners Minus Losers ("WML"), Illiquid Minus Liquid ("IML"), Risk Free ("Risk_free").
#' \cr See details \href{http://www.nefin.com.br/risk_factors.html}{here}. \cr
#'
#' \cr \emph{Volatility Index}: IVol-Br Volatility Index ("IVolBR"), Variance Premium ("variance_premium"), Risk Aversion ("risk_aversion")
#'
#' \cr \cr If you choose Portfolios in category, you also need to choose an option for 'portfolio_weigth".
#' The default option is "Equally" for Equally Weighted Returns and the other option is "Value" for Value Weighted Returns.
#'
#' \cr \cr For frequency, the default option are:
#' \cr Cost of Equity: monthly
#' \cr Dividend Yield: weekly
#' \cr Illiquidity Index: monthly
#' \cr Loan Fees: monthly
#' \cr Portfolios: daily
#' \cr Risk Factors: daily
#' \cr Short Interest: weekly
#' \cr Volatility Index: daily for IVol-Br and weekly for Variance Premium
#' \cr \cr For Cost of Equity, Dividend Yield, Illiquidity Index, Loan Fees, Portfolios and Short Interest,
#' you can choose "year" for annual data or "month" for monthly data.
#'
#' \cr \cr \cr For information about the methodology used, click \href{http://www.nefin.com.br/Metodologia/Methodology.pdf}{here}.
#'
#' @return A DataFrame containing the selected data
#' @export
#'
#' @examples
#' dataframe <- nefin_data(data = "portfolios", category = "size", portfolio_weigth = "Value", frequency = "year")
#' dataframe <- nefin_data(data = "COE", category = "Products")
#' dataframe <- nefin_data(data = "illiquidity_index")
#' dataframe <- nefin_data(data = "loan_fees")
#' dataframe <- nefin_data(data = "dividend_yield")
#' dataframe <- nefin_data(data = "short_interest")
#' dataframe <- nefin_data(data = "risk_factors", category = "Rm_minus_Rf")
nefin_data <- function(data
                       , category = "unique"
                       , portfolio_weight = "Equally"
                       , frequency = "") {
    require(readxl)
    require(xts)
    require(curl)

    if (error_data(data)) {
        return(NULL)
    }

    if (length(category) == 1) {
        if (category == "unique") {
            if (data == "COE") {
                category <- c("Products", "Construction", "Consumer", "Energy", "Finance",
                              "Manufacturing", "Other")
            } else if (data == "portfolios") {
                category <- c("size", "btm", "momentum", "illiquidity", "size_btm", "size_momentum",
                              "size_illiquidity", "industry")
            } else if (data == "risk_factors") {
                category <- c("Rm_minus_Rf", "SMB", "HML", "WML", "IML", "Risk_free")
            } else if (data == "volatility_index") {
                category <- c("IVolBR", "variance_premium", "risk_aversion")
            }
        }
    }

    if (error_category(data, category)) {
        return(NULL)
    }

    if(error_frequency(frequency)) {
        return(NULL)
    }

    data_set <- lapply(category , function(x){
        site_data(data, x, portfolio_weight)
    })

    data_set = do.call(cbind,data_set)

    if (frequency == "month" | frequency == "year") {
        if (data == "portfolios" | data == "risk_factors") {
            data_set <- frequency_convert(data_set, frequency, data, FUN = prod2)
        } else if (data == "dividend_yield" | data == "COE") {
            data_set <- frequency_convert(data_set, frequency, data, FUN = tail2)
        } else if (data == "illiquidity_index" | data == "loan_fees" | data == "short_interest") {
            data_set <- frequency_convert(data_set, frequency, data, FUN=mean)
        }
    }

    return(data_set)
}
