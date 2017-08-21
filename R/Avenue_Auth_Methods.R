################################################################################
####                     Avenue AUTHORIZATION METHODS                    #####
################################################################################

################################################################################
# Validity Functions


#' Function to check the validity of an Avenue API key
#' @param object character; typically, this function is called to validate the api_key slot of an object of class AvenueApiClient. It may also be used to examine a length 1 character vector.
#' @return An un-altered api key or, in the case of warnings, message(s) collected from the evaluation process.
check_api_key <- function(object) {
    errors <- character()
    length_api <- length(object@api_key)
    nchar_api  <- nchar(object@api_key)


    if(length_api != 1) {
        msg <- paste("API key is length ", length_api, ". Should be 1!", sep="")
        errors <- c(errors, msg)
    }

    if(nchar_api < 35 ) {
        msg <- paste("API key is malformed, please check that you are passing the correct key", sep="")
        errors <- c(errors, msg)
    }

    class_api <- class(object@api_key)
    if(class_api != "character") {
        msg <- paste("API key is class '", class_api, "'. Should be 'character'!", sep="")
        errors <- c(errors, msg)
    }

    return(if( length(errors) == 0 ) TRUE else errors)
}

#' Function to check the validity of date inputs
#' @param x Date; a length 1 Date or a string expressed in a standard unambiguous format. Descriptive errors are produced for numeric inputs or invalid string formats.
#' @param future_warning logical; When \code{TRUE}, a warning message is produced if the supplied date is greater than current date + 1 day.
#' @return When no errors are triggered, check_date returns a valid Date object equal to the input.
#' @examples
#' \dontrun{
#' check_date(3)
#' Error in check_date(3) :
#' '3' is an invalid date selection of class 'numeric'.
#' You must supply a valid date object or a character of form 'YYYY-MM-DD'!
#' }
#' @export
check_date <- function(x, future_warning=FALSE) {

    name=deparse(substitute(x))
    xclass <- class(x)
    length_date <- length(x)

    if(length_date != 1) {
        msg <- paste("'", name, "' is length ", length_date, ", but must be length 1!", sep="")
        stop(msg)
    }

    # This will prevent numeric inputs, which will generally be coercible to Date.
    if(!xclass %in% c("character", "Date")) {
        msg <- paste("'", name, "' is an invalid date selection of class '", xclass, "'. \nYou must supply a valid date object or a character of form 'YYYY-MM-DD'!", sep="")
        stop(msg)
    }

    test <- try(as.Date(x), silent=TRUE)
    if( class( test ) == "try-error" || is.na( test ) )
        stop( "Invalid Date format. You must supply a valid Date-class object or a string coercible to Date-class!" )

    if( test > Sys.Date() +1 ) {
        msg <- paste("The supplied date for the '", name, "' parameter is in the future.\nData may not be available in this range.", sep="")
        warning(msg)
    }

    return(as.character(test))
}

#' Function to check the validity of an Avenue Revenue series source input
#' @param x character; a length 1 character vector and one of "cc" (credit card panel), "m1" (Merchant Intel data), or "m2" (Merchant Intel2 data).
#' @return When no errors are triggered, check_rev_source returns an appropriate dataset name which is passed to the Avenue API.
#' @export
check_rev_source <- function(x) {

    class_check  <- class(x)
    length_check <- length(x)
    name         <- deparse(substitute(x))

    if( class_check != "character" ) {
        msg <- paste("'", name, "' is an invalid selection of class '", class_check, "'. \nYou must supply a length 1 character vector!", sep="")
        stop(msg)
    }

    if( length_check !=1 ) {
        msg <- paste("'", name, "' is length ", length_check, ", but must be length 1!", sep="")
        stop(msg)
    }

    if(!x %in% c("cc", "m1", "m2")) {
        msg <- paste(name, 'is an invalid source selection! The source parameter must be a length 1 character vector and one of: "cc", "m1", "m2"')
        stop(msg)
    }

    return(x)
}


#' Check that a specificied company name is contained within the 7Parkdata Avenue API for a given revenue data source
#' @param x character; a company name
#' @param data_source character; a data source name, one of m1, m2, or cc
#' @param validate_name logical; when \code{TRUE} (the default) \code{TRUE} is tested against a vector of known/valid names.
#' @importFrom methods validObject
#' @export
check_rev_firm <- function(x, data_source='', validate_name=NULL) {

    class_check  <- class(x)
    length_check <- length(x)
    name         <- deparse(substitute(x))

    if( data_source=="cc" & validate_name==TRUE ) {
        eval_set <- get(data("cc_names", envir = parent.frame()))
    } else if(data_source=="m1" & validate_name==TRUE ) {
        eval_set <- get(data("m1_names", envir = parent.frame()))
    } else if(data_source=="m2" & validate_name==TRUE ) {
        eval_set <- get(data("m1_names", envir = parent.frame()))
    }

    if( class_check != "character" ) {
        msg <- paste("'", name, "' is an invalid selection of class '", class_check, "'. \nYou must supply a length 1 character vector!", sep="")
        stop(msg)
    }

    if( length_check !=1 ) {
        msg <- paste("'", name, "' is length ", length_check, ", but must be length 1!", sep="")
        stop(msg)
    }

    if( validate_name ) {
        if( !any(x %in% eval_set) ) {
            msg <- paste('Invalid firm selection! Please review the Avenue documentation and select a valid firm name; you may also set  validate_name=FALSE to ignore this check.')
            stop(msg)
        }
    }

    return(x)
}


#' Function to check the validity of an Avenue traffic domain
#' @param x character; a length 1 character vector of the domain you are requesting, e.g. 'google.com'
#' @return When no errors are triggered, check_dom_source returns an appropriate domain name which is passed to the Avenue API.
#' @export
check_dom_source <- function(x) {

    class_check  <- class(x)
    length_check <- length(x)
    name         <- deparse(substitute(x))

    if( class_check != "character" ) {
        msg <- paste("'", name, "' is an invalid selection of class '", class_check, "'. \nYou must supply a length 1 character vector!", sep="")
        stop(msg)
    }

    if( length_check !=1 ) {
        msg <- paste("'", name, "' is length ", length_check, ", but must be length 1!", sep="")
        stop(msg)
    }

    eval_set_dom=AvenueAPI::extension_names

    if(! any(x %in% eval_set_dom) ) {
        msg <- paste(name, 'is an invalid domain selection!',
                     'The domain parameter must be a length 1 character vector and found within the "extension_names" list.\n',
                     'Suggestion: type "extension_names" into your R console and verify that "', x, '" is a valid domain spelling/covered entity.', sep='')
        stop(msg)
    }

    return(x)
}




#' @import rjson
#' @import httr
#' @importFrom utils URLencode
#' @importFrom methods new validObject
setClass (Class = "AvenueApiClient",
          representation = representation(
              api_key="ANY",
              avenue_root_url="ANY",
              funct = "function"),
          prototype = list ( api_key="", avenue_root_url='https://avenue-api.7parkdata.com/api/',
                             funct = function (.Object) { print(.Object) } )
)


# Apply the input validation formula to AvenueApiClient class
setValidity("AvenueApiClient", check_api_key)


setMethod ('initialize', 'AvenueApiClient',
           function(.Object, api_key='', avenue_root_url='') {
               if(nargs() > 1) .Object@api_key  = api_key
               if(nargs() > 2) .Object@avenue_root_url = avenue_root_url
               return(.Object)
           })


#' Set credentials for a 7Parkdata Avenue API connection.
#' @param api_key character; a valid Avenue API key. Must be a length 1 character vector. Contact your 7Park Sales Representative to obtain your key
#' @importFrom methods validObject
#' @aliases connect_avenue
#' @examples
#' \dontrun{
#' connection <- connect_avenue(api_key=good_key)
#' }
#' @export

connect_avenue <- function(api_key='') {
    avec <- new("AvenueApiClient", api_key)
    test <- validObject(avec)
    if(test) avec else test
}


#' fetch_data Method.
#' @name fetch_data
#' @rdname fetch_data-methods
#' @exportMethod fetch_data
setGeneric("fetch_data",  function(.Object, ave_url, params="") standardGeneric("fetch_data"))


# This retrieves the generic data request in JSON format.
#' @rdname fetch_data-methods
#' @param .Object A valid AvenueApiClient connection
#' @param ave_url Fully constructed Avenue API URL
#' @param params Params passed to the API
#' @aliases fetch_data
setMethod("fetch_data",  "AvenueApiClient",
          function(.Object, ave_url, params='') {
              if ( params != "" ) ave_url = paste(ave_url, params, sep='', collapse='')
              auth_header=paste("Token", .Object@api_key, sep=' ')
              raw <- GET(url=ave_url, add_headers(Authorization=auth_header))
              out <- fromJSON( content(raw, type="text", encoding = "UTF-8") )
              test_7p_return(out)
              out[['api_call']]  <- paste(ave_url, sep='')
              class(out) <- c(class(out), "AvenueAPIReturn")
              return(out)
          })



# Basic validitity tests for the returned JSON object.
test_7p_return <- function(ret_list=NULL) {
    if( !is.null(ret_list[['detail']]) ) {
        msg <- paste("API Return Error! Server message:\n'", ret_list$detail,"'", sep="")
        stop(msg)
    }
    if( length(ret_list$data[[1]])==0 ) warning("No data returned. Check domain and date parameters!")
}



#' fetch_revenue_series Methods
#' @name fetch_revenue_series
#' @rdname fetch_revenue_series-methods
#' @exportMethod fetch_revenue_series
setGeneric("fetch_revenue_series", function(.Object, firm, data_source, start_date='', end_date='', validate_name=TRUE) standardGeneric("fetch_revenue_series"))


#' @rdname fetch_revenue_series-methods
#' @param .Object A valid AvenueApiClient connection
#' @param firm character; an length-1 vector of the company name, e.g. "Chipotle"
#' @param data_source character; the source dataset. Must be one of: 'cc' (credit card panel), 'm1' (Merchant Intel), 'm2' (Merchant Intel2)
#' @param start_date string; a length-one character vector expressed as '\code{YYYY-MM-DD}' (ISO 8601) representing the start date of series
#' @param end_date string; a length-one character vector expressed as '\code{YYYY-MM-DD}' (ISO 8601) representing the end date of series
#' @param validate_name logical; when \code{TRUE}, AvenueAPI checks the supplied firm name against a vector of known/valid names.
#' @aliases fetch_revenue_series
#' @examples
#' \dontrun{
#' # Get credit card data for Chipotle
#' chip <- fetch_revenue_series(connection,
#'         firm="Chipotle",
#'         data_source="cc",
#'         start_date='2015-01-01')
#' }

setMethod("fetch_revenue_series", "AvenueApiClient",
          function(.Object, firm=NULL, data_source=NULL, start_date='', end_date='', validate_name=TRUE) {

              if(  is.null(data_source) ) {
                  stop("You must supply a value for the data_source parameter")
              }

              if( is.null(firm) ) {
                  stop("You must supply a value for the firm parameter")
              }

              if(end_date=='') end_date <- Sys.Date()

              # Date validation
              d1 <- check_date(start_date, future_warning=TRUE)
              d2 <- check_date(end_date)

              d1 <- paste('&date_from=', start_date, sep='')
              d2 <- paste('&date_to=', end_date, sep='')

              # Recode data_source
              phase1   <- check_rev_source(data_source)
              codename <- switch(phase1,
                                 cc="Spending Intel",
                                 m1="Merchant Intel",
                                 m2="Merchant Intel2")

              dataset <- paste(
                  'app/3.0/merchants/daily?source=',
                  URLencode(codename), sep=''
              )

              # Firm validation
              firm    <- check_rev_firm(firm, data_source=phase1, validate_name=validate_name)
              firmout <- paste('&merchant=', URLencode(firm), sep='')

              ave_url <- paste(.Object@avenue_root_url, dataset, firmout, collapse=NULL, sep='')
              params  <- paste(d1, d2, sep='', collapse=NULL)

              out <- fetch_data(.Object, ave_url, params)

              out[['end_point']] <- "daily_revenue"

              # out[['api_call']]  <- paste(ave_url, params, sep='')
              # class(out) <- c(class(out), "AvenueAPIReturn")
              return(out)
          })

################################################################################
################################################################################
################################################################################


#' fetch_app_series Methods
#' @name fetch_app_series
#' @rdname fetch_app_series-methods
#' @exportMethod fetch_app_series
setGeneric("fetch_app_series", function(.Object, app, cadence, start_date='', end_date='', country_code='', region=NULL) standardGeneric("fetch_app_series"))


#' @rdname fetch_app_series-methods
#' @param .Object A valid AvenueApiClient connection
#' @param app character; an length-1 vector of the Android package name, e.g. "com.facebook.katana"
#' @param cadence character; the cadence requested. Must be one of: 'daily' (daily app usage) or 'weekly' (weekly app usage)
#' @param start_date string; a length-one character vector expressed as '\code{YYYY-MM-DD}' (ISO 8601) representing the start date of the requested series
#' @param end_date string; a length-one character vector expressed as '\code{YYYY-MM-DD}' (ISO 8601) representing the end date of the requested series
#' @param country_code character; the ISO 3166 alpha-2 country_code for which you are requesting app data. If left unset, the API returns data for all countries.
#' @param region character; the UN region code for which you are requesting app data. An error is thrown if \strong{both} \code{region} and \code{country_code} are set.
#' @examples
#' \dontrun{
#' # Get app usage data for Facebook
#' fb <- fetch_app_series(gave, app="com.facebook.katana",
#'                        cadence="daily",
#'                        start_date='2015-01-01',
#'                        country_code = 'US')
#' }
#' @aliases fetch_app_series
setMethod("fetch_app_series", "AvenueApiClient",
          function(.Object, app=NULL, cadence=NULL, start_date='', end_date='', country_code='', region=NULL) {

              if(  is.null(cadence) ) {
                  stop("You must supply a value for the cadence parameter")
              }

              if( !cadence %in% c("weekly", "daily") ) {
                  stop('Cadence must be one of: "weekly", "daily"')
              }

              if( is.null(app) ) {
                  stop("You must supply a value for the app parameter")
              }

              if( !is.null(region) & nchar(country_code) > 0 ) {
                  stop("You specified two geographic parameters.\nThe Avenue ",
                       "API can only accept one region parameter, ",
                       "country_code or region, within a single call.\n",
                       "Please choose only one geographic parameter and resubmit",
                       " your query. Process aborted.")
              }

              # Set geo parameter
              if( !is.null(region) ) {
                  loc_request <- paste('&region=', URLencode(region), sep='')
              } else if(nchar(country_code)>0) {
                  loc_request <- paste('&country_code=', URLencode(country_code), sep='')
              } else {
                  loc_request <- NULL
              }


              # Date validation
              if(end_date=='') end_date <- Sys.Date()

              d1 <- check_date(start_date, future_warning=TRUE)
              d2 <- check_date(end_date)

              d1 <- paste('&date_from=', start_date, sep='')
              d2 <- paste('&date_to=', end_date, sep='')

              # Recode metric (TODO: Add remaining metrics)
              codename <- switch(cadence,
                                 daily="appusagedaily",
                                 weekly="appusageweekly")

              # Examples, daily and weekly
              # GET https://avenue-api.7parkdata.com/api/app/3.0/analytics/appusagedaily
              # GET https://avenue-api.7parkdata.com/api/app/3.0/analytics/appusageweekly
              # https://avenue-api.7parkdata.com/api/app/3.0/analytics/appusagedaily?pkgs=com.facebook.katana


              dataset <- paste('app/3.0/analytics/', URLencode(codename), sep='')

              # Package validation
              appout <- paste('?pkgs=', URLencode(app), sep='')

              ave_url <- paste(.Object@avenue_root_url, dataset, appout, collapse=NULL, sep='')
              params  <- paste(d1, d2, loc_request, sep='', collapse=NULL)

              out <- fetch_data(.Object, ave_url, params)
              # out <- fromJSON( content(ave_return, type="text", encoding = "UTF-8") )

              out[['api_call']]  <- paste(ave_url, params, sep='')
              out[['end_point']] <- "app_usage"
              class(out) <- c(class(out), "AvenueAPIReturn")
              return(out)
          })




################################################################################
################################################################################
################################################################################


#' fetch_traffic_series Methods
#' @name fetch_traffic_series
#' @rdname fetch_traffic_series-methods
#' @exportMethod fetch_traffic_series
setGeneric("fetch_traffic_series", function(.Object, domain=NULL, platform='', dataseries='', start_date='', end_date='', country_code='', validate_name=TRUE) standardGeneric("fetch_traffic_series"))


#' @rdname fetch_traffic_series-methods
#' @param .Object A valid AvenueApiClient connection
#' @param domain character; an length-1 vector of the requested domain name, e.g. "www.google.com"
#' @param platform character; the user's computing platform. Must be one of: 'PC' (desktop browser data), 'mobile' (mobile web browser data), or 'ALL' (mobile + PC).
#' @param dataseries character; currently, the only valid value for this parameter is "extension" (the default). This will be updated as new traffic series are added to the API.
#' @param start_date string; a length-one character vector expressed as '\code{YYYY-MM-DD}' (ISO 8601) representing the start date of the requested series
#' @param end_date string; a length-one character vector expressed as '\code{YYYY-MM-DD}' (ISO 8601) representing the end date of the requested series
#' @param country_code character; the ISO 3166 alpha-2 country_code for which you are requesting traffic data. If left unset, the API returns data for all countries.
#' @param validate_name logical; when \code{TRUE}, AvenueAPI checks the supplied domain name against a vector of known/valid names.
#' @examples
#' \dontrun{
#' # Get domain traffic data for google.com, US
#' goog   <- fetch_traffic_series(conection, domain = 'google.com',
#'                                platform = 'PC',
#'                                start_date='2014-01-01',
#'                                country_code = 'US',
#'                                validate_name = TRUE)
#' }
#' @aliases fetch_traffic_series
setMethod("fetch_traffic_series", "AvenueApiClient",
          function(.Object, domain=NULL, platform='PC', dataseries='extension', start_date='', end_date='', country_code='US', validate_name=TRUE) {


              # Domain validation
              if(  is.null(domain) ) {
                  stop("You must supply a value for the cadence parameter")
              }

              if(  length(domain)!=1 ) {
                  stop("You must supply only one value for the domain parameter")
              }

              if(validate_name) domain <- check_dom_source(domain)
              domout <- paste('?domains=', domain, sep='')


              # Platform validation
              if( is.null(platform) ) stop('The platform parameter must be set and one of: "PC", "MOBILE", or "ALL"')
              if( !platform %in% c("PC", "MOBILE", "ALL") ) {
                  stop('The platform parameter must be one of: "PC", "MOBILE", or "ALL"')
              }
              platname <- paste('&platform=', platform, sep='')

              # Country code validation
              msg <- "Country code must be either an empty string or a valid ISO 3166 alpha-2 country_code. Process aborted."
              if( is.null(country_code) ) {
                  stop(msg)
              }
              if( !nchar(country_code) %in% c(0,2) ) {
                  stop(msg)
              }

              # Now set the country_code parameter
              if( nchar(country_code) == 2 ) {
                  loc_request <- paste('&country_code=', URLencode(country_code), sep='')
              } else {
                  loc_request <- 'US'
              }

              # Date validation
              if(end_date=='') end_date <- Sys.Date()

              d1 <- check_date(start_date, future_warning=TRUE)
              d2 <- check_date(end_date)

              d1 <- paste('&date_from=', start_date, sep='')
              d2 <- paste('&date_to=', end_date, sep='')


              # Set the endpoint
              if( is.null(dataseries) ) stop('Please select dataseries="extension" or leave this parameter empty.')
              if( dataseries != 'extension' ) {
                  stop('The "extension" series is the only traffic dataset currently supported by this endpoint.\n',
                       'Please select dataseries="extension" or leave this parameter empty.')
              }
              codename <- switch(dataseries,
                                 extension="domainusagedaily")


              # Examples,
              # avenue_root_url='https://avenue-api.7parkdata.com/api/',
              #                  https://avenue-api.7parkdata.com/api/traffic/1.0/analytics/domainusagedaily

              dataset <- paste('traffic/1.0/analytics/', URLencode(codename), sep='')

              # Construct the URL
              ave_url <- paste(.Object@avenue_root_url, dataset, domout, collapse=NULL, sep='')
              params  <- paste(d1, d2, platname, loc_request, sep='', collapse=NULL)

              out <- fetch_data(.Object, ave_url, params)
              #out <- fromJSON( content(ave_return, type="text", encoding = "UTF-8") )

              # Recode to R-friendly names
              names(out$data) <- c("visitors_unique","visitors_total")

              out[['end_point']] <- dataseries

              return(out)
          })



################################################################################
################################################################################
################################################################################
# Returned Object Transformations

#' Helper function to transform an AvenueAPI data object from list to data.frame format
#' @param metrics character; the metric names of the supplied object
#' @param data list; a data set extracted from the .$data slot of an object of class "AvenueAPI'
#' @return a "long" data.frame of the requested metrics
extract_metrics <- function(metrics=metrics, data=NULL) {
    # Initialize the data object
    mvalues <- data
    df <- NULL
    for( i in seq_along(metrics) ) {
        metric  <- metrics[i][1]

        # Dates should not contain NULL values; precautionary test:
        a1 <- a2 <- lapply(mvalues[[i]][[1]], `[[`, 1)
        a2[sapply(a2, is.null)] <- NA
        testleg <- identical(a2, a2)

        if( !testleg ) {
            msg <- paste("NULL Date values detected for ", metric,
                         "!\n Suggestion: Inspect the returned data frame.", sep="")
            warning(msg)
        }

        date <- as.Date( unlist( a2 ) )

        # Data outages, historically, may yield NULLs for empty slots within a
        # series, which will yield dropped rows and mismatched vectors.
        # This recodes NULL to NA to ensure proportional vector length.
        b1 <- b2 <- lapply(mvalues[[i]][[1]], `[[`, 2)
        b2[sapply(b2, is.null)] <- NA
        testleg <- identical(b1,b2)
        if( !testleg ) {
            msg <- paste("NOTE: NULL data values detected for ", metric,
                         "!\n Suggestion: Inspect the returned data frame.", sep="")
            warning(msg)
        }
        # Coerce the response to char.
        data <- as.character( unlist( b2 ) )
        rm(b1, b2)

        tmp <- data.frame(date=as.Date(date),
                          value=as.numeric(data),
                          metric=as.character(metric),
                          stringsAsFactors = FALSE)
        df  <- rbind(df, tmp)

    }
    return(df)
}


#' Transform a 7Parkdata Avenue API return into a "long" or "wide" dataframe for anaysis.
#' @param x list; a 7Park Data revenue object pulled with \code{fetch_*_series} method.
#' @param wide logical; if \code{TRUE}, a "wide" data frame is returned using \code{\link{dcast}}
#' @importFrom reshape2 dcast
#' @export
transform_avenue_series <- function(x, wide=FALSE) {

    if( !any(class(x) %in% c("AvenueAPIReturn")) ) {
        name=deparse(substitute(x))
        msg <- paste("'", name, "' is not an object of class 'AvenueAPIReturn'! This function requires an object created by one of the avenue_*_fetch methods!", sep="")
        stop(msg)
    }

    if( !any( x$end_point %in% c("extension", "app_usage", "daily_revenue") ) ) {
        stop("Curently, this function only supports data objects created by:\n(1) fetch_revenue_series \n(2) fetch_app_series \nor (3) fetch_traffic_series")
    }

    # These list-slots are generally available in all Avenue return formats currently supported.
    metrics  <- names(x$data)
    mvalues  <- x$data

    df <- extract_metrics(metrics=metrics, data=mvalues)

    if ( wide ) df <- dcast(data=df, formula=date~metric)

    # Set revenue call attributes:
    if( x$end_point=='daily_revenue' ) {
        firmname <- x$merchant
        # Set call attributes:
        df[['firm']] <- firmname
    }

    # Set app data call attributes:
    if( x$end_point=='app_usage' ) {
        df[['app']] <- x$pkgs

        df[['region']]       <- x$region
        df[['country_code']] <- x$country_code

        if( is.null(df[['country_code']]) ) {
            df[['country_code']] <- 'none requested'
        }

        if( is.null(df[['region']]) ) {
            df[['region']] <- 'none requested'
        }
    }

    # Set exentension series data call attributes:
    if( x$end_point=='extension' ) {
        df[['domain']] <- x$domains

        if( !is.null( x$companies[[1]]$name )) {
            df[['company']] <- df[[ as.character(x$companies[[1]]$name) ]]
        }else {
            df[['company']] <- 'Other'
        }

        df[['platform']] <- as.character(x$platform)

        df[['country_code']] <- x$country_code

        if( is.null(df[['country_code']]) ) {
            df[['country_code']] <- 'none requested'
        }
    }

    return(df)
}





