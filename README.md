# AvenueAPI

## Overview

Much like our [Python](https://www.python.org/) "CLI", 7Park provides an [R](https://www.r-project.org/) package with several convenience functions that allow you to quickly analyze data from our REST API. In the examples below, we show you how to install the Avenue R package ("AvenueAPI") and reference it's help system. The AvenueAPI package is updated frequently, and we recommend repeating this installation procedure on a regular basis in order to get the latest package functionality. We also recommend installing the AvenueAPI client on a recent (<3.4) [R release](https://cran.r-project.org/).  

<br>

## Part 1: Install and Load devtools
Currently, the AvenueAPI package can be installed via Github using Hadley Wickham's excellent [devtools package](https://github.com/hadley/devtools) (a CRAN release is also planned). If you do not have `devtools` installed, it can be retrieved from CRAN and loaded into your working session with the following commands:

```
install.packages("devtools") # only necessary if devtools is not currently installed
library(devtools)
```

Devtools has a number of package dependencies, and occasionally you will need to install additional system software for it to load correctly. On MacOS Sierra, for example, you may need to install `openssl` prior to installing devtools. A quick workaround for this issue is to use the [Brew package manager](https://brew.sh/). From the Mac Terminal, running the following commands should allow proper devtools installation if Brew is instaled on your workstation:

```
# From the Mac Terminal
brew install openssl
mkdir -p /tmp/git2rpackage && cd /tmp/git2rpackage
git clone https://github.com/ropensci/git2r.git .
R CMD INSTALL git2r
```

<br>

## Part 2: Install the AvenueAPI Package

After sucessfully installing devtools and loading the library into your working environment, you are ready to install the AvenueAPI client. The following command will retrieve the package from 7Park's Github repository and install additional package dependencies automatically:

```{r, eval=FALSE}

install_github("sevenpark/AvenueAPI-R")

library(AvenueAPI)
```


<br>

## Part 3: Setting the Connection and Using the Help System 
AvenueAPI is a standard R package. All user-visible functions are documented and can be accessed in the normal way, e.g.:

```
# View all help pages for the AvenueAPI package:
 ??AvenueAPI

# Get help with a specific function:
?connect_avenue
```

Following the manual entry for `connect_avenue`, we can now create a credential object with the following command:

```
aveconnection <- connect_avenue(api_key = 'YOUR_API_KEY_HERE')
```

This function performs light validation on the key itself and, if successful, creates an S4 object of class "AvenueAPI" that can be passed to subsequent calls. For example, to retrieve data on Chipotle we might pass the 'aveconnection' object created above to the `fetch_revenue_series` function:

```
chip <- fetch_revenue_series(aveconnection, firm="Chipotle", data_source="cc", start_date='2015-01-01')
```

This function call, in turn, creates a nested list object ("chip" in this example) which contains revenue and volume data, as well as information about the API call. Please see our API Guides for additional examples of how to work with the Avenue API, and contact your sales representative for more information about obtaining a valid API key.
