\name{plotWorldBank}
\alias{plotWorldBank}
\title{plotWorldBank}
\description{This package plots a selected World Bank time series over a selected time period
as a bar chart.}
\usage{For quick and dirty exploratory data visualisation of World Bank data}
\arguments{
\item{i:}{country (country codes can be found here: http://api.worldbank.org/v2/country/)}
\item{ii:}{indicator (indicators can be found here: https://data.worldbank.org/indicator?tab=all)}
\item{iii:}{start_year}
\item{iv:}{end_year}
\item{v:}{color}}
\author{Callan Windsor: callan.windsor@barcelonagse.eu}
\examples{
plotWorldBank(country='US', indicator='NY.GDP.MKTP.CD', start_year='2002', end_year='2016', color='blue')
plotWorldBank(country='US', indicator='GC.DOD.TOTL.GD.ZS', start_year='2010', end_year='2016', color='tomato3')
}
