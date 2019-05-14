#' @title Produces a bar chart of a World Bank time series
#'
#' @description This package produces a bar chart, plotting a selected World Bank
#' time series over a selected time period
#'
#' @param (1) country (country codes can be found here: http://api.worldbank.org/v2/country/);
#'        (2) indicator (indicators can be found here: https://data.worldbank.org/indicator?tab=all)
#'        (3) start_year
#'        (4) end_year
#'        (5) color
#'
#' @return NONE
#'
#' @examples (1) plotWorldBank(country='US', indicator='NY.GDP.MKTP.CD', start_year='2002', end_year='2016', color='tomato3')
#'           (2) plotWorldBank(country='US', indicator='GC.DOD.TOTL.GD.ZS', start_year='2010', end_year='2016', color='tomato3')

plotWorldBank = function (country,
                          indicator,
                          start_year,
                          end_year,
                          color){

  if(!require(XML))install.packages("XML"); library(XML)
  if(!require(ggplot2))install.packages("ggplot2"); library(ggplot2)

  # Construct the URL
  url = paste('http://api.worldbank.org/v2/countries/',
              country,
              '/indicators/',
              indicator,
              '?date=',
              start_year,
              ':',
              end_year,
              sep='')

  # Print URL for reference
  print(url)

  # Parse the XML
  doc = xmlTreeParse(url, useInternal = TRUE)

  # Extract the relevant values
  indicator = xmlValue(getNodeSet(doc, "//wb:indicator")[[1]])
  countryName = xmlValue(getNodeSet(doc, "//wb:country ")[[1]])
  values = sapply(getNodeSet(doc, "//wb:value") , function(el) xmlValue(el))
  dates = sapply(getNodeSet(doc, "//wb:date") , function(el) xmlValue(el))
  names(values)=dates

  # Plot the data
  theme_set(theme_bw())
  ggplot(data=data.frame(indicator,dates,rev(values)),
         aes(x=dates, y=values)) +
         geom_bar(stat="identity", width=.5, fill=color) +
         labs(title=paste(countryName),
         subtitle=paste(indicator),
         caption="source: World Bank") +
         theme(axis.text.x = element_text(angle=65, vjust=0.6)) +
         theme(axis.title.x=element_blank(),axis.title.y=element_blank())
}


