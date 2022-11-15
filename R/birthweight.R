
#' @title Classifying Birthweights Using Provided Centile Values
#' @description Return a list with a data frame and table containing birthweight classes.
#' @usage birthweightclass(data, centiles, GAcols, CENTcolsB, CENTcolsG, CENTcolsU, dataBW, BWscale = NULL, dataGEND, pathname)
#' @param data object of class "data.frame" with birthweight data.
#' @param centiles object of class "data.frame" with centiles data.
#' @param GAcols string. Character string with names of columns in \code{data} and \code{centiles} containing gestational age (GA) of newborn. The GA column name in \code{data} should be first in the string, followed by the GA column name in \code{centiles}.
#' @param CENTcolsB string. Character string with names of columns in \code{data} containing the nth lower centile values and nth upper centile values for male newborns. The column with nth lower centile values should be named first in the string, followed by the column with nth upper centile values.
#' @param CENTcolsG string. Character string with names of columns in \code{data} containing the nth lower centile values and nth upper centile values for female newborns. The column with nth lower centile values should be named first in the string, followed by the column with nth upper centile values.
#' @param CENTcolsU string. Character string with names of columns in \code{data} containing the nth lower centile values and nth upper centile values for unknown gender newborns. The column with nth lower centile values should be named first in the string, followed by the column with nth upper centile values.
#' @param dataBW character. Name of column in \code{data} with newborn birthweights.
#' @param BWscale character. Must be one of "grams" or "kilos", specifying the measurement scale used in \code{dataBW}. Default: NULL
#' @param dataGEND character. Name of column from \code{data} with newborn gender.
#' @param pathname character. File path to folder where the output data frame and table should be written.
#' @return A list with a data frame containing all the columns in \code{data} in addition to a column with birthweight class, as well as a table containing the birthweight class frequencies.
#' @details Appropriate conversions need to be done for birthweights not recorded in either grams or kilos. Newborn genders should be recorded as either "male" or "female" with the option of the first letter being capitalized.
#'
#' Birthweights will be classified as either Small-for-Gestational Age (SGA), Large-for-Gestational Age (LGA) or Appropriate-for-Gestational Age (AGA) based on the gender-specific centile values provided in \code{centiles}.
#' @author Bancy Ngatia
#' @examples
#' \dontrun{
#' if(interactive()){
#' # Obtain birthweight classes
#' bwclasses.list <- birthweightclass(data = data,
#' centiles = centiles, GAcols = c("GA column in data",
#'  "GA column in centiles"), CENTcolsB = c("nth lower centile male",
#'   "nth upper centile male"), CENTcolsG = c("nth lower centile female",
#'   "nth upper centile female"),
#'    CENTcolsU = c("nth lower centile unknown gender",
#'   "nth upper centile unknown gender"),
#'    dataBW = "birthweight", BWscale = "grams",
#'    dataGEND = "newborn gender", pathname = "output path name")
#'
#' # Extract data set with new column indicating birthweight class
#' bwclasses.dat <- bwclasses.list[[1]]
#'
#' # Extract table with birthweight class frequencies
#' bwclasses.tab <- bwclasses.list[[2]]
#'  }
#' }
#' @seealso
#'  \code{\link[dplyr]{mutate}}
#' @rdname birthweight
#' @export
#' @importFrom dplyr mutate

birthweightclass <- function(data, centiles, GAcols, CENTcolsB, CENTcolsG, CENTcolsU, dataBW, BWscale = NULL, dataGEND, pathname)
{
  #
  dataGA <- data[ , GAcols[1]]
  centilesGA <- centiles[ , GAcols[2]]
  centiles$centilesGA <- centilesGA
  #
  data$datacentGA <- ""
  for(i in 1:nrow(data)){
    data$datacentGA[i] <- centiles[which.min(abs(dataGA[i] - centilesGA)), GAcols[2]]
  }
  data$datacentGA <- as.numeric(data$datacentGA)
  #
  data$BWkg <- ""
  if(is.null(BWscale)){
    stop("Please input either BWscale = 'grams' or BWscale = 'kilos'")
  }
  if(BWscale == "grams"){
    data$BWkg <- data[ , dataBW] / 1000
  }
  else if(BWscale == "kilos"){
    data$BWkg <- data[ , dataBW]
  }
  #
  data$dataGEND <- data[ , dataGEND]
  datamales.df <- droplevels(data[grep("^[Mm]ale", data$dataGEND), ])
  datafemales.df <- droplevels(data[grep("^[Ff]emale", data$dataGEND), ])
  dataunknowns.df <- droplevels(data[data$dataGEND %in% c("", NA), ])
  #
  datamales.df$cent10B <- centiles[ , CENTcolsB[1]][match(datamales.df$datacentGA, centiles$centilesGA)]
  datamales.df$cent90B <- centiles[ , CENTcolsB[2]][match(datamales.df$datacentGA, centiles$centilesGA)]
  datamales.df <- dplyr::mutate(datamales.df, BWclass = ifelse(BWkg < cent10B, "SGA", ifelse(BWkg > cent90B, "LGA", ifelse(BWkg >= cent10B & BWkg < cent90B, "AGA", ifelse(BWkg == cent90B, "AGA", NA_real_)))))
  datamales.df2 <- droplevels(subset(datamales.df, select = -c(BWkg, datacentGA, datacentGA, dataGEND, cent10B, cent90B)))
  #
  datafemales.df$cent10G <- centiles[ , CENTcolsG[1]][match(datafemales.df$datacentGA, centiles$centilesGA)]
  datafemales.df$cent90G <- centiles[ , CENTcolsG[2]][match(datafemales.df$datacentGA, centiles$centilesGA)]
  datafemales.df <- dplyr::mutate(datafemales.df, BWclass = ifelse(BWkg < cent10G, "SGA", ifelse(BWkg > cent90G, "LGA", ifelse(BWkg >= cent10G & BWkg < cent90G, "AGA", ifelse(BWkg == cent90G, "AGA", NA_real_)))))
  datafemales.df2 <- droplevels(subset(datafemales.df, select = -c(BWkg, datacentGA, datacentGA, dataGEND, cent10G, cent90G)))
  #
  dataunknowns.df$cent10U <- centiles[ , CENTcolsU[1]][match(dataunknowns.df$datacentGA, centiles$centilesGA)]
  dataunknowns.df$cent90U <- centiles[ , CENTcolsU[2]][match(dataunknowns.df$datacentGA, centiles$centilesGA)]
  dataunknowns.df <- dplyr::mutate(dataunknowns.df, BWclass = ifelse(BWkg < cent10U, "SGA", ifelse(BWkg > cent90U, "LGA", ifelse(BWkg >= cent10U & BWkg < cent90U, "AGA", ifelse(BWkg == cent90U, "AGA", NA_real_)))))
  dataunknowns.df2 <- droplevels(subset(dataunknowns.df, select = -c(BWkg, datacentGA, datacentGA, dataGEND, cent10U, cent90U)))
  #
  BWclass.df <- rbind(datamales.df2, datafemales.df2, dataunknowns.df2)
  row.names(BWclass.df) <- NULL
  utils::write.csv(BWclass.df, paste(pathname, "/dataBWclass.csv", sep = ""), row.names = FALSE)
  #
  BWclass.table <- data.frame(table(BWclass.df$BWclass))
  BWclass.table$Percent <- round(proportions(BWclass.table$Freq), 3) * 100
  names(BWclass.table) <- c("BW Class", "n", "%")
  utils::write.csv(BWclass.table, paste(pathname, "/tableBWclass.csv", sep = ""), row.names = FALSE)
  #
  x <- list(BWclass.df, BWclass.table)
}
