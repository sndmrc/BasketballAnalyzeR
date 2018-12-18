#' Manipulate play-by-play (PbP) dataset
#'
#' @param data PbP dataframe
#' @return A Pbp data frame
#' @export
#' @importFrom stringr str_sub
#' @importFrom operators %~%
#' @importFrom operators %!~%

PbPmanipulation <- function(data) {
  #### Convert shot distance and x-y coordinates to numeric
  num_vars <- c("shot_distance","original_x","original_y","converted_x","converted_y")
  data[,num_vars] <- sapply(data[,num_vars], function(x) suppressWarnings(as.numeric(as.character(x))))

  #### Drop empty levels from factors
  fact_vars <- sapply(data, function(x) is.factor(x))
  data[,fact_vars] <- lapply(data[,fact_vars], function(x) droplevels(x))

  #### Extract minutes and seconds and calculate the total time played
  Minutes <- as.numeric(stringr::str_sub(data$remaining_time,-5,-4))
  Seconds <- as.numeric(stringr::str_sub(data$remaining_time,-2,-1))
  period.length <- 12
  data$periodTime  = period.length*60 - (Minutes*60 + Seconds)
  data$totalTime = data$periodTime + period.length*60*(data$period-1)

  #### Add play length
  data$playlength <- as.numeric(stringr::str_sub(data$play_length,-2,-1))

  #### Add shot type
  filt <- (data$result!="")
  mat <- data[filt,]
  mat$ShotType <- ifelse(mat$event_type!="free throw" & mat$description%~%"3PT","3P",
                  ifelse(mat$event_type!="free throw" & mat$description%!~%"3PT","2P","FT"))
  data$ShotType[filt] <- mat$ShotType
  data$ShotType <- as.factor(data$ShotType)

  return(data)
}
