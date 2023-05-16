## Method is based on this paper
## https://www.sciencedirect.com/science/article/pii/S2405880716300309?via%3Dihub#s0045

## "If the temperature reaches the Spic threshold, a heat wave is detected. The
## start of the event is defined by the first day for which the temperature is
## above the Sdeb threshold. The heat wave is interrupted if the temperature is
## below the Sdeb threshold for at least three consecutive days, or if the
## temperature falls back (even occasionally) to values below the Sint
## threshold. Once a heat wave is identified, it can be characterized by three
## values: duration, maximal mean temperature reached during the event, and
## global intensity which is defined by the cumulative difference between the
## temperature and the Sdeb threshold during the event, divided by the
## difference between Spic and Sdeb." -- quotes from the paper

## daily average temperature is used in the analysis.
## The three threshold uses the three quantiles of the 20 year data
## Spic: 99.5 percentile
## Sdeb: 97.5 percentile
## Sint: 95   percentile

#' Get the three threshold Spic, Sdeb, Sint
#'
#' @param temperature a vector of temperature
#' @param target.quantiles quantile level to compute the three thresholds.
#'     default are 0.995, 0.975, 0.95
#' @return a named list with keys Spic, Sdeb, Sint
#' @export
get.threshold <- function(temperature, target.quantiles=c(0.995, 0.975, 0.95)) {
    quantile(temperature, probs = target.quantiles)
}

#' Get the start of periods with temperature lower than "lessThan" for at least
#' min.run days long
#' @export
get.runs.start.end <- function(temperature, lessThan, min.run=3) {
    result = rle(temperature < lessThan)
    duration = result$lengths %>% as.vector()
    start = c(1, cumsum(duration)[1:(length(duration) - 1)] + 1)
    end = cumsum(duration)
    good.run.idx <- which((result$values) & (result$lengths >= min.run))
    list("start"=start[good.run.idx], "end"=end[good.run.idx])
    ## fixme: need to get end
}

#' Get the heatwave statistics
#'
#' Get start, end, duration, maximum temperature, global intensity of heatwaves
#'
#' @param df a data frame, containing a "temperature" column and a "day" column
#' @param thresholds three thresholds.
#' @return a data frame with start and end time for heatwave
#' @export
heatwave.start.end <- function(df, thresholds) {
    ## in case the thresholds probabilities are reversed
    spic = max(thresholds)
    sint = min(thresholds)
    sdeb = thresholds[[2]]
    print(sprintf("%s %s %s", spic, sint, sdeb))
    hw <- which(df$temperature > spic)
    print(hw[1:10])
    candidate.start <- which(df$temperature > sdeb)
    hw.start <- sapply(hw,
                       function(x) {
                           ## start date can equal to detect date
                           max(candidate.start[which(x >= candidate.start)])})
    print(hw.start[1:30])
    candidate.end <- which(df$temperature < sint)
    hw.end <- sapply(hw,
                       function(x) {
                           min(candidate.end[which(x < candidate.end)])})
    print(hw.end[1:30])
    run.start.end <- get.runs.start.end(df$temperature, sdeb, 3)
    run.start <- run.start.end$start
    run.end <- run.start.end$end
    hw.end.2 <- sapply(hw, function(x) {
        min(run.end[which((x < run.start) & (x < run.end))])})
    tibble::tibble(start = df$day[hw.start],
                   end.sint = df$day[hw.end],
                   end.sdeb = df$day[hw.end.2],
                   detect = df$day[hw])
}

