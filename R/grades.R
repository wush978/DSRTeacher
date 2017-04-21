#'@title Calculate the grades according to the Records
#'@param records The data from \code{\link{get.progress.table}}
#'@param grades A named numeric vector. The name is the course name. The value is the grades associated with the course.
#'@param threshold A POSIXct vector. This is the deadline of the corresponding course / grades.
#'@param decay.gap The duration of discounting the score if the student is late. Please see the details.
#'  Specifying \code{decay.gap = 0} will make the score directly be 0 if the student is late.
#'@param decay.ratio The amount of discounting of the score if the student is late.
#'@details
#'If the student was late but he/she accomplished the course in \code{decay.gap} seconds after the threshold timestamp,
#'then his/her score was multiplied by \code{1 - decay.ratio}.
#'The discount will become heavier and heavier until the ratio become 0 or less.
#'@return A data.frame whose column is:
#'\itemize{
#'  \item{student} {The classroom account of the student.}
#'  \item{grades} {The total scores of the student.}
#'  \item{comment} {The detailed score of the student.}
#'}
#'@export
#'@examples
#'\dontrun{
#'# login first
#'records <- get.records()
#'result <- get.progress.table(records)
#'grades <- structure(c(0, 6, 0, 4, 0, 0, 6, 2, 2, 2, 2, 0, 4, 4, 4, 0, 6,
#'  6, 3, 3, 3, 3), .Names = c("2017-NTUCSX:2nd-week", "2017-NTUCSX:2nd-week-hw",
#'  "2017-NTUCSX:3rd-week", "2017-NTUCSX:3rd-week-hw", "DataScienceAndR:00-Hello-DataScienceAndR",
#'  "DataScienceAndR:01-RBasic-01-Introduction", "DataScienceAndR:01-RBasic-02-Data-Structure-Vectors",
#'  "DataScienceAndR:01-RBasic-03-Data-Structure-Object", "DataScienceAndR:01-RBasic-04-Factors",
#'  "DataScienceAndR:01-RBasic-05-Arrays-Matrices", "DataScienceAndR:01-RBasic-06-List-DataFrame",
#'  "DataScienceAndR:01-RBasic-07-Loading-Dataset", "DataScienceAndR:02-RDataEngineer-01-Parsing",
#'  "DataScienceAndR:02-RDataEngineer-02-XML", "DataScienceAndR:02-RDataEngineer-03-JSON",
#'  "DataScienceAndR:02-RDataEngineer-04-Database", "DataScienceAndR:02-RDataEngineer-05-Data-Manipulation",
#'  "DataScienceAndR:02-RDataEngineer-06-Join", "DataScienceAndR:03-RVisualization-01-One-Variable-Visualization",
#'  "DataScienceAndR:03-RVisualization-02-Multiple-Variables-Visualization",
#'  "DataScienceAndR:03-RVisualization-03-ggplot2", "DataScienceAndR:03-RVisualization-04-Javascript-And-Maps"
#'))
#'thresholds <- c("2017-03-16 08:00:00", "2017-03-16 08:00:00", "2017-03-16 08:00:00",
#'  "2017-03-23 08:00:00", "2017-03-16 08:00:00", "2017-03-16 08:00:00",
#'  "2017-03-16 08:00:00", "2017-03-23 08:00:00", "2017-03-23 08:00:00",
#'  "2017-03-23 08:00:00", "2017-03-23 08:00:00", "2017-03-16 08:00:00",
#'  "2017-03-30 08:00:00", "2017-03-30 08:00:00", "2017-03-30 08:00:00",
#'  "2017-03-16 08:00:00", "2017-04-06 08:00:00", "2017-04-06 08:00:00",
#'  "2017-04-13 08:00:00", "2017-04-13 08:00:00", "2017-04-13 08:00:00",
#'  "2017-04-13 08:00:00")
#'thresholds <- as.POSIXct(thresholds)
#'
#'result.grades <- get.grades(result, grades, thresholds)
#'}
get.grades <- function(result, grades, thresholds, decay.gap = 86400 * 7, decay.ratio = 0.2) {
  result.grades <- data.table::data.table(student = unique(result$student), grades = 0, comment = "")
  data.table::setkey(result.grades, "student")
  stopifnot(length(grades) == length(thresholds))
  it <- itertools::izip(course = names(grades), grade = grades, threshold = thresholds) %>%
    itertools::ihasNext()
  while(itertools::hasNext(it)) {
    .x <- iterators::nextElem(it)
    if (.x$grade == 0) next
    .df <- dplyr::filter(result, course == .x$course) %>%
      dplyr::select(student, course, status, finish.time)
    .finish <- dplyr::filter(.df, status == "O") %>%
      dplyr::mutate(done = FALSE) %>%
      data.table::data.table()
    data.table::setkey(.finish, "student")
    ratio <- 1.0
    while(ratio > 0) {
      .df2 <- dplyr::filter(.finish, finish.time < .x$threshold, !done)
      .old.grades <- result.grades[.df2$student]$grades
      .old.comment <- result.grades[.df2$student]$comment
      .new.comment <- sprintf("%s %s:%s", .old.comment, .x$course, .x$grade * ratio)
      result.grades[.df2$student, grades := .old.grades + .x$grade * ratio]
      result.grades[.df2$student, comment := .new.comment]
      .finish[.df2$student, done := TRUE]
      if (decay.gap == 0) break
      .x$threshold <- .x$threshold + decay.gap
      ratio <- ratio - decay.ratio
      if (ratio <= 0) break
    }
  }
  result.grades
}
