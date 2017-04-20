
#'@title Download the Records of Associated Students
#'@return A data.frame whose rows are the observed events. It contains following columns:
#'\itemize{
#'  \item{user_id} {The id of the student who triggered the event.}
#'  \item{created_at} {The time when the even is recorded. The timezone is Taipei/Asia.}
#'  \item{course} {The swirl course and lesson of the event. The format is \code{<course>:<lesson>}.}
#'  \item{type} {Indicates whether the student entered the lesson or finished the lesson. 0 is entering. 1 is finishing.}
#'  \item{version} {The version of the lesson.}
#'  \item{userinfo} {Information of the users.}
#'  \item{log} {The detailed log of the user activity.}
#'}
#'@export
#'@importFrom data.table data.table
get.records <- function() {
  records <- .get.records() %>%
    data.table() %>%
    dtplyr::tbl_dt() %>%
    dplyr::mutate(created_at = as.POSIXct(created_at))
  data.table::setkey(records, "user_id", "course")
}

.get.records <- function() {
  user <- .get_current_user()
  .cookie <- .get_user_cookie(user)
  servers <- .get.servers()
  for(server in sample(servers)) {
    tryCatch({
      .r <- httr::POST(
        sprintf("%s/api/auth/getTeacherRecords", server),
        body = list(),
        encode = "json",
        .ca,
        httr::add_headers(Connection = "keep-alive"),
        httr::set_cookies(.cookie$value),
        httr::write_disk(tmp.path <- tempfile(fileext = ".Rds"))
        )
      if (httr::status_code(.r) < 300) {
        return(readRDS(tmp.path))
      } else {
        stop("Invalid account or password")
      }
    }, error = function(e) {
      Sys.sleep(1)
      if (conditionMessage(e) == "Invalid account or password") stop(conditionMessage(e))
      warnings(conditionMessage(e))
    })
  }
}

.null.to.0 <- function(x) {
  if (is.null(x)) 0 else x
}

.to.POSIXct <- function(x) {
  stopifnot(is.numeric(x))
  class(x) <- c("POSIXct", "POSIXt")
  x
}

#'@title Extract the student progress from the records
#'@param records The data returned from \code{\link{get.records}}.
#'@param key The key of the google spreadsheet of the project.
#'  If the \code{key} is not specified, then only students and courses recorded in the \code{records} are reported.
#'@return A data.frame whose column is:
#'\itemize{
#'  \item{student} {The classroom account of the student.}
#'  \item{course} {The tracked course.}
#'  \item{status} {Indicates the progress of the student at the course. "O" : The student finished the course. "P": The student entered but not finished the course. "X": The student has not entered the course yet.}
#'  \item{finish.time} {The first time to complete.}
#'  \item{duration} {The time student spent on the course before the first accomplishment.}
#'  \item{comment} {Additional information from the records.}
#'}
#'@export
#'@examples
#'\dontrun{
#'The teacher could convert the result to wild table as follow:
#'library(reshape2)
#'# login first
#'records <- get.records()
#'result <- get.progress.table(records)
#'dcast(result, student ~ course, value.var = "status")
#'}
#'@importFrom magrittr %>% %<>%
get.progress.table <- function(records, key = NULL) {
  if (is.null(key)) {
    students <- unique(records$user_id)
    courses <- unique(records$course)
  } else {
    googlesheets::gs_auth()
    .doc <- googlesheets::gs_key(key, verbose = TRUE)
    students <- googlesheets::gs_read(.doc, "user_id", col_names = FALSE)[[1]] %>%
      sprintf(fmt = "classroom:%s")
    courses <- googlesheets::gs_read(.doc, "course_id", col_names = FALSE)[[1]]
    rm(.doc)
  }
  result <- expand.grid(student = students, course = courses, stringsAsFactors = FALSE) %>%
    dplyr::mutate(status = "X", finish.time = NA, duration = NA, comment = NA)
  for(i in seq_len(nrow(result))) {
    student.value <- result$student[i]
    course.value <- result$course[i]
    .df <- dplyr::filter(records, user_id == student.value, course == course.value) %>%
      dplyr::arrange(created_at)
    .df <- dplyr::mutate(.df, index = seq_len(nrow(.df)))
    if (nrow(.df) == 0) {
      result$status[i] <- "X"
    } else if (all(.df$type == 0)) {
      result$status[i] <- "P"
      comment <- sprintf("The student tried %d times. ", nrow(.df))
      result$comment[i] <- comment
    } else {
      result$status[i] <- "O"
      time.enter <- dplyr::filter(.df, type == 0) %>%
        `[[`("created_at") %>%
        min()
      time.finish <- dplyr::filter(.df, type == 1) %>%
        `[[`("created_at") %>%
        min()
      result$finish.time[i] <- format(time.finish)
      result$duration[i] <- difftime(time.finish, time.enter, units = "secs") %>%
        as.numeric()
      comment <- sprintf("The student finished this course %d times. ", dplyr::filter(.df, type == 1) %>% nrow())
      log <- dplyr::filter(.df, type == 1) %>%
        `[[`("log") %>%
        lapply(jsonlite::fromJSON)
      skip.last.count <- sapply(log, function(obj) {
        tail(obj$skipped, 1) %>%
          .null.to.0()
      }) %>% sum()
      comment <- sprintf("The student skip the last challange %d times. ", skip.last.count) %>%
        append(x = comment)
      skip.count <- sapply(log, function(obj) {
        obj$skipped %>%
          .null.to.0() %>%
          sum()
      })
      comment <- sprintf("The student totally skip %d times. ", skip.count) %>%
        append(x = comment)
      restore.count <- sapply(log, function(obj) {
        length(obj$restore_timestamp)
      }) %>%
        sum()
      comment <- sprintf("The student totally restore %d times. ", restore.count) %>%
        append(x = comment)
      comment %<>% paste(collapse = "\n")
      result$comment[i] <- comment
    }
  }
  result
}

