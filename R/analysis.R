#'@export
DSR.lessons <- sprintf(
  "DataScienceAndR:%s",
  c("00-Hello-DataScienceAndR", "01-RBasic-01-Introduction", "01-RBasic-02-Data-Structure-Vectors",
    "01-RBasic-03-Data-Structure-Object", "01-RBasic-04-Factors",
    "01-RBasic-05-Arrays-Matrices", "01-RBasic-06-List-DataFrame",
    "01-RBasic-07-Loading-Dataset", "02-RDataEngineer-01-Parsing",
    "02-RDataEngineer-02-XML", "02-RDataEngineer-03-JSON", "02-RDataEngineer-04-Database",
    "02-RDataEngineer-05-Data-Manipulation", "02-RDataEngineer-06-Join",
    "03-RVisualization-01-One-Variable-Visualization", "03-RVisualization-02-Multiple-Variables-Visualization",
    "03-RVisualization-03-ggplot2", "03-RVisualization-04-Javascript-And-Maps",
    "Optional-RProgramming-01-Loop-And-Condition", "Optional-RProgramming-02-Function-Introduction",
    "Project-ROpenData-Power-GDP"
    )
)
#'@export
#'@title Analyze whether the student skipped the questions or not
#'@return data.frame with following columns: \itemize{
#'  \item course The course name.
#'  \item question.no The number of the question
#'  \item user_id The id of the student
#'  \item skipped Whether the question is skipped or not
#'}
#'Note that the students might do the question multiple times, so
#'there might be multiple rows for the specific course, question.no and user_id.
#'Each row represents each trials of the student at the specific course and question.
#'@param records The data.frame of the raw records. This should be the output of the function
#'\code{get.records}.
#'@param students The user_id to be analyzed. If it is \code{NULL}, then all students in the \code{records} will
#'be analyzed.
#'@param lessons The character vector of the courses to be analyzed.
#'@param course.source named list. The name is the course name.
#'The value is the root of the swirl course directory.
skipped.analysis <- function(
  records, students = NULL,
  lessons = DSR.lessons,
  course.source = list(
    "DataScienceAndR" = "https://raw.githubusercontent.com/wush978/DataScienceAndR/course"
    )
  ) {
  if (is.null(students)) students <- unique(records$user_id)
  as.data.frame(records) %>%
    dplyr::filter(user_id %in% students, course %in% lessons, type == 1) %>%
    dplyr::group_by(course) %>%
    dplyr::do({
      course.tokens <- .$course[1] %>%
        strsplit(split = ":", fixed = TRUE) %>%
        extract2(1)
      course <- course.tokens[1]
      lesson <- course.tokens[2]
      lesson.src <- try(
        yaml::yaml.load_file(file.path(course.source[[course]], lesson, "lesson.src.yaml")),
        silent = TRUE
      )
      if (class(lesson.src) == "try-error") lesson.src <- try(
        yaml::yaml.load_file(file.path(course.source[[course]], lesson, "lesson.yaml")),
        silent = TRUE
      )
      if (class(lesson.src) == "try-error") stop(conditionMessage(attr(lesson.src, "condition")))
      lesson.class <- sapply(lesson.src, "[[", "Class")
      total.correct <- sum(lesson.class %in% c("script", "mult_question", "cmd_question"))
      dplyr::group_by(., user_id) %>%
        dplyr::do({
          user_id.data <- .
          # browser()
          log <- lapply(.$log, jsonlite::fromJSON)
          itertools::izip(
            correct = lapply(log, "[[", "correct"),
            skipped = lapply(log, "[[", "skipped"),
            question_number = lapply(log, "[[", "question_number")
          ) %>%
            lapply(function(x) {
              if (length(x$correct) > length(x$skipped)) x$skipped <- c(FALSE, x$skipped)
              result <- x$skipped[x$correct]
              data.frame(question.no = which(!lesson.class %in% c("meta", "text")), skipped = result)
            }) %>%
            do.call(what = rbind)
        })
    }) %>%
    as.data.frame()
}
