.get.servers <- function(switch = FALSE) {
  c("https://api.datascienceandr.org", "https://api2.datascienceandr.org")
}
.ca <- httr::config(cainfo = system.file("cert/dsr.ca-bundle", package = "DSRTeacher"))

.current <- new.env()
.set_current_user <- function(user) {
  assign("user", user, envir = .current)
}
.get_current_user <- function() {
  retval <- try(get("user", envir = .current), silent = TRUE)
  if (class(retval)[1] == "try-error") stop("Please login first. Check `?login` for documentation of login") else retval
}

.user_cookie <- new.env()
.set_user_cookie <- function(user, cookie) {
  assign(user, cookie, envir = .user_cookie)
}
.get_user_cookie <- function(user) {
  retval <- try(get(user, envir = .user_cookie, inherits = FALSE), silent = TRUE)
  if (class(retval)[1] == "try-error") NULL else retval
}

#'@title Login to the DataScienceAndR Service
#'@param account The registered account of the teacher
#'@return NULL. It will raise an error if login is failed.
#'@examples
#'\dontrun{
#'login("wush")
#'# input the password
#'
#'# get records of all associated students
#'records <- get.records()
#'}
#'@export
login <- function(account) {
  .cookie <- try(.get_user_cookie(account))
  if (is.null(.cookie)) {
    password <- getPass::getPass("password: ")
    servers <- .get.servers()
    for(server in sample(servers)) {
      tryCatch({
        object <- sprintf("%d-%s", as.integer(Sys.time()), paste(sample(letters, 4, TRUE), collapse = ""))
        .r <- httr::POST(
          sprintf("%s/api/authUser", server),
          body = list(
            service = "classroom",
            token = jsonlite::toJSON(list(
              account = account, object = object, hmac = digest::hmac(password, object, algo = "sha256")
            ), auto_unbox = TRUE)
          ),
          encode = "json",
          .ca,
          httr::add_headers(Connection = "keep-alive")
          )
        if (httr::status_code(.r) < 300) {
          .set_user_cookie(account, httr::cookies(.r))
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
  .set_current_user(account)
}

#'@title Download the Records of Associated Students
#'@return A data.frame whose rows are the observed events. It contains following columns:
#'\itemize{
#'  \item{user_id} {The id of the student who triggered the event.}
#'  \item{created_at} {The time when the even is recorded. The timezone is Taipei/Asia.}
#'  \item{course} {The swirl course and lesson of the event. The format is \code{<course>:<lesson>}.}
#'  \item{type} {Indicates whether the student entered the lesson or finished the lesson.}
#'  \item{version} {The version of the lesson.}
#'  \item{userinfo} {Information of the users.}
#'  \item{log} {The detailed log of the user activity.}
#'}
#'@export
get.records <- function() {
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

