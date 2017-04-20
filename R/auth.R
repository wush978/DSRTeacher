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
