.get.servers <- function(switch = FALSE) {
  c("https://api.datascienceandr.org", "https://api2.datascienceandr.org")
}
.cookie.env <- new.env()
.cookie.env$cookie <- NULL
#'@export
login <- function(account) {
  password <- getPass::getPass("password: ")
  object <- sprintf("%d-%s", as.integer(Sys.time()), paste(sample(letters, 4, TRUE), collapse = ""))
  servers <- .get.servers()
  for(server in servers) {
    tryCatch({
      .r <- httr::POST(sprintf("%s/api/classroomAuth", server), body = list(account = account, object = object, hmac = digest::hmac(password, object, algo = "sha256")), encode = "form")
      if (httr::status_code(.r) < 300) {
        userinfo <- list(name = account, tracked_usr = sprintf("classroom:%s", account), usr = account)
      } else {
        .remove_classroom_password(account)
        stop("Invalid account or password")
      }
      return(userinfo)
    }, error = function(e) {
      Sys.sleep(1)
      if (conditionMessage(e) == "Invalid account or password") stop(conditionMessage(e))
      warnings(conditionMessage(e))
    })
  })
  }
}
