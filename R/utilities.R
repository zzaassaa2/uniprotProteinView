#' If else function
#'
#' Made because function provided by base package A) is really slow as there
#' are a thousand unneeded things they do, and B) cause the function from the
#' base package also for some reason returns incorrect values, no clue
#'
#' @param condition Condition for which to evaluate by
#'
#' @param true What value should be returned/run should the condition be true
#'
#' @param false What value should be returned/run should the condition be false
#'
#' @return Value depending on should the condition be true or false
#'
#' @author {George Zorn, \email{george.zorn@mail.utoronto.ca}}
ifelse <- function (condition, true, false){
  if(condition){
    true
  }else{
    false
  }
}

#' Get and random rgb color
#'
#' @return Returns random rgb color
#'
#' @author {George Zorn, \email{george.zorn@mail.utoronto.ca}}
#'
#' @export
#' @importFrom stats runif
#' @importFrom grDevices rgb
randomColor <- function(){
  i <- runif(3)
  rgb(i[[1]],
      i[[2]],
      i[[3]]
  )
}


