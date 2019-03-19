#' Update the tree with new data
#' 
#' Extract the nodes from the tree that are selected in a more
#' convenient format. You can choose which format you prefer.
#' 
#' @param session The current session variable.
#' @param treeId The identifier for the shinyTree object
#' @param data JSON data or nested list representing the new tree structure.
#' @export
updateTree <- function(session, treeId, data=NULL) {
  data <- Rlist2json(data)
  message <- list(type="updateTree",data=data)
  session$sendInputMessage(treeId, message)
}

#' @importFrom rjson toJSON
Rlist2json <- function(nestedList) {
  d <- as.character(toJSON(get_flatList(nestedList)))
  gsub(d, pattern = "null", fixed = TRUE, replacement = "{}")
}

get_flatList <- function(nestedList, flatList = NULL, parent = "#") {
  for (name in names(nestedList)) {
    additionalAttributes <- list(
      "icon" = attr(nestedList[[name]],"sticon"),
      "type" = attr(nestedList[[name]],"sttype")
    )

    additionalAttributes <- additionalAttributes[!unlist(lapply(additionalAttributes,is.null))]

    data <- lapply(names(attributes(nestedList[[name]])), function(key){
      if (key %in% c("icon","type","names","stopened","stselected","sttype")){
        NULL
      }else{
        attr(nestedList[[name]],key)
      }
    })
    if (!is.null(data) && length(data) > 0) {
      names(data) <- names(attributes(nestedList[[name]]))
      data <- data[!unlist(lapply(data,is.null))]
    }

    nodeData <- append(
      list(
        id = as.character(length(flatList) + 1),
        text = name,
        parent = parent,
        state = list(
          opened   = isTRUE(attr(nestedList[[name]], "stopened")),
          selected = isTRUE(attr(nestedList[[name]], "stselected"))
        ),
        data = data
      ),
      additionalAttributes
    )

    flatList = c(flatList,list(nodeData))
    if (is.list(nestedList[[name]])) {
      flatList = get_flatList(nestedList[[name]], flatList, parent = as.character(length(flatList)))
    }
  }
  flatList
}