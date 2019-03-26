#' Get the selected nodes from a tree
#' 
#' Extract the nodes from the tree that are selected in a more
#' convenient format. You can choose which format you prefer.
#' 
#' @param tree The \code{input$tree} shinyTree you want to 
#' inspect.
#' @param format In which format you want the output. Use 
#' \code{names} to get a simple list of the names (with attributes
#' describing the node's ancestry), or \code{slices} to get a list
#' of lists, each of which is a slice of the list used to get down
#' to the selected node. 
#' @export
get_selected <- function(tree, format=c("names","names2","names3","names4", "slices", "classid")){
  format <- match.arg(format, c("names", "names2","names3", "names4", "slices", "classid"), FALSE)
  switch(format,
         "names"=get_selected_names(tree),
         "names2"=get_selected_names2(tree),
         "names3"=get_selected_names3(tree),
         "names4"=get_selected_names4(tree),
         "slices"=get_selected_slices(tree),
         "classid"=get_selected_classid(tree)
         )  
}

get_selected_names2 = function(tree) {
  names(unlist(lapply(tree, function(i) attr(i, "stselected", TRUE))))
}
get_selected_names3 = function(tree) {
  a <- unlist(lapply(tree, function(i) {
    lapply(i, function(j) {
      attr(j, "stselected", TRUE)
    })
  }));
  whna = which(sapply(names(tree), function(i) { grepl(pattern = i, x = names(a), fixed = TRUE)}))
  names(whna)
}
get_selected_names4 = function(tree) {
  a <- unlist(lapply(tree, function(i) {
    sapply(i, function(j) {
      lapply(j, function(k) {
        attr(k, "stselected", TRUE)
      })
    }, USE.NAMES = F)
  }));
  names(a)
}

get_selected_names <- function(tree, ancestry=NULL, vec=list()){
  if (is.list(tree)){
    for (i in 1:length(tree)){
      anc <- c(ancestry, names(tree)[i])
      vec <- get_selected_names(tree[[i]], anc, vec)
    }    
  }
  
  a <- attr(tree, "stselected", TRUE)
  if (!is.null(a) && a == TRUE){
    # Get the element name
    leAn <- length(ancestry)
    el <- ancestry[leAn]
    vec[length(vec)+1] <- el
    attr(vec[[length(vec)]], "ancestry") <- ancestry[1:leAn-1]
    #save attributes that start with "st" (ShinyTree)
    lapply(names(attributes(tree)),function(attribute){
      if(grepl("^st",attribute)){
        attr(vec[[length(vec)]], attribute) <<- attr(tree,attribute)
      }
    })
  }
  return(vec)
}

get_selected_slices <- function(tree, ancestry=NULL, vec=list()){
  
  if (is.list(tree)){
    for (i in 1:length(tree)){
      anc <- c(ancestry, names(tree)[i])
      vec <- get_selected_slices(tree[[i]], anc, vec)
    }    
  }
  
  a <- attr(tree, "stselected", TRUE)
  if (!is.null(a) && a == TRUE){
    # Get the element name
    ancList <- 0
    
    for (i in length(ancestry):1){
      nl <- list()
      nl[ancestry[i]] <- list(ancList)
      ancList <- nl
    }
    vec[length(vec)+1] <- list(ancList)
  }
  return(vec)
}

get_selected_classid <- function(tree, ancestry=NULL, vec=list()){
    if (is.list(tree)){
      for (i in 1:length(tree)){
        anc <- c(ancestry, names(tree)[i])
        vec <- get_selected_classid(tree[[i]], anc, vec)
      }
    }
    
    a <- attr(tree, "stselected", TRUE)
    if (!is.null(a) && a == TRUE){
      # Get the element name
      el <- ancestry[length(ancestry)]
      vec[length(vec)+1] <- el
      attr(vec[[length(vec)]], "stclass") <- attr(tree, "stclass", TRUE)
      attr(vec[[length(vec)]], "stid") <- attr(tree, "stid", TRUE)
      attr(vec[[length(vec)]], "id") <- attr(tree, "id", TRUE)
    }
    return(vec)
  }
