#' Create a Shiny Tree
#' 
#' This creates a spot in your Shiny UI for a shinyTree which can then be filled
#' in using \code{\link{renderTree}}
#' @param outputId The ID associated with this element
#' @param checkbox If \code{TRUE}, will enable checkboxes next to each node to 
#' make the selection of multiple nodes in the tree easier.
#' @param search If \code{TRUE}, will enable search functionality in the tree by adding
#' a search box above the produced tree. Alternatively, you can set the parameter
#' to the ID of the text input you wish to use as the search field.
#' @param searchtime Determines the reaction time of the search algorithm. Default is 1000ms.
#' @param dragAndDrop If \code{TRUE}, will allow the user to rearrange the nodes in the
#' tree.
#' @param types enables jstree types functionality when sent proper json (please see the types example)
#' @param theme jsTree theme, one of \code{default}, \code{default-dark}, or \code{proton}.
#' @param themeIcons If \code{TRUE}, will show theme icons for each item.
#' @param themeDots If \code{TRUE}, will include level dots.
#' @param sort If \code{TRUE}, will sort the nodes in alphabetical/numerical order.
#' @param unique If \code{TRUE}, will ensure that no node name exists more than once.
#' @param wholerow If \code{TRUE}, will highlight the whole selected row.
#' @param state If \code{TRUE}, will enable the state plugin and will store the tree nodes in the 
#' browser, so opened and selected nodes will remain after a page refresh. The key used is 'jstree'
#' @param contextmenu If \code{TRUE}, will enable a contextmenu.
#' @param openAll Activates a button to open all nodes if \code{TRUE}. Can be a list with
#' \emph{id}, \emph{class}, \emph{label} for the button. \code{FALSE} or \code{NULL} will deactivate
#' the button.
#' @param closeAll Activates a button to close all opened nodes if \code{TRUE}. Can be a list with
#' \emph{id}, \emph{class}, \emph{label}, \emph{icon} for the button.
#' @param toggleStripes Activates a button to toggle stripes if \code{TRUE}. Can be a list with
#' \emph{id}, \emph{class}, \emph{label}, \emph{icon} for the button.
#' @param toggleDots Activates a button to toggle the dots if \code{TRUE}. Can be a list with
#' \emph{id}, \emph{class}, \emph{label}, \emph{icon} for the button. 
#' @param toggleIcons Activates a button to toggle the icons if \code{TRUE}. Can be a list with
#' \emph{id}, \emph{class}, \emph{label}, \emph{icon} for the button. 
#' @seealso \code{\link{renderTree}}
#' @export
shinyTree <- function(outputId, checkbox=F, 
                        search=F, dragAndDrop=F, 
                        types=NULL, theme="default", themeIcons=T, 
                        themeDots=T, contextmenu=F,
                        sort=F, unique=T, wholerow=T, searchtime=1000,
                        state = F, openAll=F, closeAll=F, toggleStripes=F,
                        toggleDots=F, toggleIcons=F){
  
  searchEl <- shiny::div("")
  if (search == TRUE){
    search <- paste0(outputId, "-search-input")
    searchEl <- shiny::tags$input(id=search, class="input", type="text", value="")
  }
  if (is.character(search)){
    # Either the search field we just created or the given text field ID
    searchEl <- shiny::tagAppendChild(searchEl, shiny::tags$script(type="text/javascript", shiny::HTML(
      paste0("shinyTree.initSearch('",outputId,"','",search,"', ", searchtime,");")
      )))
  }
  
  btn0 = btn1 = btn2 = btn3 = btn4 = NULL;
  extraBtns = list(openAll, closeAll, toggleStripes, toggleDots, toggleIcons)
  names(extraBtns) <- c("openAll", "closeAll", "toggleStripes", "toggleDots", "toggleIcons")
  if (any(unlist(lapply(extraBtns, is.logical)))){
    ind = which(unlist(lapply(extraBtns, is.logical)))
    extraBtns[ind] = lapply(extraBtns[ind], FUN = function(x) { 
      if (x) list(id=NULL, class=NULL, label=NULL, icon=NULL) else F })
  }
  
  ## use fixIconName ???
  ## more actions
  ## combine t dropdown menue?
  if (is.list(extraBtns[[1]])) {
    openAll <- extraBtns$openAll
    if (is.null(openAll$id)) {
      openAll$id="open_tree"
    }
    if (is.null(openAll$class)) {
      openAll$class="btn btn-default"
    }
    if (is.null(openAll$label)) {
      openAll$label="Open all"
    }
    if (is.null(openAll$icon)) {
      openAll$icon="folder-open"
    }  
    tmp = shiny::HTML(paste0('$("#', outputId,'").jstree("open_all");'))
    btn = shiny::tags$button(id=openAll$id, class=openAll$class, type="button", onclick=tmp)
    btn = shiny::tagAppendChild(btn, shiny::icon(openAll$icon))
    btn0 = shiny::tagAppendChild(btn, openAll$label)
  }
  if (is.list(extraBtns[[2]])) {
    closeAll <- extraBtns$closeAll
    if (is.null(closeAll$id)) {
      closeAll$id="close_tree"
    }
    if (is.null(closeAll$class)) {
      closeAll$class="btn btn-default"
    }
    if (is.null(closeAll$label)) {
      closeAll$label="Close all"
    }
    if (is.null(closeAll$icon)) {
      closeAll$icon="folder"
    }  
    tmp = shiny::HTML(paste0('$("#', outputId,'").jstree("close_all");'))
    btn = shiny::tags$button(id=closeAll$id, class=closeAll$class, type="button", onclick=tmp)
    btn = shiny::tagAppendChild(btn, shiny::icon(closeAll$icon))
    btn1 = shiny::tagAppendChild(btn, closeAll$label)
  }
  if (is.list(extraBtns[[3]])) {
    toggleStripes <- extraBtns$toggleStripes
    if (is.null(toggleStripes$id)) {
      toggleStripes$id="toggle_stripes"
    }
    if (is.null(toggleStripes$class)) {
      toggleStripes$class="btn btn-default"
    }
    if (is.null(toggleStripes$label)) {
      toggleStripes$label="Toggle Stripes"
    }
    if (is.null(toggleStripes$icon)) {
      toggleStripes$icon="align-justify"
    }  
    tmp = shiny::HTML(paste0('$("#', outputId,'").jstree("toggle_stripes");'))
    btn = shiny::tags$button(id=toggleStripes$id, class=toggleStripes$class, type="button", onclick=tmp)
    btn = shiny::tagAppendChild(btn, shiny::icon(toggleStripes$icon))
    btn2 = shiny::tagAppendChild(btn, toggleStripes$label)
  }
  if (is.list(extraBtns[[4]])) {
    toggleDots <- extraBtns$toggleDots
    if (is.null(toggleDots$id)) {
      toggleDots$id="toggle_dots"
    }
    if (is.null(toggleDots$class)) {
      toggleDots$class="btn btn-default"
    }
    if (is.null(toggleDots$label)) {
      toggleDots$label="Toggle Dots"
    }
    if (is.null(toggleDots$icon)) {
      toggleDots$icon="ellipsis-v"
    }  
    tmp = shiny::HTML(paste0('$("#', outputId,'").jstree("toggle_dots");'))
    btn = shiny::tags$button(id=toggleDots$id, class=toggleDots$class, type="button", onclick=tmp)
    btn = shiny::tagAppendChild(btn, shiny::icon(toggleDots$icon))
    btn3 = shiny::tagAppendChild(btn, toggleDots$label)
  }
  if (is.list(extraBtns[[5]])) {
    toggleIcons <- extraBtns$toggleIcons
    if (is.null(toggleIcons$id)) {
      toggleIcons$id="toggle_icons"
    }
    if (is.null(toggleIcons$class)) {
      toggleIcons$class="btn btn-default"
    }
    if (is.null(toggleIcons$label)) {
      toggleIcons$label="Toggle Icons"
    }
    if (is.null(toggleIcons$icon)) {
      toggleIcons$icon="ellipsis-v"
    }  
    tmp = shiny::HTML(paste0('$("#', outputId,'").jstree("toggle_icons");'))
    btn = shiny::tags$button(id=toggleIcons$id, class=toggleIcons$class, type="button", onclick=tmp)
    btn = shiny::tagAppendChild(btn, shiny::icon(toggleIcons$icon))
    btn4 = shiny::tagAppendChild(btn, toggleIcons$label)
  }

  
  if(!theme %in% c("default","default-dark","proton")) { stop(paste("shinyTree theme, ",theme,", doesn't exist!",sep="")) }
  
  # define theme tags (default, default-dark, or proton)
  theme.tags<-shiny::tags$link(rel = 'stylesheet',
                               type = 'text/css',
                               href = paste('shinyTree/jsTree-3.3.6/themes/',theme,'/style.min.css',sep=""))
  
  if(!is.null(types)){
    types <- paste("sttypes =",types)
  }

  shiny::tagList(
    shiny::singleton(shiny::tags$head(
      initResourcePaths(),
      theme.tags,
      shiny::tags$link(rel = "stylesheet", 
                type = "text/css", 
                href = "shared/font-awesome/css/font-awesome.min.css"),
      shiny::tags$script(src = 'shinyTree/jsTree-3.3.6/jstree.min.js'),
      shiny::tags$script(src = 'shinyTree/shinyTree.js'),
      shiny::tags$script(shiny::HTML(types))
    )),
    searchEl,
    btn0,
    btn1,
    btn2,
    btn3,
    btn4,
    shiny::div(id=outputId, class="shiny-tree", 
        `data-st-checkbox`=checkbox, 
        `data-st-search`=is.character(search),
        `data-st-dnd`=dragAndDrop,
        `data-st-types`=!is.null(types),
        `data-st-theme`=theme,
        `data-st-theme-icons`=themeIcons,
        `data-st-theme-dots`=themeDots,
        `data-st-contextmenu`=contextmenu,
        `data-st-sort`=sort,
        `data-st-unique`=unique,
        `data-st-wholerow`=wholerow,
        `data-st-state`=state
        )
  )
}
