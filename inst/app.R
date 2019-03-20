
library(devtools)
# install.packages("shinyTree")
# devtools::install_github("YsoSirius/shinyTreeSG")

## LIBS / DATA ###########################
library(shiny)
library(devtools)
library(shinycssloaders)
library(shinyTree)
# library(shinyTree)
library(ggplot2)
library(plotly)
library(data.table)
library(shinyjs)
library(shinyWidgets)
library(lubridate)

# setwd("C:/Users/Bobo/Documents/TraffiCon/VDE_TIROL/easyui_shiny")
setwd("C:/Users/Bobo/Documents/TraffiCon/VDE_TIROL/shinyTree")
# https://stackoverflow.com/questions/40403093/tree-view-in-r-shiny-using-shinytree-package
Data51 = list("SLZ" = "", "PKW" = "", "LKW" = "", "ADK" = "")
Data81 = list("SLZ" = "", "PKW" = "", "LKW" = "", "ADK" = "", "LKWAGO" = "")
tree <- fread(file = "C:/Users/Bobo/Documents/TraffiCon/VDE_TIROL/tree.csv", sep = ",")
tree$V1 <- NULL
tree <- tree[!tree$type=="",]
tree <- tree[1:10,]
dfN <- data.frame(
  time_stamp = seq.Date(as.Date("2018-01-01"), as.Date("2018-12-30"), 1.0),
  val = runif(364, 100,1000),
  col = "green", stringsAsFactors = F
)
setDT(dfN)
# dfN[sample(x = 1:40, size = 10, replace = F),]$col = "red"
# dfN[sample(x = 41:60, size = 10, replace = F),]$col = "blue"
# dfN[sample(x = 61:89, size = 10, replace = F),]$col = "darkgreen"
dfN[95,]$col = "orange"


# str(tree)
#############################
## FUNCS #########################
gettree <- function(df) {
  # df <- tr
  tmpAll = list()
  for (i in 1:length(df$directions)) {
    splitDir <- strsplit(df$directions[[i]], ",")[[1]]
    splitDir <- gsub("[[:space:]]", "", splitDir)
    splitDir[splitDir == "ALL"] <- "Gesamtquerschnitt"
    splitDir[splitDir == "D1"] <- "Fahrtrichtung 1"
    splitDir[splitDir == "D2"] <- "Fahrtrichtung 2"
    tmp <- as.list(splitDir)
    
    names(tmp) <- splitDir
    if (df$type[[i]] == "5+1") {
      tmp <- lapply(X = tmp, function(X) Data51)
      lapply(X = tmp, function(X) Data51)
    } else if (df$type[[i]] == "8+1") {
      tmp <- lapply(X = tmp, function(X) Data81)
    } else {
      tmp <-lapply(X = tmp, function(X) list(XLM = ""))
    }
    tmpAll[[i]] = tmp
  }
  
  names(tmpAll) <- df$name
  
  for (i in names(tmpAll)) {
    attr(tmpAll[[i]], "sttype") <- "root"
    # attr(tmpAll[[i]], "sticon") <- "/shinyTree/icon.png"
    attr(tmpAll[[i]], "sticon") <- "map-signs"
    for (j in names(tmpAll[[i]])) {
      attr(tmpAll[[i]][[j]], "sticon") <- "map-signs"
      attr(tmpAll[[i]][[j]], "sttype") <- "root1"
      for (k in names(tmpAll[[i]][[j]])) {
        attr(tmpAll[[i]][[j]][[k]], "sticon") <- "leaf"
        attr(tmpAll[[i]][[j]][[k]], "sttype") <- "root2"
      }
    }
  }
  
  return(tmpAll)
}
###########################

# ui <- htmlTemplate(filename="easyui.html")
ui <- {fluidPage(
  tags$head(),
  useShinyjs(),
  actionButton("icad", "ICON", icon = icon("leaf"), width = NULL),
  selectInput("change", label = "Change Order", multiple = F, choices = c("", "Names", "Id"), selected = ""),
  actionButton("runjs", "Close Tree"),
  actionButton("updateTree", "Update Tree"),
  textInput("searchtxt", "Search the tree"),
  splitLayout(cellWidths = c("30%","70%"),
    shinyTree("tree", search = "searchtxt", #sort = F, searchtime = 1000, contextmenu = F, state=F, 
              # openAll=T,
              # closeAll=list(label="Close me 1", id="asd", icon="leaf"), 
              # toggleStripes=list(label="toglid", icon="leaf"),
              # toggleDots=T,
              # toggleIcons=T,
              # shinyTree("tree", search = "searchtxt",
              types =
                "{'#': { 'max_children' : -1, 'max_depth' : 3, 'use_data': true, 'valid_children' : ['root'] },
              'root' : {  'valid_children' : ['none'] },
              'root1' : {  'valid_children' : ['none'] },
              'root2' : {  'valid_children' : ['none'] }
  }", dragAndDrop = T, themeIcons = T,   themeDots =T)#,
    #plotlyOutput("plotly")
  ),
  verbatimTextOutput("click"),
  # verbatimTextOutput("selected"),
  # verbatimTextOutput("sess"),
  # verbatimTextOutput("sess1"),
## jstree events ###################
  tags$script(HTML("$('#tree').on('init.jstree', function (e, data) {
                      console.log('init');
                   });
                   ")),
  tags$script(HTML("$('#tree').on('dnd_move.vakata', function (e, data) {
                      console.log('dnd_move.vakata');
                   });
                   ")),
  tags$script(HTML("$('#tree').on('dnd_stop.vakata', function (e, data) {
                      console.log('dnd_stop.vakata');
                   });
                   ")),
  tags$script(HTML("$('#tree').on('changed.jstree', function (e, data) {
                      console.log('changed.jstree');
                   });
                   ")),
  tags$script(HTML("$('#tree').on('loading.jstree', function (e, data) {
                      console.log('loading');
                   });
                   ")),
  tags$script(HTML("$('#tree').on('loaded.jstree', function (e, data) {
                      console.log('loaded');
                   });
                   ")),
  tags$script(HTML("$('#tree').on('ready.jstree', function (e, data) {
                      console.log('ready');
                   });
                   ")),
  tags$script(HTML("$('#tree').on('load_node.jstree', function (e, data) {
                      console.log('load_node');
                   });
                   ")),
  tags$script(HTML("$('#tree').on('load_all.jstree', function (e, data) {
                      console.log('load_all');
                   });
                   ")),
  tags$script(HTML("$('#tree').on('destroy.jstree', function (e, data) {
                      console.log('destroy.jstree');
                   });
                   ")),
  tags$script(HTML("$('#tree').on('model.jstree', function (e, data) {
                      console.log('model');
                   });
                   ")),
  tags$script(HTML("$('#tree').on('enable_node.jstree', function (e, data) {
                      console.log('enable_node');
                   });
                   ")),
  tags$script(HTML("$('#tree').on('select_node.jstree', function (e, data) {
                      console.log('select_node');
                   });
                   ")),
  tags$script(HTML("$('#tree').on('show_contextmenu.jstree', function (e, data) {
                      console.log('show_contextmenu.jstree');
                   });
                   ")),
  tags$script(HTML("$('#tree').on('move_node.jstree', function (e, data) {
                      console.log('move_node.jstree');
                   });
                   ")),
  tags$script(HTML("$('#tree').on('search.jstree', function (e, data) {
                      console.log('search.jstree');
                   });
                   ")),
  tags$script(HTML("$('#tree').on('state_ready.jstree', function (e, data) {
                      console.log('state_ready.jstree');
                   });
                   ")),
  tags$script(HTML("$('#tree').on('clear_search.jstree', function (e, data) {
                      console.log('clear_search.jstree');
//$('#tree').jstree().refresh(false, true);
                   });
                   ")),
  tags$script(HTML("$('#tree').on('changed.jstree', function (e, data) {
                   var i, j, r = [];
                   for(i = 0, j = data.selected.length; i < j; i++) {
                   r.push(data.instance.get_node(data.selected[i]).text);
                   }
                   $('#event_result').html('Selected: ' + r.join(', '));
                   })
                   ")),
########################
## rot färben #####################
tags$script(HTML("function colorNodes(e, data) {
                 var chTot = e.currentTarget.children[0].children;
                 var myArr = ['PKW', 'LKW','PKWAE', 'SLZ'];
                 for (var i = 0; i < chTot.length; i++) {
                 var a = chTot[i].attributes.class.value;
                 if (a.indexOf('open') >= 0 == true) {
//console.log(chTot[i]);
//console.log(chTot[i].children);
                 var chiOpe = chTot[i].children[2].children;
                 for (var j = 0; j < chiOpe.length; j++) {
                 var chchOp = chiOpe[j];
                 if (chchOp.className.indexOf('open') >= 0 == true) {
                 if (chchOp.textContent.indexOf('Gesamtquerschnitt') >= 0 == true) {
//console.log('chchOp.children');
//console.log(chchOp.children);
                 var geQU = chchOp.children[2].children;
                 for (var m = 0; m < geQU.length; m++) {
                 var chGeQU = chchOp.children[2].children[m];
                 $('#' + chGeQU.id).css('color', 'red');
                 }} else {
                 var chchchOp = chchOp.children[2].children;
                 for (var k = 0; k < chchchOp.length; k++) {
console.log('chchOp.children');
console.log(chchOp.children);
console.log('chchOp.children[2]');
console.log(chchOp.children[2]);
console.log('chchOp.children[2].children[k]');
console.log(chchOp.children[2].children[k]);
console.log('chchOp.children[2].children[k].children[1]');
console.log(chchOp.children[2].children[k].children[1]);
                 var fzgg = chchOp.children[2].children[k].children[1];
                 if (myArr.indexOf(fzgg.text.replace(/\\s/g, '')) > -1) {
                 $('#' + fzgg.id).css('color', 'red');
                 }}}}}}}}")),



tags$script(HTML("$('#tree').on('clear_search.jstree', function(e, data) {
                    colorNodes(e, data);
                    $('#tree').jstree().refresh(true, true);
                 });")),
tags$script(HTML("$('#tree').on('state_ready.jstree open_node.jstree move_node.jstree', function(e, data) {
                    colorNodes(e, data);
                 });")),
#####################
#######################
## plotly events ##########
tags$script(HTML("
$('#plotly').on('plotly_selected', function (eventData) {
console.log('plotly was selected');
console.log(eventData);
});
$('#plotly').on('plotly_clicked', function (eventData) {
console.log('plotly was clicked');
console.log(eventData);
});


$('#plotly').on('plotly_click', function (data) {
console.log('plotly was click');
console.log(data);
console.log(data.currentTarget.data);
console.log(data.delegateTarget.data);
console.log(data.target.data);
console.log(data.target.data.length);
var last = data.target.data.length-1;
console.log(last);
console.log(data.target.data.length);
//console.log(data.currentTarget.data[last].x);
console.log(data.currentTarget.data[last].y);


//console.log(eventData.target.points[0].x);
});





$('#plotly').on('plotly_selecting', function (eventData) {
console.log('plotly was selecting');
console.log(eventData);
});
$('#plotly').on('plotly_relayout', function (eventData) {
console.log('plotly was relayout');
console.log(eventData);
});
//$('#plotly').on('plotly_hover', function (eventData) {
//console.log('plotly was hover');
//console.log(eventData);
//});

                 "))

  
  # verbatimTextOutput("treeTxt"),
  # verbatimTextOutput("root"),
  # verbatimTextOutput("dire"),
  # div(id="event_result"),
  # verbatimTextOutput("group")
)}

clickElem <- reactiveValues(a=NULL)
list_to_string <- function(obj, listname) {
  if (is.null(names(obj))) {
    paste(listname, "[[", seq_along(obj), "]] = ", obj,
          sep = "", collapse = "\n")
  } else {
    paste(listname, "$", names(obj), " = ", obj,
          sep = "", collapse = "\n")
  }
}

server <- function(input, output, session) {
  # session$onSessionEnded(stopApp)
  
  output$tree <- renderTree({
    tree <- tree[!is.na(tree$name),]
    tree <- tree[!tree$type == "",]
    # isolate({gettree(tree)})
    gettree(tree)
  })
  observeEvent(input$updateTree, {
  # observe( {
    # req(input$tree)
    req(input$change != "")

    if (input$change == "Names") {
      tree <- tree[order(tree$name),]
    } else if (input$change == "Id") {
      tree <- tree[order(tree$id),]
    }
    newtree <- gettree(tree)
    # browser()
    
    # updateTree(session, "tree", newtree, skipload=T, fortgetstate=T)
    updateTree(session, "tree", newtree)
  })
  observeEvent(input$runjs,{
    runjs(HTML('$("#tree").jstree("close_all");'))
  })
  
  ##-----------------------
  selRV <- reactiveValues(a = NULL)
  selectElem <- reactive({
    # req(length(get_selected(input$tree, "slices"))!=0)
    req(length( get_selected(input$tree, "names"))!=0)
    req(length(attr(get_selected(input$tree, "names")[[1]], "ancestry")) == 2)
    selectSlice <- get_selected(input$tree, "slices")[[1]]
    selectNames <- get_selected(input$tree, "names")[[1]]
    # req(length(attr(selectNames, "ancestry")) >= 1)
    
    if (length(attr(selectNames, "ancestry")) == 1) {
      a <- list(
        root = names(selectSlice),
        dire = names(selectSlice[[1]]),
        vehi = names(input$tree[[names(selectSlice)]][[names(selectSlice[[1]])]])
      )
    } else if (length(attr(selectNames, "ancestry")) == 2) {
      a <- list(
        root = names(selectSlice),
        dire = names(selectSlice[[1]]),
        vehi = selectNames[1]
      )
    }
    # selRV$a = a
    a
  })
  output$root <- renderPrint({
    selectElem()$root
    # req(selRV$a)
    # selRV$a$root
  })
  output$dire <- renderPrint({
    selectElem()$dire
    # req(selRV$a)
    # selRV$a$dire
  })
  output$group <- renderPrint({
    selectElem()$vehi
    # req(selRV$a)
    # selRV$a$vehi
  })
  ##-----------------------
  
  output$treeTxt <- renderPrint({
    req(input$tree)
    unlist(input$tree)
  })
  
  SOURCE <- reactiveValues(a="Src")
  output$plotly <- renderPlotly({
    # maxY = max(dfN$val)
    key <- highlight_key(dfN)
    p <- ggplot() +
      geom_col(data = key, aes(x = plotly:::to_milliseconds(time_stamp), y = val, fill=I(col),
                               text = paste("Datum: ", time_stamp, "<br>",
                                            "Verkehrsdichte: ", val)))
    
    
    DateLabels = dfN[which(lubridate::mday((dfN$time_stamp)) == 1),"time_stamp"]
    # dfN$timelabel = NULL
    # dfN$timelabel = as.Date("2018-01-01")
    # dfN[which(lubridate::mday((dfN$time_stamp)) == 1), timelabel := time_stamp]
    
    # ggplotly(p, source = SOURCE$a, tooltip = c("text"), dynamicTicks = "x") %>% 
    ggplotly(p, source = "Src", tooltip = c("text"), dynamicTicks = "x") %>% 
      layout(
        xaxis=list(fixedrange=F, tickmode="array", showline=T,
                   tickval = DateLabels$time_stamp, 
                   ticktext = DateLabels$time_stamp, 
                   autotick=FALSE,
                   automargin=T, showspikes=T, spikethickness=0),
        yaxis=list(fixedrange=F, automargin=F, tickmode="array")
      )%>%
      config(displayModeBar = T, collaborate = F, cloud = F) %>%
      style(hoverinfo = "text") %>%
      highlight(selectize=F, on = "plotly_click", off = "plotly_doubleclick",
                opacityDim = 0.2, selected = attrs_selected(opacity = 1), 
                persistent = F)
  })
  
  
  observe({
    d <- event_data("plotly_click", source = SOURCE$a, session = session)
    req(d)

    # isolate({
    # if (is.null(clickElem$a)) {
    #   clickElem$a = d
    # } else {
    #   print("asdadasf")
    #   clickElem$a <- rbind(d, clickElem$a)
    # }
    # })
    
    clickElem$a = d
  })
  observe({
    # d <- event_data("plotly_click", source = SOURCE$a, session = session)
    # req(d)
    # # req(SOURCE$a != "Src")
    # print(SOURCE$a)
    # SOURCE$a = sample(LETTERS, 1, F)
  })
  
  output$click <- renderPrint({
    req(clickElem$a)
    print("click")
    clickElem$a
  })
  
  output$selected <- renderPrint({
    d <- event_data("plotly_selected", source = "Src", session = session)
    # browser()
    print("selected")
    req(d)
    d
  })
  
  
  output$sess <- renderText({
    # Find the names of all the keys in clientData
    cnames <- names(session$clientData)
    
    # Apply a function to all keys, to get corresponding values
    allvalues <- lapply(cnames, function(name) {
      item <- session$clientData[[name]]
      if (is.list(item)) {
        list_to_string(item, name)
      } else {
        paste(name, item, sep=" = ")
      }
    })
    paste(allvalues, collapse = "\n")
  })
  output$sess1 <- renderPrint({
    session
  })
}



shinyApp(ui,server)
