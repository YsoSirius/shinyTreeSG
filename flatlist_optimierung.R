library(compiler)
load(file = 
 "C:/Users/Bobo/Documents/TraffiCon/VDE_TIROL/VDE/VDE_Tirol_neu/nestedList.rda")
keys_c <- c("icon","type","names","stopened","stselected","sttype")

get_flatList <- function(nestedList, flatList = NULL, parent = "#") {

  for (name in names(nestedList)) {
    additionalAttributes <- list(
      "icon" = attr(nestedList[[name]],"sticon"),
      "type" = attr(nestedList[[name]],"sttype")
    )
    
    additionalAttributes <- additionalAttributes[
      !unlist(lapply(additionalAttributes,is.null))]
    
    data <- lapply(names(attributes(nestedList[[name]])), function(key){
      if (key %in% c("icon","type","names","stopened",
                     "stselected","sttype")) {
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
      flatList = get_flatList(nestedList[[name]], 
                              flatList, 
                              parent = as.character(length(flatList)))
    }
  }
  flatList
}
get_flatList1 <- function(nestedList, flatList = NULL, parent = "#") {
  for (name in names(nestedList)) {
    additionalAttributes <- list(
      "icon" = attr(nestedList[[name]],"sticon"),
      "type" = attr(nestedList[[name]],"sttype")
    )
    
    additionalAttributes <- additionalAttributes[
      !unlist(lapply(additionalAttributes,is.null))]
    
    na_attr <- names(attributes(nestedList[[name]]))
    data <- lapply(na_attr, function(key){
      if (key %in% c("icon","type","names","stopened",
                     "stselected","sttype")) {
        NULL
      }else{
        attr(nestedList[[name]],key)
      }
    })
    
    if (!is.null(data) && length(data) > 0) {
      names(data) <- na_attr
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
      flatList = get_flatList1(nestedList[[name]], 
                              flatList, 
                              parent = as.character(length(flatList)))
    }
  }
  flatList
}
get_flatList2 <- function(nestedList, flatList = NULL, parent = "#") {
  for (name in names(nestedList)) {
    nst_name <- nestedList[[name]]
    
    additionalAttributes <- list(
      "icon" = attr(nst_name,"sticon"),
      "type" = attr(nst_name,"sttype")
    )
    
    additionalAttributes <- additionalAttributes[
      !unlist(lapply(additionalAttributes,is.null))]
    
    na_attr <- names(attributes(nst_name))
    data <- lapply(na_attr, function(key){
      if (key %in% c("icon","type","names","stopened",
                     "stselected","sttype")) {
        NULL
      }else{
        attr(nst_name,key)
      }
    })
    
    if (!is.null(data) && length(data) > 0) {
      names(data) <- na_attr
      data <- data[!unlist(lapply(data,is.null))]
    }
    
    nodeData <- c(
      list(
        id = as.character(length(flatList) + 1),
        text = name,
        parent = parent,
        state = list(
          opened   = isTRUE(attr(nst_name, "stopened")),
          selected = isTRUE(attr(nst_name, "stselected"))
        ),
        data = data
      ),
      additionalAttributes
    )

    flatList = c(flatList,list(nodeData))
    if (is.list(nst_name)) {
      flatList = get_flatList2(nst_name, 
                              flatList, 
                              parent = as.character(length(flatList)))
    }
  }
  flatList
}
get_flatList3 <- function(nestedList, flatList = NULL, parent = "#") {
  for (name in names(nestedList)) {
    nst_name <- nestedList[[name]]
    
    additionalAttributes <- list(
      "icon" = attr(nst_name,"sticon"),
      "type" = attr(nst_name,"sttype")
    )
    
    additionalAttributes <- additionalAttributes[
      !unlist(lapply(additionalAttributes,is.null))]
    
    na_attr <- names(attributes(nst_name))
    data <- lapply(na_attr, function(key){
      if (any(key == c("icon","type","names","stopened",
                     "stselected","sttype"))) {
        NULL
      }else{
        attr(nst_name,key)
      }
    })

    if (!is.null(data) && length(data) > 0) {
      names(data) <- na_attr
      data <- data[!unlist(lapply(data,is.null))]
    }

    nodeData <- c(list(
        id = as.character(length(flatList) + 1),
        text = name,
        parent = parent,
        state = list(
          opened   = isTRUE(attr(nst_name, "stopened")),
          selected = isTRUE(attr(nst_name, "stselected"))
        ),
        data = data
      ),
      additionalAttributes
    )

    flatList = c(flatList,list(nodeData))
    if (is.list(nst_name)) {
      flatList = get_flatList3(nst_name, 
                              flatList, 
                              parent = as.character(length(flatList)))
    }
  }
  flatList
}
get_flatList4 <- function(nestedList, flatList = NULL, parent = "#") {
  for (name in names(nestedList)) {
    nst_name <- nestedList[[name]]
    
    additionalAttributes <- list(
      "icon" = attr(nst_name,"sticon"),
      "type" = attr(nst_name,"sttype")
    )
    
    additionalAttributes <- additionalAttributes[
      !unlist(lapply(additionalAttributes,is.null))]
    
    na_attr <- names(attributes(nst_name))
    data <- lapply(na_attr, function(key){
      if (any(key == keys_c)) {
        NULL
      }else{
        attr(nst_name,key)
      }
    })

    if (!is.null(data) && length(data) > 0) {
      names(data) <- na_attr
      data <- data[!unlist(lapply(data,is.null))]
    }

    nodeData <- c(list(
        id = as.character(length(flatList) + 1),
        text = name,
        parent = parent,
        state = list(
          opened   = isTRUE(attr(nst_name, "stopened")),
          selected = isTRUE(attr(nst_name, "stselected"))
        ),
        data = data
      ),
      additionalAttributes
    )

    flatList = c(flatList,list(nodeData))
    if (is.list(nst_name)) {
      flatList = get_flatList4(nst_name, 
                              flatList, 
                              parent = as.character(length(flatList)))
    }
  }
  flatList
}
get_flatList5 <- cmpfun(function(nestedList, flatList = NULL, parent = "#") {
  for (name in names(nestedList)) {
    nst_name <- nestedList[[name]]
    
    additionalAttributes <- list(
      "icon" = attr(nst_name,"sticon"),
      "type" = attr(nst_name,"sttype")
    )
    
    additionalAttributes <- additionalAttributes[
      !unlist(lapply(additionalAttributes,is.null))]
    
    na_attr <- names(attributes(nst_name))
    data <- lapply(na_attr, function(key){
      if (any(key == keys_c)) {
        NULL
      }else{
        attr(nst_name,key)
      }
    })

    if (!is.null(data) && length(data) > 0) {
      names(data) <- na_attr
      data <- data[!unlist(lapply(data,is.null))]
    }

    nodeData <- c(list(
        id = as.character(length(flatList) + 1),
        text = name,
        parent = parent,
        state = list(
          opened   = isTRUE(attr(nst_name, "stopened")),
          selected = isTRUE(attr(nst_name, "stselected"))
        ),
        data = data
      ),
      additionalAttributes
    )

    flatList = c(flatList,list(nodeData))
    if (is.list(nst_name)) {
      flatList = get_flatList5(nst_name, 
                              flatList, 
                              parent = as.character(length(flatList)))
    }
  }
  flatList
})
get_flatList6 <- function(nstl, fl = NULL, pr = "#") {
  for (name in names(nstl)) {
    nstnm <- nstl[[name]]
    
    adatr <- list(
      "icon" = attr(nstnm,"sticon"),
      "type" = attr(nstnm,"sttype")
    )
    
    adatr <- adatr[!unlist(lapply(adatr,is.null))]
    
    natr <- names(attributes(nstnm))
    data <- lapply(natr, function(key){
      if (any(key == keys_c)) {
        NULL
      }else{
        attr(nstnm,key)
      }
    })

    if (!is.null(data)&&length(data) > 0) {
      names(data) <- natr
      data <- data[!unlist(lapply(data,is.null))]
    }

    nd <- c(list(
        id = as.character(length(fl) + 1),
        text = name,
        parent = pr,
        state = list(
          opened   = isTRUE(attr(nstnm, "stopened")),
          selected = isTRUE(attr(nstnm, "stselected"))
        ),
        data = data
      ),
      adatr
    )

    fl = c(fl,list(nd))
    if (is.list(nstnm)) {
      fl = get_flatList6(nstnm, 
                         fl, 
                         pr=as.character(length(fl)))
    }
  }
  fl
}
get_flatList7 <- function(nstl, fl = NULL, pr = "#") {
  for (name in names(nstl)) {
    nstnm <- nstl[[name]]
    
    adatr <- list(
      "icon" = attr(nstnm,"sticon"),
      "type" = attr(nstnm,"sttype")
    )
    
    adatr <- adatr[!vapply(adatr, is.null, TRUE)]
    
    natr <- names(attributes(nstnm))
    data <- lapply(natr, function(key){
      if (any(key == keys_c)) {
        NULL
      }else{
        attr(nstnm,key)
      }
    })

    if (!is.null(data)&&length(data) > 0) {
      names(data) <- natr
      data <- data[!vapply(data,is.null, TRUE)]
    }

    len = as.character(length(fl) + 1)
    nd <- c(list(
        id = len,
        text = name,
        parent = pr,
        state = list(
          opened   = isTRUE(attr(nstnm, "stopened")),
          selected = isTRUE(attr(nstnm, "stselected"))
        ),
        data = data
      ),
      adatr
    )

    fl = c(fl,list(nd))
    if (is.list(nstnm)) {
      fl = get_flatList7(nstnm, 
                         fl, 
                         pr=len)
    }
  }
  fl
}
get_flatList8 <- function(nstl, fl = NULL, pr = "#") {
  for (name in names(nstl)) {
    nstnm <- nstl[[name]]

    typ = attr(nstnm,"sttype")
    ico = attr(nstnm,"sticon")
    if (is.null(typ)) {
      adatr <- list("icon" = ico)
    } else {
      adatr <- list("icon" = ico,"type" = typ)
    }

    data = list(ico)
    names(data) <- "sticon"

    len = as.character(length(fl) + 1)
    nd <- c(list(
        id = len,
        text = name,
        parent = pr,
        state = list(
          opened   = isTRUE(attr(nstnm, "stopened")),
          selected = isTRUE(attr(nstnm, "stselected"))
        ),
        data = data
      ),
      adatr
    )

    fl = c(fl,list(nd))
    if (is.list(nstnm)) {
      fl = get_flatList8(nstnm, fl, pr=len)
    }
  }
  fl
}
get_flatList7_print <- function(nstl, fl = NULL, pr = "#") {
  for (name in names(nstl)) {
    nstnm <- nstl[[name]]
    cat("\n\n")
    print(paste("name",name))
    
    adatr <- list(
      "icon" = attr(nstnm,"sticon"),
      "type" = attr(nstnm,"sttype")
    )
    
    adatr <- adatr[!vapply(adatr, is.null, TRUE)]
    print(paste("adatr",unlist(adatr)))
    
    natr <- names(attributes(nstnm))
    data <- lapply(natr, function(key){
      if (any(key == keys_c)) {
        NULL
      }else{
        attr(nstnm,key)
      }
    })
    print(paste("data",unlist(data)))
    
    if (!is.null(data)&&length(data) > 0) {
      names(data) <- natr
      data <- data[!vapply(data,is.null, TRUE)]
    }
    print(paste("data",data))

    nd <- c(list(
        id = as.character(length(fl) + 1),
        text = name,
        parent = pr,
        state = list(
          opened   = isTRUE(attr(nstnm, "stopened")),
          selected = isTRUE(attr(nstnm, "stselected"))
        ),
        data = data
      ),
      adatr
    )
    print(paste("len1", as.character(length(fl) + 1)))
    
    fl = c(fl,list(nd))
    if (is.list(nstnm)) {
      print(paste("len2", as.character(length(fl))))
      cat("\n\n")
      fl = get_flatList7_print(nstnm, 
                         fl, 
                         pr=as.character(length(fl)))
      print("END OR??")
      cat("\n\n")
    }
  }
  fl
}

invisible(get_flatList7_print(nstlist[1]))
invisible(get_flatList7_print(nstlist[[1]]))


get_flatList_test <- function(nstl, fl = NULL, pr = "#") {
# nstl = nstlist[1]; fl = NULL; pr ="#"
nstl = nstlist[[1]]; fl = NULL; pr ="#"
nstl= nstnm; fl=fl;pr=as.character(length(fl))
# nstl = nstlist[[1]]; fl = NULL; pr ="#"

  for (name in names(nstl)) {
names(nstl)
# name="8244 - Absam"
# name="Gesamtquerschnitt"
name="Fahrtrichtung 1 - Innsbruck (B 171)"
name="KFZ"
name="LKWAE"
name="PKWAE"
name="PAB-LKWOA"
name="SLZ"
name="Fahrtrichtung 1 - Innsbruck (B 171)"
    
    nstnm <- nstl[[name]]
nstnm
    
    adatr <- list(
      "icon" = attr(nstnm,"sticon"),
      "type" = attr(nstnm,"sttype")
    )
adatr

    data = list(attr(nstnm,"sticon"))
    names(data) <- "sticon"
data
    
    nd <- c(list(
        id = as.character(length(fl) + 1),
        text = name,
        parent = pr,
        state = list(
          opened   = isTRUE(attr(nstnm, "stopened")),
          selected = isTRUE(attr(nstnm, "stselected"))
        ),
        data = data
      ),
      adatr
    )
nd
# fl
str(fl)
    fl = c(fl,list(nd))
# fl
str(fl)
is.list(nstnm)

    if (is.list(nstnm)) {
      fl = get_flatList6(nstnm, 
                         fl, 
                         pr=as.character(length(fl)))
    }
  }
  fl
}

pr="#"
add_attrs <- function(lst, id, text, parent) {
  typ = attr(lst,"sttype")
  ico = attr(lst,"sticon")
  if (is.null(typ)) {
    adatr <- list("icon" = ico)
  } else {
    adatr <- list("icon" = ico,
                  "type" = typ)
    
  }
  data = list(ico)
  names(data) <- "sticon"
  c(list(id = id, text = text, parent = parent,
         state = list(opened=isTRUE(attr(lst,"stopened")), 
                      selected=isTRUE(attr(lst,"stselected"))
         ), data = data),adatr)
}
flatlist <- function(nstl) {
  ## Inits ##########
  id = 1; pr = "#"
  
  ## Detektor ####################
  detlistout <- list()
  for (name in names(nstl)) {
    detlst <- nstl[[name]]
    ## Fahrtrichtungen ####################
    frlistout <- list()
    pr1 = as.character(id)
    for (fr in names(detlst)) {
      id = id + 1
      frlst <- detlst[[fr]]
      ## Vehicles ####################
      vehlistout <- list()
      pr2 = as.character(id)
      for (veh in names(frlst)) {
        # browser()
        id = id + 1
        vehlst <- frlst[[veh]]
        nd <- add_attrs(vehlst, as.character(id), veh, pr2)
        vehlistout[[veh]] <- nd
      }
      names(vehlistout) <- NULL
      ####################
      nd <- add_attrs(frlst, as.character(id), fr, pr1)
      frlistout[[fr]] <- c(list(nd), vehlistout)
    }
    names(frlistout)<-NULL
    frlistout <- unlist(frlistout, recursive = FALSE)
    ####################
    nd <- add_attrs(detlst, as.character(id), name, pr)
    detlistout[[fr]] <- c(list(nd), frlistout)
  }
  names(detlistout)<-NULL
  unlist(detlistout, recursive = FALSE)
}

str(get_flatList8(nstlist[1]))
str(flatlist(nstlist[1]))
####################

mc <- microbenchmark::microbenchmark(
  a=get_flatList8(nstlist[1]),
  b=flatlist(nstlist[1])
); mc

a<-get_flatList1(nstlist[[1]])
b<-get_flatList2(nstlist[[1]])
c<-get_flatList3(nstlist[[1]])
d<-get_flatList4(nstlist[[1]])
e<-get_flatList6(nstlist[[1]])
f<-get_flatList7(nstlist[[1]])
g<-get_flatList8(nstlist[[1]])
identical(a,b);identical(a,c);identical(a,d);identical(a,e);identical(a,f)
identical(f,g)
all.equal(f,g)


my_check <- function(values) {all(sapply(values[-1], function(x) identical(values[[1]], x)))}
mc <- {microbenchmark::microbenchmark(
  check = my_check, times=30,
  a=get_flatList(nstlist),
  b=get_flatList1(nstlist),
  c=get_flatList2(nstlist),
  d=get_flatList3(nstlist),
  e=get_flatList4(nstlist),
  f=get_flatList5(nstlist),
  g=get_flatList6(nstlist),
  h=get_flatList7(nstlist),
  i=get_flatList8(nstlist)
)};mc

Unit: milliseconds
expr      min       lq     mean   median       uq      max neval cld
a 303.3398 349.5506 381.8709 382.1019 405.0946 481.1994    30   c
b 299.3725 345.7200 381.5133 381.2650 393.3386 721.7987    30   c
c 239.5002 285.3532 312.0663 317.4375 328.6037 395.7121    30  b 
d 223.2593 242.3046 283.0079 295.7432 312.1671 335.6241    30  b 
e 206.7501 241.8370 283.6779 281.5273 306.4480 550.2833    30  b 
f 217.9341 279.7485 308.5030 299.5537 314.6312 664.3810    30  b 
g 218.2166 283.3742 293.4572 297.8847 305.0948 420.4022    30  b 
h 225.7168 270.2575 280.4266 284.3909 296.8511 332.4914    30  b 
i 120.3667 158.8939 166.2474 167.8651 173.5688 237.4099    30 a  


## OLDER MCS ##########################
Unit: milliseconds
expr      min       lq     mean   median       uq      max neval cld
g 211.7771 232.9104 268.8297 254.1616 287.3840 603.2662    60   a
h 203.4056 236.8417 255.1907 254.9883 277.7981 331.3252    60   a

Unit: milliseconds
expr      min       lq     mean   median       uq      max neval cld
g 225.1835 242.0043 267.9474 259.2199 288.4026 553.9393    60   a
h 208.7329 220.4234 256.1859 253.6049 270.8254 526.5140    60   a

Unit: milliseconds
expr      min       lq     mean   median       uq      max neval cld
b 298.8569 312.8353 347.0959 346.0788 361.4719 619.1082    60   b
c 243.6199 251.8725 283.0349 282.3232 299.5844 558.2439    60  a 
d 229.9464 240.2894 281.7804 273.2734 288.6599 576.0796    60  a 
e 219.7525 238.0774 279.1440 268.8752 287.0718 567.3022    60  a 
g 221.5768 240.9046 264.3123 269.3083 285.2580 309.8458    60  a 

Unit: milliseconds
expr      min       lq     mean   median       uq      max neval cld
a 308.5783 351.5276 371.1453 375.4480 391.8877 426.9593    60   b
b 297.5948 334.1252 372.8755 363.0729 379.0173 700.9937    60   b
c 249.7058 279.7174 302.1690 300.5573 313.9212 606.6108    60  a 
d 226.9356 253.6527 289.1377 282.0171 305.9344 613.8642    60  a 
e 230.8413 248.5326 277.4947 276.5106 295.8465 574.8738    60  a 
f 230.7998 256.2031 297.1894 285.5870 302.6250 618.7213    60  a 

Unit: milliseconds
expr      min       lq     mean   median       uq      max neval cld
a 300.5434 344.3438 379.9006 375.7383 388.6548 664.7020    30   b
b 289.1996 340.4032 353.1924 359.8121 368.8938 435.5676    30   b
c 226.7504 270.7057 294.9654 297.0074 316.5782 368.9698    30  a 
d 222.1983 275.0880 293.6922 302.8237 312.9123 369.1371    30  a 
e 214.8257 262.7312 278.4983 284.5066 297.2544 354.1392    30  a 

Unit: milliseconds
expr      min       lq     mean   median       uq      max neval cld
a 306.9495 337.8879 365.7465 370.0610 386.0957 457.4316    30   b
b 292.9507 337.9000 372.7599 371.8071 385.2023 627.9823    30   b
c 241.0468 286.8896 298.0790 296.3430 320.7916 356.3254    30  a 
d 233.8384 263.2660 298.0528 283.4005 315.2000 584.3909    30  a 

Unit: milliseconds
expr      min       lq     mean   median       uq      max neval cld
a 296.8117 337.1136 361.7185 356.2113 378.0786 584.1768    60   b
b 286.6293 325.1527 362.2256 358.5484 379.3573 675.1704    60   b
c 233.0779 255.7253 276.8167 268.8953 291.8993 510.1151    60  a 

Unit: milliseconds
expr      min       lq     mean   median       uq      max neval cld
a 294.9348 319.0306 341.0443 341.2192 357.1469 398.8870    20   a
b 295.0136 314.6847 328.1818 329.6684 338.0381 359.7605    20   a


library(profvis)

profvis({
  get_flatList2(nstlist)
})