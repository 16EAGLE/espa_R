load.lib <- function(x){
  y <- lapply(x, function(p){
    t <- try(library(package = p, character.only = T), silent = T)
    if(class(t) == "try-error") stop(paste0("'", p, "' is required."))
  })
}

## GET function
espa.get <- function(url.exp = "", api.props = api, v = verbose){
  x <- GET(paste0(api.props$url, "/v0/", url.exp), authenticate(api.props$user, api.props$pass))
  stop_for_status(x, "connect to ESPA API. Please retry.")
  warn_for_status(x)
  if(isTRUE(v)){ message_for_status(x); cat("\n")}
  return(x)
}

## POST function
espa.post <- function(url.exp = "", body = FALSE, api.props = api, v = verbose){
  x <- POST(paste0(api.props$url, "/v0/", url.exp), authenticate(api.props$user, api.props$pass), body = body)
  stop_for_status(x, "connect to ESPA API. Please retry.")
  warn_for_status(x)
  if(isTRUE(v)){message_for_status(x); cat("\n")}
  return(x)
}

## order function
espa.order <- function(pid.dir, pid, q.dir, api.user, api.passw, q.product = "sr", q.format = "gtiff", api.url = "https://espa.cr.usgs.gov/api", verbose = T, sign = "[espa.order]:"){
  
  ## initialize
  pid.dir <- paste0(pid.dir, "/", pid, ".txt")
  api <- list(url = api.url, user = api.user, pass = api.passw)
  query <- list(dir = q.dir, product = q.product, format = q.format)
  ini <- espa.get(api.props = api, v = F)
  
  ## check query and abort, if not available
  q.id <- readLines(query$dir)
  q.check <- lapply(q.id , function(id, v = verbose){
    r <- espa.get(paste0("available-products/", id), api, v = F)
    if(names(content(r)) == "not_implemented") stop(paste0(toString(Sys.time()), " ", sign, " '", id, "': This ID is invalid, as it cannot be found in the ESPA database. Please remove it from input and reexecute."))
    list(id, r)
  })
  
  ## group request by collection (single or multi order)
  req.data <- lapply(q.check, function(x) c(names(content(x[[2]])), x[[1]]))
  coll <- sapply(req.data, function(x) x[[1]], USE.NAMES=F)
  coll.uni <- unique(coll)
  if(isTRUE(verbose)) cat(toString(Sys.time()), " ", sign, " Collecting from ", toString(length(coll.uni)), " collection(s) [", paste0(coll.uni, collapse = ", "), "], resulting in ", toString(length(coll.uni)), " orders...",  sep = "")
  req.coll <- lapply(coll.uni, function(x, c = coll, rd = req.data) rd[which(c == x)])
  
  ## build request
  req.body <- lapply(req.coll, function(x, p = query$product, f = query$format){
    i <- paste0(sapply(x, function(y) y[2], USE.NAMES = F), collapse = '", "')
    paste0('{"', x[[1]][1], '": { "inputs": ["', i, '"], "products": ["', p, '"]}, "format": "', f, '"}')
  })
  #req.body <- lapply(req.data, function(x, p = query$product, f = query$format) paste0('{"', x[1], '": { "inputs": ["', x[2], '"], "products": ["', p, '"]}, "format": "', f, '"}'))
  
  ## order
  order <- lapply(req.body, function(x, a = api) espa.post(url.exp = paste0("order/"), body = x, api.props = a, v = F))
  if(isTRUE(verbose)) cat(toString(Sys.time()), " ", sign, " Products '", paste0(query$uuid, collapse = "', '"), " have been ordered successfully [product = '", query$product, ", format = '", query$format, "'].", sep = "")
  writeLines(sapply(order, function(x) content(x)[[1]], USE.NAMES = F), pid.dir)
}

## server item status function
espa.status <- function(pid.dir, pid, arc.dir, api.user, api.passw, api.url = "https://espa.cr.usgs.gov/api", sleep = 5, verbose = T, sign = "[espa.status]:"){
  
  ## initialize
  if(isTRUE(verbose)) cat(toString(Sys.time()), sign, "Starting...\n")
  run <- TRUE
  api <- list(url = api.url, user = api.user, pass = api.passw)
  order.id <- readLines(paste0(pid.dir, "/", pid, ".txt"))
  pid.dir <- paste0(pid.dir, "/", pid, ".csv")
  while(isTRUE(run)){ # run until all orders are complete and downloaded
    
    ## query server for all ordered items
    #if(isTRUE(verbose)) cat(toString(Sys.time()), sign, "Checking orders and filter items by requested orders...", "\n", sep = " ")
    order.query <- espa.get("list-orders", api, v = F)
    order.list <- strsplit(gsub("[]]", "", gsub("[[]", "", gsub('[\"]',"", xml_text(content(order.query)) ))), ", ")[[1]]
    order.status <- lapply(order.list, function(x, a = api) espa.get(paste0("order-status/", x), a,  v = F))
    
    items <- lapply(order.list, function(x, a = api){
      y <- espa.get(paste0("item-status/", x), a, v = F)
      status <- content(y)
    })
    
    items <- lapply(items, function(x) lapply(x[[1]], function(y){
      r <- unlist(y)
      names(r) <- names(y)
      return(r)
    }))
    items <- data.frame(do.call(rbind, lapply(items, function(x) do.call(rbind, lapply(x, function(y) rbind(y))))), row.names = NULL, check.names = F, fix.empty.names = F)
    
    
    ## select relevant items for this task (get rid off old orders)
    sub.items <- unlist(lapply(order.id, function(x, i = items) grep(x, i$product_dload_url)))
    if(length(sub.items) != 0){
      items.order <- items[sub.items,]
      
      
      ## fuse local and order status
      if(file.exists(pid.dir)){
        #if(isTRUE(verbose)) cat(toString(Sys.time()), sign, "Found task file, checking, if items need to be added...", "\n", sep = " ")
        local.status <- read.csv2(pid.dir, row.names = 1)
        missing.sub <- sapply(items.order$product_dload_url, function(x, lst.url = local.status$product_dload_url) if(is.na(match(x, lst.url))) return(TRUE) else return(FALSE), USE.NAMES = F)
        if(any(missing.sub)){
          missing.items <- cbind(items.order[missing.sub,], FALSE, FALSE)
          n.cols <- length(colnames(missing.items))
          colnames(missing.items)[seq(n.cols-1, n.cols)] <- c("server.avail", "local.avail")
          local.status <- rbind(local.status, missing.items)
        }
        row.names(local.status) <- seq(1, length(local.status[,1]))
        
      } else {
        if(isTRUE(verbose)) cat(toString(Sys.time()), sign, "Initializing task file from requested orders...", "\n", sep = " ")
        local.status <- items.order
        n.cols <- length(colnames(local.status))
        local.status[,n.cols+1] <- F
        local.status[,n.cols+2] <- F
        colnames(local.status) <-  c(colnames(local.status)[1:n.cols], "server.avail",  "local.avail")
      }
      
      
      ## check online availabilty
      #if(isTRUE(verbose)) cat(toString(Sys.time()), sign, "Checking server and local item status...", "\n", sep = " ")
      local.status$server.avail <- sapply(local.status$status, function(x) if(x != "complete") F else T, USE.NAMES = T)
      
      ## check local availability
      file.dir <- sapply(as.character(local.status$product_dload_url), function(x, d = arc.dir) paste0(d, "/", tail(strsplit(x, "/")[[1]], n=1)), USE.NAMES = F)
      local.status$local.avail  <- sapply(file.dir, file.exists, USE.NAMES = F)
      write.csv2(local.status, pid.dir)
      
      ## check stopping condition
      if(isTRUE(verbose)) cat(toString(Sys.time()), " ", sign, " ", length(which(local.status$server.avail == T)), " items are ready to download, ", length(which(local.status$local.avail == T)), "/", length(local.status$local.avail), " have been downloaded.", "\n", sep = "")
      if(all(local.status$server.avail) & all(local.status$local.avail)) {
        if(isTRUE(verbose)) cat(toString(Sys.time()), sign, "Done, qutting task.", "\n", sep = " ")
        run <- FALSE
      }else{
        #if(isTRUE(verbose)) cat(toString(Sys.time()), sign, "Updating status in", toString(sleep), "seconds...", "\n", sep = " ")
        Sys.sleep(sleep)
      }
    } else{
      if(isTRUE(verbose)) cat(toString(Sys.time()), sign, "Waiting for server to assign order IDs to items...", "\n", sep = " ")
      Sys.sleep(sleep)
    }
  }
}

## item download function
espa.download <- function(pid.dir, pid, arc.dir, api.user, api.passw, api.url = "https://espa.cr.usgs.gov/api", sleep = 10, max.iter = 60, verbose = T, sign = "[espa.download]:"){
  
  run <- T; skip <- F
  timeout.count <- 0; n.downloads <- 0
  api <- list(url = api.url, user = api.user, pass = api.passw)
  pid.dir <- paste0(pid.dir, "/", pid, ".csv")
  while(isTRUE(run)){ # run until all orders are complete and downloaded
    
    sub.download <- NULL
    if(file.exists(pid.dir)){
      task <- read.csv2(pid.dir)
      sub.needed <- which(task$local.avail == F)

      if(length(sub.needed) != 0){
        sub.ready <- which(task$server.avail == T)
        sub.download <- intersect(which(task$server.avail == T), which(task$local.avail == F))
      }
    }
    if(!file.exists(pid.dir) | length(sub.download) == 0){
      if(isTRUE(verbose)) cat(toString(Sys.time()), sign, "No available downloads, waiting for server to deliver...", "\n", sep = " ")
      timeout.count <- timeout.count+1
      skip <- T
    } else { skip <- F}
    
    ## if there is something to donload, do it, otherwise either stop or recycle
    if(!isTRUE(skip) & isTRUE(run)){
      
      down <- lapply(seq(1, length(task$name[sub.download])), function(i, t = task[sub.download,], d = arc.dir){
        
        if(isTRUE(verbose)) cat(toString(Sys.time()), " ", sign, " Recieving MD5 check sums for '", as.character(t$name)[i],"'...", "\n", sep = "")
        md5 <- strsplit(content(GET(as.character(t$cksum_download_url)[i]), as = "text", encoding = "UTF-8"), " ")[[1]][1]
        
        file.dir <- paste0(d, "/", tail(strsplit(as.character(t$product_dload_url)[i], "/")[[1]], n=1))
        file.tmp <- tempfile(tmpdir = arc.dir, fileext = ".tar.gz") #download to cryptical find so that it is not indexed until finished
        if(isTRUE(verbose)) cat(toString(Sys.time()), " ", sign, " Downloading '", as.character(t$name)[i],"'...", "\n", sep = "")
        try(GET(as.character(t$product_dload_url)[i], write_disk(file.tmp)))
        
        if(isTRUE(verbose)) cat(toString(Sys.time()), " ", sign, " Comparing MD5 check sums for '", as.character(t$name)[i],"'...", "\n", sep = "")
        if(as.character(md5sum(file.tmp)) == md5){
          file.rename(file.tmp, file.dir)
          n.downloads <- n.downloads+1
        } else{
          if(isTRUE(verbose)) warning(paste0(toString(Sys.time()), " ", sign, " Downloading of '", as.character(t$name)[i],"' failed, MD5 check sums do not match. Recycling..."), call. = F)
          file.remove(file.tmp)
        }
      })
    }
    if(timeout.count >= max.iter & n.downloads > 0){
      if(isTRUE(verbose)) cat(toString(Sys.time()), sign, "Timeout, assuming no more pending processing on server.", "\n", sep = " ")
      run = FALSE
    } else{
      #if(isTRUE(verbose)) cat(toString(Sys.time()), sign, "Checking...", "\n", sep = " ")
      Sys.sleep(sleep)
    }
  }
}


## determine if espa_API is called directly from command line
args = commandArgs(trailingOnly=TRUE)
suppressMessages(load.lib(c("httr", "xml2", "compare", "tools")))
if(length(args) > 0){
  if(args[1] == "--espa.order"){
    # if command line use, supply arguments in this order: --espa.order pid.dir pid q.dir api.user api.passw
    espa.order(pid.dir = args[2], pid = args[3], q.dir = args[4], api.user = args[5], api.passw = args[6],
               q.product = if(!is.na(args[7])) args[7] else "sr",
               q.format = if(!is.na(args[8]))  args[8] else "gtiff")
  }
  
  if(args[1] == "--espa.status"){
    # if command line use, supply arguments in thi order: --espa.status pid.dir pid, arc.dir, arc.dir api.user api.passw
    espa.status(pid.dir = args[2], pid = args[3], arc.dir = args[4], api.user = args[5], api.passw = args[6])
  }
  
  if(args[1] == "--espa.download"){
    # if command line use, supply arguments in this order: --espa.status pid.dir pid, arc.dir, arc.dir api.user api.passw
    espa.download(pid.dir = args[2], pid = args[3], arc.dir = args[4], api.user = args[5], api.passw = args[6])
  }
}