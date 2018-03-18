## All functions and the batch interfaces are in espa_client.R
## only prerequisites: install.packages(c("httr", "xml2", "compare", "tools")), espa_client.R will want to load them

client.dir <- "/home/someUser/Dokumente/wd/dev/espa_client/espa_client.R"
source(client.dir)


## Define API access arguments
api.user = "username"
api.passw = "password" #or giv getPass::getPass() as direct function input to hide password variable

## Define directories to files and folders
q.dir <- "/home/someUser/Dokumente/wd/dev/espa_client/order.txt" #directory to order, naming can be anything
arc.dir <- "/home/someUser/Dokumente/wd/dev/espa_client/archive/" #download folder


## Create PID (all generated files will be recognized by there common PID)
pid.dir <- paste0(paste0(head(strsplit(q.dir, "/")[[1]], n=-1), collapse = "/"), "/")
pid <- paste0("task_", gsub(":", "-", gsub(" ", "T", toString(Sys.time()))))


## Function calls
# There are three main level functions: espa.order, espa.status and espa.download. All can be either called directly
# or from bash command line like this: Rscript /dir/to/espa_client.R --espa.status /dir/to/task pid q.dir api.user api.passw
# first argument (--) defines one of the three functions

# epsa.order: Can be called directly from R instead bash, since it is only called once.
# Places order (will check it and if passed, prepare it and place it) and saves the order IDs of the task to a .txt
espa.order(pid.dir = pid.dir, pid = pid, q.dir = q.dir, api.user = api.user, api.passw = api.passw) #or:
# system(paste("Rscript", client.dir, "--espa.order", pid.dir, pid, q.dir, api.user, api.passw), wait = T)
# optional appendable with q.product, q.format (arguments 7 and 8), defaults to "sr" and "gtiff"

# espa.status: 
# start it as child process, so that it runs until it terminates itself
system(paste("Rscript", client.dir, "--espa.status", pid.dir, pid, arc.dir, api.user, api.passw), wait = F)

# espa.download
# will iterate itself as well but since nothing comes after, no child required and you can call it directly
espa.download(pid.dir = pid.dir, pid = pid, arc.dir = arc.dir, api.user = api.user, api.passw = api.passw) #or
# system(paste("Rscript", client.dir, "--espa.download", pid.dir, pid, arc.dir, api.user, api.passw), wait = T)