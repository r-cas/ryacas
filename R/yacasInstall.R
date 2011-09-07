
yacasFile <- function(filename = c("yacas.exe", "scripts.dat", "R.ys"), 
   slash = c("\\", "/")) {
   stopifnot(.Platform$OS.type == "windows")
   whole.path <- function(path, defpath, deffile) {
      if (path == "") path <- defpath
      if (file.info(path)$isdir) 
         file.path(sub("[/\\]$", "", path), deffile) 
      else path
   }
   yacas.exe <- whole.path(Sys.getenv("YACAS_HOME"),
         system.file(package = "Ryacas", "yacdir"), "yacas.exe")
   fullname <- switch(match.arg(filename), yacas.exe = yacas.exe,
      scripts.dat = whole.path(Sys.getenv("YACAS_SCRIPTS"),
         dirname(yacas.exe), "scripts.dat"), 
      R.ys = whole.path(Sys.getenv("YACAS_INIT"),
	 system.file(package = "Ryacas", "yacdir"), "R.ys"))
   slash <- match.arg(slash)
   chartr(setdiff(c("/", "\\"), slash), slash, fullname)
}

yacasInstall <- function(url = 
   "http://ryacas.googlecode.com/files/yacas-1.0.63.zip",
   overwrite = FALSE) {
   stopifnot(.Platform$OS.type == "windows")
   tmpd <- tempdir()
   tmpz <- file.path(tmpd, basename(url))
   download.file(url, tmpz, mode = "wb")
   # zip.unpack(tmpz, tmpd)
   unzip(tmpz, overwrite = TRUE, junkpaths = TRUE, exdir = tmpd)
   files <- c("scripts.dat", "yacas.exe")
   lf <- function(f) list.files(path = tmpd, pattern = f,
      all.files = FALSE, full.names = TRUE, recursive = TRUE)
   for (f in files)
      if (length(lf(f)) ==0) stop(paste(f, "not found in", url))
   for (f in files) if (!file.copy(lf(f), yacasFile(f), overwrite = overwrite))
      warning(paste(yacasFile(f), "already exists.\n  Use overwrite = TRUE"))
   invisible()
}



