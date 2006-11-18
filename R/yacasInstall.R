yacasInstall <- function(showonly = FALSE, ..., filesize = 376832) {
   stopifnot(.Platform$OS.type == "windows")
   urlbase <- "http://ryacas.googlecode.com/svn/trunk/inst/yacdir"
   yacdir <- system.file(package = "Ryacas", "yacdir")
   files <- c("scripts.dat", "yacas.exe")
   lapply(files, function(f) if (showonly)
      cat("Download", file.path(urlbase, f), "\nto", file.path(yacdir, f), "\n")
   else
      download.file(file.path(urlbase, f), file.path(yacdir, f), 
         mode = "wb", ...) 
   )
   if (!showonly) {
      cmd <- paste("cmd /c dir /-c", 
          chartr("/", "\\", file.path(yacdir, "yacas.exe")))
      out <- system(cmd, intern = TRUE)
      out <- grep("yacas.exe", out, value = TRUE)
      infilesize <- tail(strsplit(out, " ")[[1]], 2)[1]
      if (!is.na(filesize)) {
      infilesize <- suppressWarnings(as.numeric(infilesize))
         if (is.na(infilesize) || infilesize != filesize) 
            warning("wrong yacas.exe")
      }
   }
}
