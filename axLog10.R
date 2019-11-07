axLog10 <- function(side,usr=NULL,plab=TRUE,ax29=FALSE,prug=FALSE,...) {
    if (!(side <- as.integer(side)) %in% 1L:4L) stop("'side' must be in {1:4}")
    is.x <- side%%2 == 1
    XY <- function(ch) paste(if (is.x) "x"else "y",ch,sep="")
    log <- par(XY("log"))
    if (!log) stop(paste(XY("log"),"must be TRUE"))
    axp <- par(XY("axp"))
    if (is.null(usr)) usr <- par("usr")[if (is.x) 1L:2L else 3L:4L]
    if (plab) {
        lat1 <- as.integer(round(log10(axTicks(side))))
        axis(side,at=10^lat1,label=parse(text=paste("10^",lat1,sep="")),las=1,tick=FALSE,...)
    }
    ax <- as.integer(round(log10(axp[1L:2L])))
    axis(side,at=10^seq(ax[1L],ax[2L]),label=FALSE,...)
    if (prug) rug(10^lat2,ticksize=1,side=side,col="gray")
    if (ax29) {
        udr <- seq(floor(usr[1L]),ceiling(usr[2L]))
        udr29 <- sort(as.vector(outer(10^udr,1L:9L)))
        udr29 <- udr29[10^usr[1L]<udr29&udr29<10^usr[2L]]
        axis(side,at=udr29,label=FALSE,tcl=par("tcl")*0.5,...)
        if (prug) rug(udr29,ticksize=1,side=side,col="gray")
    }
    if (prug) rect(10^par("usr")[1],10^par("usr")[3],10^par("usr")[2],10^par("usr")[4])
}

