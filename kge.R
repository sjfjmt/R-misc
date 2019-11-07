kge <- function(ki=100,ns=12,d=5,p=0.5) {
    kdata <- ki
    kdlst <- ki
    dev.new()
    dev1 <- dev.cur()
    plot(c(0,ns),c(ki-ns*d,ki+ns*d),type="n",xlab="ステップ数",ylab="株価")
    points(0,ki,pch=16)
    text(0,ki,paste(ki),pos=3)
    for (ii in seq_len(ns)) {
        sl <- ""
        sl <- select.list(c("次のステップ"))
        if (sl=="") break
        pm <- sample(x=c(-1,1),size=1,prob=c(1-p,p))
        kdata <- kdata + d*pm
        kdlst <- c(kdlst,kdata)
        plot(c(0,seq_len(ii)),kdlst,xlim=c(0,ns),ylim=c(ki-ns*d,ki+ns*d),type="b",pch=16,lwd=1,xlab="ステップ数",ylab="株価")
        if (pm==1) {
            mtext("UP",side=3,cex=2)
        }
        else {
            mtext("DOWN",side=3,cex=2)
        }
        text(ii,kdata,paste(kdata),pos=3)
    }
    sl <- NULL
    sl <- select.list(c("グラフ閉じて終了","グラフ閉じない"))
    if (sl=="グラフ閉じて終了" || is.null(sl)) {
        dev.off(dev1)
    }
}

kge2 <- function(ki=100,ns=12,d=5,p=0.5,nt=50,ncntn=TRUE,st=0.1) {
    dev.new()
    plot(c(0,ns),c(ki-ns*d,ki+ns*d),type="n",xlab="ステップ数",ylab="株価")
    text(0,ki,paste(ki),pos=3)
    locator(1)
    dev1 <- dev.cur()
    dev.new()
    dev2 <- dev.cur()
    kfinL <- NULL
    for (jj in seq_len(nt)) {
        kdata <- ki
        kdlst <- ki
        dev.set(dev1)
        for (ii in seq_len(ns)) {
            kdata <- kdata + d*sample(x=c(-1,1),size=1,prob=c(1-p,p))
            kdlst <- c(kdlst,kdata)
            points(c(0,seq_len(ii)),kdlst,pch=16)
            lines(c(0,seq_len(ii)),kdlst)
        }
        text(ii,kdata,paste(kdata),pos=3)
        mtext(paste("残り試行回数:",nt-jj),side=3,adj=1)
        kfinL <- c(kfinL,kdata)
        dev.set(dev2)
        tblkfin <- table(factor(kfinL,levels=seq(ki-ns*d,ki+ns*d,2*d)))
        par(lend=1)
        plot(as.numeric(names(tblkfin)),tblkfin/jj,bty="n",type="h",lwd=20,yaxs="i",xlab="",ylab="確率",col="gray")
        text(as.numeric(names(tblkfin)),tblkfin/jj,labels=round(tblkfin/jj,3),pos=1,cex=0.5,offset=0.1)
        title(main=paste(ns,"ステップ後の株価の確率分布",sep=""))
        axis(2)
        par(lend=0)
        usr <- par("usr")
        usrH <- usr[4]-usr[3]
        par(xpd=TRUE)
        qt <- quantile(kfinL)
        yat <- 0.12
        ydf <- 0.02
        arrows(x0=qt[1],y0=usr[3]-yat*usrH,x1=qt[5],length=0,lwd=3)
        rect(xleft=qt[2],ybottom=usr[3]-(yat+ydf)*usrH,xright=qt[4],ytop=usr[3]-(yat-ydf)*usrH,lwd=3,col="white")
        arrows(x0=qt[3],y0=usr[3]-(yat+ydf)*usrH,y1=usr[3]-(yat-ydf)*usrH,length=0,lwd=3)
        par(xpd=FALSE)
        dev.set(dev1)
        if (ncntn) {
            sl <- ""
            sl <- select.list(c("次の試行","連続全試行"))
            if (sl=="連続全試行") {
                ncntn=FALSE
            }
            else if (sl=="") {
                break
            }
        }
        else {
            Sys.sleep(st)
        }
        points(c(0,seq_len(ii)),kdlst,pch=16,col="gray")
        lines(c(0,seq_len(ii)),kdlst,col="gray")
        text(ii,kdata,paste(kdata),pos=3,col="gray")
        mtext(paste("残り試行回数:",nt-jj),side=3,adj=1,col="white")
        mtext(paste("残り試行回数:",nt-jj),side=3,adj=1,col="white",font=2)
    }
    mtext(paste("全試行回数:",jj),side=3,adj=1)
    sl <- select.list(c("グラフ閉じて終了","グラフ閉じない"))
    if (sl=="グラフ閉じて終了") {
        dev.off(dev1)
        dev.off(dev2)
    }
}

