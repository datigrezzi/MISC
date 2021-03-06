# function to plot error bars in barplots
# from monkeysuncle.stanford.edu/?p=485

colorvec<-c('antiquewhite','cornflowerblue','darkolivegreen2','brown1')

error.bar <- function(x, y, upper, lower = upper, length = 0.1, ...) {
	if (length(x) != length(y) | length(y) != length(lower) | length(lower) != length(upper)){
		stop("Vectors must be of the same length")
		} else {
			arrows(x, y + upper, x, y - lower, angle = 90, code = 3, length = length, ...)
			}
}

# function to calculate the standard error of the mean
sem <- function(x, na.rm = TRUE) {
	if (na.rm) {stdError <- sd(x, na.rm = T)/sqrt(length(x))} else {stdError <- sd(x)/sqrt(length(x))}
	return(stdError)
}

# function to plot line and shaded error area around the line
error.line<-function(x,y,error,type='l',lineCol='black',ylim=c(min(y),max(y)),ylab='',xlab='',errorCol=lineCol,errorAlpha=40,predict=T,add=F,...){
	if(length(error)==1){
		warning('Length of error was expanded to match length of x')
		error<-rep(error,length(x))
	}else if(length(error)>1&length(error)!=length(x)){
		stop('Error and x lengths differ!')
	}
	if(predict==T){
		lineData<-predict(smooth.spline(x,y))
		errorData<-predict(smooth.spline(x,error))
	} else {
		lineData<-as.data.frame(cbind(x,y))
		errorData<-as.data.frame(cbind(x,error))
		names(errorData)<-c('x','y')
	}
	
	if(add==F){plot(x,y,type = 'n',ylim = ylim,ylab = ylab, xlab = xlab)}
	lines(lineData, col = lineCol,type=type,...)
	colVal<-paste(rgb(t(col2rgb(errorCol)),maxColorValue=255),as.character(errorAlpha),sep='')
	polygon(c(lineData$x,rev(lineData$x)), c(lineData$y+errorData$y, rev(lineData$y-errorData$y)),col = colVal, border = NA)
}

# function to calculate cohen's d effect size for t.tests
cohen.d<-function(M1,SD1,M2,SD2){
	sPooled=sqrt(((SD1^2)+(SD2^2))/2)
	es<-M1-M2/sPooled
	return(es)
}

# interpolate ColNAs
col.na.approx<-function(data,na.rm=F){
	for(colNum in 1:ncol(data)){
		data[,colNum]<-na.approx(data[,colNum],...)
		# print(colNum)
	}
}