plot.rdela <-
function (x,...) 
{

require(rgl)


if(dim(x$data)[2]==2){
	x11()
	plot(x$data,asp=1,xlab=expression(data[x]),ylab=expression(data[y]))
	symbols(x=x$center[1,],y=x$center[2,],circles=x$radii,fg="grey",inches=F,add=T)
	apply(x$tri,1,function(y) lines(x$data[c(y,y[1]),]))
	symbols(x=x$center[1,x$LiB[[which.max(x$GeB)]]],y=x$center[2,x$LiB[[which.max(x$GeB)]]],circles=x$radii[x$LiB[[which.max(x$GeB)]]],fg="steelblue2",inches=F,add=T)
	apply(x$tri[x$LiB[[which.max(x$GeB)]],],1,function(y) lines(x$data[c(y,y[1]),],col="darkorange"))
	points(x$data[unique(as.vector(x$tri[x$LiB[[which.max(x$GeB)]],])),],col="red",pch=19)
	}

if(dim(x$data)[2]==3){
	open3d(mouseMode="trackball")
	plot3d(x$data,xlab="data_x",ylab="data_y",zlab="data_z")
	points3d(x$data[unique(as.vector(x$tri[x$LiB[[which.max(x$GeB)]],])),],col="red",size=5)
	}

x11()
plot(mahalanobis(x$data,center=colMeans(x$data[x$drin,]),cov=cov(x$data[x$drin,])),ylab="Mahalanobis distance",xlab="Index")
points(x$drin,mahalanobis(x$data,center=colMeans(x$data[x$drin,]),cov=cov(x$data[x$drin,]))[x$drin],col="red")

}

