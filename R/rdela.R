rdela <-
function ( data, N=floor((dim(data)[1]+dim(data)[2]+1)/2) )
{

if(is.matrix(data)==F)stop("at least two-dimensional data matrix required")
if(mode(data)!="numeric")stop("numeric data required")
if(dim(data)[1]<=dim(data)[2])stop("n > d required")

require(geometry)


outp<-list()
outp$data<-data


rdela_tri<-delaunayn(as.matrix(data),options="Fa Fn TO 'qhull_out.txt'")
outp$tri<-rdela_tri

if(any(apply(rdela_tri,1,function(x) det(cov(data[x,])))==0))stop("degenerate data set")

rdela_neigh<-as.list(readLines(paste(getwd(),"/qhull_out.txt",sep="")))[-c(1:(dim(rdela_tri)[1]+2))]
rdela_neigh<-sapply(rdela_neigh,function(x) strsplit(x," "))
rdela_neigh<-lapply(rdela_neigh,function(x) as.numeric(x[-1])+1)
rdela_neigh<-lapply(rdela_neigh,function(x) x[which(x>0)])

file.remove(paste(getwd(),"/qhull_out.txt",sep=""))
outp$neigh<-rdela_neigh

rdela_area<-apply(rdela_tri,1,function(x) sqrt(-1/sum(solve(as.matrix(dist(data[x,]))^2*(-.5)))))
outp$radii<-rdela_area
rdela_center<-apply(rdela_tri,1,function(x) (rowSums(solve(as.matrix(dist(data[x,]))^2*(-.5)))/sum(solve(as.matrix(dist(data[x,]))^2*(-.5))))%*%data[x,])
outp$center<-rdela_center


k<-0
T1<-order(rdela_area)
l<-0
LiB<-list()
LiN<-list()
GeB<-c()
test88<-c()

repeat{l<-l+1

T2<-sapply(LiN,function(x){any(x==T1[l])})

if(any(T2==TRUE)){

	if(sum(T2)>1){
	T4<-which(T2)
	LiB[[T4[1]]]<-c(unlist(LiB[T4]),T1[l])
	LiB[T4[-1]]<-0
	LiN[[T4[1]]]<-unique(c(unlist(LiN[T4]),rdela_neigh[[T1[l]]]))
	LiN[T4[-1]]<-0
	GeB[T4[1]]<-length(unique(as.vector(rdela_tri[LiB[[T4[1]]],])))
	GeB[T4[-1]]<-NA
	}
	else{
	T3<-which(T2)
	LiB[[T3]]<-c(LiB[[T3]],T1[l])
	LiN[[T3]]<-unique(c(LiN[[T3]],rdela_neigh[[T1[l]]]))
	GeB[T3]<-length(unique(as.vector(rdela_tri[LiB[[T3]],])))
	}

}
else{
LiB[[l]]<-c(T1[l])
LiN[[l]]<-rdela_neigh[[T1[l]]]
GeB[l]<-length(unique(as.vector(rdela_tri[LiB[[l]],])))
}

test88[l]<-max(na.omit(GeB))

if(any(na.omit(GeB)>=N)==TRUE) break
}

outp$LiB<-LiB
outp$LiN<-LiN
outp$GeB<-GeB

outp$drin<-unique(as.vector(rdela_tri[LiB[[which.max(GeB)]],]))

class(outp)<-"rdela"
return(outp)

}

