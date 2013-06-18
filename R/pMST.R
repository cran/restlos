pMST<-function (data, N = floor((dim(data)[1] + dim(data)[2] + 1)/2), 
    lmax = dim(data)[1] * 100) 
{

	require(nnclust)
 	
	if(is.data.frame(data))
		data=as.matrix(data)
		
    if (!is.matrix(data)) 
        stop("at least two-dimensional data matrix required")

    if (mode(data) != "numeric") 
        stop("numeric data required")

    if (dim(data)[1] <= dim(data)[2]) 
        stop("n > d required")
	
	if (dim(data)[1] <= N) 
        stop("Trying to find more than all observations")
		
		
	x2 <- mst(data)
	U1 <- x2$dist
	x2 <- cbind(x2$from,x2$to)
	T1<-order(U1)
	l<-0
	LiB<-list(c())
	GeB<-c()
	LeB<-c()
	x6<-matrix(0,ncol=3,nrow=1)

    repeat {
        l <- l + 1
        T2 <- sapply(LiB, function(x) {
            any(x == x2[T1[l], 1] | x == x2[T1[l], 2])
        })
        x70 <- 0
        if (any(T2 == TRUE)) {
            if (sum(T2 == TRUE) > 1) {
                T4 <- which(T2 == TRUE)
                maxi <- which.max(sapply(LiB[T4], length))
                x3 <- colMeans(data[unlist(LiB[T4[maxi]]), ])
                LiB[[T4[1]]] <- unique(c(unlist(LiB[T4]), x2[T1[l], 
                  1], x2[T1[l], 2]))
                x4 <- mean(sapply(LiB[T4[-maxi]], function(x) {
                  sqrt(sum((x3 - colMeans(data[x, ]))^2))
                }))
                LiB[T4[-1]] <- 0
                GeB[T4[1]] <- length(LiB[[T4[1]]])
                GeB[T4[-1]] <- NA
                LeB[T4[1]] <- sum(LeB[T4]) + U1[T1[l]]
                LeB[T4[-1]] <- 0
                x5 <- x4
                x7 <- LeB[which.max(sapply(LiB, length))]
            }
            else {
                T3 <- which(T2 == TRUE)
                x3 <- colMeans(data[LiB[[T3]], ])
                LiB[[T3]] <- unique(c(LiB[[T3]], x2[T1[l], 1], 
                  x2[T1[l], 2]))
                x4 <- colMeans(data[x2[T1[l], ], ])
                GeB[T3] <- length(LiB[[T3]])
                x5 <- sqrt(sum((x3 - x4)^2))
                LeB[T3] <- sum(LeB[T3]) + U1[T1[l]]
                x7 <- LeB[which.max(sapply(LiB, length))]
            }
            x6 <- rbind(x6, c(x7, U1[T1[l]], max(sapply(LiB, 
                length))))
        }
        else {
            LiB[[l]] <- c(x2[T1[l], 1], x2[T1[l], 2])
            GeB[l] <- length(LiB[[l]])
            LeB[l] <- U1[T1[l]]
        }
        if (any(na.omit(GeB) >= N) == TRUE || l == lmax) 
            break
    }
    drin <- LiB[[which.max(sapply(LiB, length))]]
    pMST <- list(loc = colMeans(data[drin, ]), cov = cov(data[drin, 
        ]), sample = sort(drin), data = data, x6 = x6)
    class(pMST) <- "pMST"
    return(pMST)
}
