#######generate graph and matrix######
#' @importFrom mvtnorm rmvnorm
#' @export
simulateLANDD<-function(rho, n.sample, z.percent,k,kernel.sd,normalize.method ) {
  n.gene = 5000
  net.density = 1
  neighbor.correlation = 0.4
  
  #generate graph
  simu.g = LANDD:::createnet(n.gene, net.density)
  
  #generate expression matrix
  s<-shortest.paths(simu.g)
  s<-neighbor.correlation^(s)  # this is to serve as covariance matrix
  simu.matrix<-t(mvtnorm::rmvnorm(n=n.sample, mean=rep(0,n.gene), sigma=s, method="chol")) 
  simu.g.step=simu.g
  if(k!=1){
    simu.g.step = connect.neighborhood(simu.g, 2) #connect 2 step neighbours
    
    #degree is the number of 2 step neighbours
    #
  }
  degree.step = igraph::degree(simu.g.step)
  
  
  egonodes = V(simu.g)[which(degree.step<=40 & degree.step>=20)] #filter out nodes with 20<= neighbour<=40
  path.matrix = shortest.paths(simu.g, egonodes,egonodes)
  pairs = which(path.matrix>=5, arr.ind = TRUE) #filter out X Z piars with distance >=5
  pair = pairs[sample(nrow(pairs), 1), ] #randomly select one pair
  if(degree.step[egonodes[pair[1]]]<degree.step[egonodes[pair[2]]]) { #find out which egonet is larger
    pair = c(pair[2],pair[1])
  }
  
  XY.ego.node = egonodes[pair[1]] #node with larger egonet as center of XY
  Z.ego.node = egonodes[pair[2]] #node with smaller wgonet as center of Z
  #
  #print(XY.ego.node)
  
  #print(V(simu.g)[XY.ego.node])
  #print(typeof(V(simu.g)[XY.ego.node]))
  
  XY.nodes =  unlist(igraph::neighborhood(simu.g, k, nodes = XY.ego.node)) #XY egonet nodes including ego x
  Z.nodes =  unlist(igraph::neighborhood(simu.g, k, nodes = Z.ego.node)) #Z egonet nodes including ego w
  Y.nodes = XY.nodes[which(XY.nodes!=XY.ego.node)]
  
  # XY.ego.node = as.character(XY.ego.node)
  # XY.nodes = as.character(XY.nodes)
  # Z.nodes = as.character(Z.nodes)
  # Y.nodes = as.character(Y.nodes)
  
  
  replace.times = floor((length(Z.nodes)*z.percent))# replace z
  
  
  Y.sample = sample(Y.nodes,replace.times) #the Y nodes to be re used to generate z
  Z.sample = sample(Z.nodes,replace.times) # the Z nodes to be replaced
  if(rho!=0){
    index = 0
    while(index < replace.times) {
      index = index + 1
      la = la.simu.given.xy(simu.matrix[XY.ego.node,],simu.matrix[Y.sample[index],], rho)
      simu.matrix[Z.sample[index],] = la#replace z nodes
    }
  }
  V(simu.g)$name = 1:n.gene
  rownames(simu.matrix) = 1:n.gene
  
  g = simu.g
  m = simu.matrix
  
  #LANDD
  lamatrix = lascouting(g,m,k=k,n.cores=4)
  kd = graph.kd(lamatrix,g, kernel.sd=kernel.sd,smoothing.normalize=normalize.method)
  
  la.x = kd[as.character(XY.ego.node),]#get the kernel result of X
  la.x = la.x[! names(la.x) %in% XY.nodes]
  # w = names(tail(sort(kd[as.character(XY.ego.node),]),replace.times*2))
  w = names(tail(sort(la.x),replace.times*2))
  common = intersect(Z.sample, w)
  
  # subg <- induced.subgraph(simu.g, unique(c(XY.nodes, w,Z.sample)))
  # type <- rep("not found", length(V(subg)))
  # type <- setNames(type, V(subg)$name)
  # type[as.character(XY.nodes)] = "XY"
  # type[as.character( w)] = "w"
  # type[as.character(common )] = "found"
  # type[as.character(Z.ego.node)] = "Z"
  # type[as.character(XY.ego.node)] = "X"
  # ggnet2(subg, color=type,palette = "Set1")
  
  #roc
  g.name = V(simu.g)$name 
  g.sample.name = setdiff(g.name,XY.nodes)
  response <- rep(0, length(g.sample.name))
  response <- setNames(response, g.sample.name)
  response[as.character(Z.sample)] = 1
  predictor <- rep(0, length(g.sample.name))
  predictor <- setNames(predictor, g.sample.name)
  predictor[as.character(w)] = 1
  
  
  
  modelroc<-roc(response,predictor)
  modelroc$auc
  plot(modelroc, smooth = TRUE, print.auc=TRUE, auc.polygon=TRUE, grid=c(0.1, 0.2),
       grid.col=c("green", "red"), max.auc.polygon=TRUE,
       auc.polygon.col="skyblue", print.thres=TRUE)
  return(modelroc$auc)
  #result = get.W(g,lamatrix,kd,cutoff=0.4)
  
  
  
  
}

simulateLANDD3<-function(n.gene,net.density,rho,neighbor.correlation,n.sample,z.percent) {
  #generate graph
  simu.g = LANDD:::createnet(n.gene, net.density)
  
  #generate expression matrix
  s<-shortest.paths(simu.g)
  s<-neighbor.correlation^(s)  # this is to serve as covariance matrix
  simu.matrix<-t(mvtnorm::rmvnorm(n=n.sample, mean=rep(0,n.gene), sigma=s, method="chol")) 
  
  
  simu.g.2step = connect.neighborhood(simu.g, 2) #connect 2 step neighbours
  degree.2step = degree(simu.g.2step) #degree is the number of 2 step neighbours
  egonodes = V(simu.g)[which(degree.2step<=40 & degree.2step>=20)] #filter out nodes with 20<= neighbour<=40
  path.matrix = shortest.paths(simu.g, egonodes,egonodes)
  pairs = which(path.matrix>=5, arr.ind = TRUE) #filter out X Z piars with distance >=5
  pair = pairs[sample(nrow(pairs), 1), ] #randomly select one pair
  if(degree.2step[egonodes[pair[1]]]<degree.2step[egonodes[pair[2]]]) { #find out which egonet is larger
    pair = c(pair[2],pair[1])
  }
  
  XY.ego.node = egonodes[pair[1]] #node with larger egonet as center of XY
  Z.ego.node = egonodes[pair[2]] #node with smaller wgonet as center of Z
  
  XY.nodes =  unlist(igraph::neighborhood(simu.g, 2, nodes = XY.ego.node)) #XY egonet nodes
  Z.nodes =  unlist(igraph::neighborhood(simu.g, 2, nodes = Z.ego.node)) #Z egonet nodes
  
  #origin replace method 
  replace.times = floor(length(Z.nodes)/2) #replace half of Z egonet
  
  Z.sample = sample(Z.nodes, replace.times) #randomly select z nodes to replace
  XY.sample = sample(XY.nodes, replace.times*2) #randomly select x and y nodes to replace
  
  index = 0
  while(index < replace.times) {
    index = index + 1
    la = la.simu(n.sample, rho)
    simu.matrix[XY.sample[index*2],] = la[,"x"]
    simu.matrix[XY.sample[index*2-1],] = la[,"y"]
    simu.matrix[Z.sample[index],] = la[,"z"]
    
  }
  V(simu.g)$name = 1:n.gene
  rownames(simu.matrix) = 1:n.gene
  return(list(g = simu.g, m = simu.matrix,XY.sample=XY.sample, Z.sample=Z.sample,Z.nodes = Z.nodes, XY.nodes = XY.nodes,XY.ego.node=XY.ego.node,Z.ego.node=Z.ego.node))
  #end
  
}



#### simulate a gene triplet with LA relations
### the three columns in the output are X, Y and Z genes
la.simu<-function(n, rho)
{
  library(mvtnorm)
  z<-rnorm(n)
  z<-sort(z)
  
  x<-rnorm(n)
  y<-rnorm(n)
  
  sigma.z.low<-matrix(c(1,-rho,-rho,1),ncol=2)
  sigma.z.high<-matrix(c(1,rho, rho,1),ncol=2)
  
  xy.z.low<-rmvnorm(round(n/3), mean=c(0,0), sigma=sigma.z.low)
  xy.z.high<-rmvnorm(round(n/3), mean=c(0,0), sigma=sigma.z.high)
  
  x[1:round(n/3)]<-xy.z.low[,1]
  y[1:round(n/3)]<-xy.z.low[,2]
  
  x[(n-round(n/3)+1):n]<-xy.z.high[,1]
  y[(n-round(n/3)+1):n]<-xy.z.high[,2]
  
  return(cbind(x,y,z))
}

###
#######generate graph and matrix######
#' @importFrom mvtnorm rmvnorm
#' @export
la.simu.given.xy<-function(x, y, rho) # here rho is the correlation strength between x*y and z
{
  library(mvtnorm)
  xy<-x*y
  r<-rank(xy)
  
  sigma.z.w<-matrix(c(1,rho, rho,1),ncol=2)
  zw<-rmvnorm(length(x), mean=c(0,0), sigma=sigma.z.w)
  #w is the second column. It is an auxilary variable, whose order will help establish correlation between xy and z
  
  zw<-zw[order(zw[,2]),]
  zw<-zw[r,]
  z<-zw[,1]
  
  return(z)
}
####simulate a network######
createnet<-function(num.gene,netdensity){
  library(igraph)
  g<-barabasi.game(num.gene,m=netdensity)
  return(g)
}
