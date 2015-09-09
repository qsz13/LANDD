
load("/home/daniel/LAS/GSE10255_entrez.bin")
g = graph.data.frame(as.matrix(read.table("/home/daniel/LAS/HumanBinaryHQ_HINT.txt")), directed=FALSE)
laresult10255 = lascouting(g, new.array,n.cores=8)
print("laresult10255 done")
zmatrix10255 = graph.kd(laresult10255,g)
print("kd done")
for(cut in seq(from=1, to=0.5, by=-0.25))
{
  result = getgobp(g, zmatrix10255,cutoff=cut,n.cores=8)
  write.csv(result,file=paste("cutoff",as.character(cut),"-10255.csv", sep=""))
  result = NULL
}


