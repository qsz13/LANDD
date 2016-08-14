rho=c(0, 0.3, 0.5, 0.8)
n.sample=c(100, 500)
z.percent= c(0.25,0.5,0.75,0.95)
k = c(1,2)
kernel.sd=c(1 , 1.5)
normalize.method =c("one", "squareM", "none")


rho=0.3
n.sample=100
z.percent=0.95
k=2
kernel.sd=1
normalize.method="one"
result = c()
repeat.times = 50
index = 0
while(index<repeat.times){
  index = index + 1
  print(index)
  auc = simulateLANDD(rho, n.sample, z.percent,k,kernel.sd,normalize.method)
  print(paste0("rho:",rho," n.sample:",n.sample, " z.percent:",z.percent, " kernel.sd:",kernel.sd, " normalize.method:", normalize.method))
  result = c(result, auc)
  
}
result1=result

rho=0.3
n.sample=500
z.percent=0.95
k=2
kernel.sd=1
normalize.method="one"
result = c()
repeat.times = 50
index = 0
while(index<repeat.times){
  index = index + 1
  print(index)
  auc = simulateLANDD(rho, n.sample, z.percent,k,kernel.sd,normalize.method)
  print(paste0("rho:",rho," n.sample:",n.sample, " z.percent:",z.percent, " kernel.sd:",kernel.sd, " normalize.method:", normalize.method))
  result = c(result, auc)
  
}
result5001=result

rho=0.3
n.sample=500
z.percent=0.75
k=2
kernel.sd=1
normalize.method="one"
result = c()
repeat.times = 50
index = 0
while(index<repeat.times){
  index = index + 1
  print(index)
  auc = simulateLANDD(rho, n.sample, z.percent,k,kernel.sd,normalize.method)
  print(paste0("rho:",rho," n.sample:",n.sample, " z.percent:",z.percent, " kernel.sd:",kernel.sd, " normalize.method:", normalize.method))
  result = c(result, auc)
  
}
result5002=result

rho=0.3
n.sample=500
z.percent=0.5
k=2
kernel.sd=1
normalize.method="one"
result = c()
repeat.times = 50
index = 0
while(index<repeat.times){
  index = index + 1
  print(index)
  auc = simulateLANDD(rho, n.sample, z.percent,k,kernel.sd,normalize.method)
  print(paste0("rho:",rho," n.sample:",n.sample, " z.percent:",z.percent, " kernel.sd:",kernel.sd, " normalize.method:", normalize.method))
  result = c(result, auc)
  
}
result5003=result

rho=0.3
n.sample=500
z.percent=0.25
k=2
kernel.sd=1
normalize.method="one"
result = c()
repeat.times = 50
index = 0
while(index<repeat.times){
  index = index + 1
  print(index)
  auc = simulateLANDD(rho, n.sample, z.percent,k,kernel.sd,normalize.method)
  print(paste0("rho:",rho," n.sample:",n.sample, " z.percent:",z.percent, " kernel.sd:",kernel.sd, " normalize.method:", normalize.method))
  result = c(result, auc)
  
}
result5004=result

rho=0.5
n.sample=500
z.percent=0.95
k=2
kernel.sd=1
normalize.method="one"
result = c()
repeat.times = 50
index = 0
while(index<repeat.times){
  index = index + 1
  print(index)
  auc = simulateLANDD(rho, n.sample, z.percent,k,kernel.sd,normalize.method)
  print(paste0("rho:",rho," n.sample:",n.sample, " z.percent:",z.percent, " kernel.sd:",kernel.sd, " normalize.method:", normalize.method))
  result = c(result, auc)
  
}
result5005=result

rho=0.5
n.sample=500
z.percent=0.75
k=2
kernel.sd=1
normalize.method="one"
result = c()
repeat.times = 50
index = 0
while(index<repeat.times){
  index = index + 1
  print(index)
  auc = simulateLANDD(rho, n.sample, z.percent,k,kernel.sd,normalize.method)
  print(paste0("rho:",rho," n.sample:",n.sample, " z.percent:",z.percent, " kernel.sd:",kernel.sd, " normalize.method:", normalize.method))
  result = c(result, auc)
  
}
result5006=result

rho=0.5
n.sample=500
z.percent=0.5
k=2
kernel.sd=1
normalize.method="one"
result = c()
repeat.times = 50
index = 0
while(index<repeat.times){
  index = index + 1
  print(index)
  auc = simulateLANDD(rho, n.sample, z.percent,k,kernel.sd,normalize.method)
  print(paste0("rho:",rho," n.sample:",n.sample, " z.percent:",z.percent, " kernel.sd:",kernel.sd, " normalize.method:", normalize.method))
  result = c(result, auc)
  
}
result5007=result

rho=0.5
n.sample=500
z.percent=0.25
k=2
kernel.sd=1
normalize.method="one"
result = c()
repeat.times = 50
index = 0
while(index<repeat.times){
  index = index + 1
  print(index)
  auc = simulateLANDD(rho, n.sample, z.percent,k,kernel.sd,normalize.method)
  print(paste0("rho:",rho," n.sample:",n.sample, " z.percent:",z.percent, " kernel.sd:",kernel.sd, " normalize.method:", normalize.method))
  result = c(result, auc)
  
}
result5008=result

rho=0.8
n.sample=500
z.percent=0.95
k=2
kernel.sd=1
normalize.method="one"
result = c()
repeat.times = 50
index = 0
while(index<repeat.times){
  index = index + 1
  print(index)
  auc = simulateLANDD(rho, n.sample, z.percent,k,kernel.sd,normalize.method)
  print(paste0("rho:",rho," n.sample:",n.sample, " z.percent:",z.percent, " kernel.sd:",kernel.sd, " normalize.method:", normalize.method))
  result = c(result, auc)
  
}
result5009=result

rho=0.8
n.sample=500
z.percent=0.75
k=2
kernel.sd=1
normalize.method="one"
result = c()
repeat.times = 50
index = 0
while(index<repeat.times){
  index = index + 1
  print(index)
  auc = simulateLANDD(rho, n.sample, z.percent,k,kernel.sd,normalize.method)
  print(paste0("rho:",rho," n.sample:",n.sample, " z.percent:",z.percent, " kernel.sd:",kernel.sd, " normalize.method:", normalize.method))
  result = c(result, auc)
  
}
result5010=result

rho=0.8
n.sample=500
z.percent=0.5
k=2
kernel.sd=1
normalize.method="one"
result = c()
repeat.times = 50
index = 0
while(index<repeat.times){
  index = index + 1
  print(index)
  auc = simulateLANDD(rho, n.sample, z.percent,k,kernel.sd,normalize.method)
  print(paste0("rho:",rho," n.sample:",n.sample, " z.percent:",z.percent, " kernel.sd:",kernel.sd, " normalize.method:", normalize.method))
  result = c(result, auc)
  
}
result5011=result

rho=0.8
n.sample=500
z.percent=0.25
k=2
kernel.sd=1
normalize.method="one"
result = c()
repeat.times = 50
index = 0
while(index<repeat.times){
  index = index + 1
  print(index)
  auc = simulateLANDD(rho, n.sample, z.percent,k,kernel.sd,normalize.method)
  print(paste0("rho:",rho," n.sample:",n.sample, " z.percent:",z.percent, " kernel.sd:",kernel.sd, " normalize.method:", normalize.method))
  result = c(result, auc)
  
}
result50012=result