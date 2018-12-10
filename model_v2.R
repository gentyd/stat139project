# datapath = file.choose(new = FALSE)
# data = read.csv(datapath)

data$Interior_Wall = NULL
data$Exterior_Wall = NULL
data$House.. = NULL
data$Sale_Date = NULL

new_model = lm(Price~.,data=data)
# 
# set.seed(12)
# nsims = 2
# n=2000
# trainsize = 1500
# mse=rep(NA,nsims)
# 
# for(i in 1:nsims){
# reorder=sample(n)
#    train=data[reorder[1:trainsize],]
# 
#    test=data[reorder[(trainsize+1):n],]
# 
#    fit4=lm(formula(new_model),data=train)
#    mse[i]=mean(abs(test$Price-predict(fit4,new=test)))
# }
# 
# print(mse)

the_best_model = step(new_model)
# 
set.seed(12)
nsims = 2
n=2000
trainsize = 1500
mse=rep(NA,nsims)

for(i in 1:nsims){
  reorder=sample(n)
  train=data[reorder[1:trainsize],]
  
  test=data[reorder[(trainsize+1):n],]
  
  fit4=lm(formula(the_best_model),data=train)
  mse[i]=mean(abs(test$Price-predict(fit4,new=test)))
}

print(mse)
