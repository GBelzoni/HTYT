library('ggplot2')

# The palette with grey:
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
# To use for fills, add
scale_fill_manual(values=cbPalette)

#Generate binomial distribution

#distribution params
N=100
prob = 0.5

#Generating data
heads = 0:N
probs = dbinom(heads,size=N,prob=prob)
data = data.frame(heads)
data['prob']=probs

#Plot binomial distribution
p = ggplot(data=data, aes(x=heads,y=prob)) 
p = p + geom_bar(stat='identity',fill="#FF9999")
title = paste("Prob Density Funtion (PDF) for Binomial Distribution with N = ",N," and prob = ",prob,sep="")
p = p + ggtitle(title)
p

#Get percentile
alpha = 0.05
oneside_alpha = alpha/2
pctl = qbinom(p=oneside_alpha,size=N,prob=prob)

#Shade 'Unlikely' area
unlikely_border = pctl-1
unlikelyNs = c(0:(unlikely_border), (N-unlikely_border):(N) )
unlikelyNsIndex = unlikelyNs + 1
unlikely_fill = rep('likely',N+1)
unlikely_fill[unlikelyNsIndex]='unlikely'

#Probability of unlikely area
sum(dbinom(unlikelyNs,size=N,prob=0.5))

#plot unlikely area
data['unlikely_region'] = unlikely_fill
title = paste("PDF for Binomial Distribution with N = ",N," and prob = ",prob," n = ",unlikely_border,sep="")
p = ggplot(data=data, aes(x=heads,y=prob,fill=unlikely_region)) + geom_bar(stat='identity') + scale_color_brewer()
p = p + ggtitle(title)
p
