

#Average shortest path (SP) length computation in a network and comparing SPs with n random networks

#1. Load the necessary libraries and read data files
library(igraph)
gDC <- graph.data.frame(DC,directed=F)  #read a data frame and transform into network
get.shortest.paths(gDC, 1)->g_sp # 1 is (from to) for first vertex to all other nodes (shortest paths)

#2. Get best shortest pathlength of each node in native network
pathlength = list()
for (i in 1:length(g_sp$vpath))
{
z<-length(g_sp$vpath[[i]])
pathlength[[i]]<-z
}
as.numeric(unlist(pathlength))->pathlength2
pathlength2->cs
network_a <- hist(cs, -1:max(cs), plot = FALSE)$density



#3. Perform randomization (n=100) of native network: Rewires the endpoints of the edges of a graph to a random vertex and compute probability of Shortest paths
A = list() #store probability of SP
B= list() #store SP length
pathlengthR = list()
shapiroP_value= list() #store results of sharpio test (p-value)
ASPLV= list() #store avg. shortest path length of random networks

for (j in 1:100)
{
set.seed(j)
g_random <- rewire(gDC, each_edge(prob=0.3, loops = FALSE, multiple = FALSE))
ASPLV[[j]]<-average.path.length(g_random)
get.shortest.paths(g_random, 5)->g_spR

for (i in 1:length(g_spR$vpath))
{
zR<-length(g_spR$vpath[[i]])
pathlengthR[[i]]<-zR
}

as.numeric(unlist(pathlengthR))->RPL2
print (RPL2)
B[[j]]<-RPL2
spl_prob<-hist(RPL2, -1:max(RPL2), plot = FALSE)$density
print(spl_prob)
A[[j]]<-spl_prob
shapiroP_value[[j]]<-shapiro.test(spl_prob)[[2]]
}

for (k in 1:100)
{
l<-c(A[[k]],0.000000000)
B[[k]]<-l
}
B->BC #rename variable

network_a
network_a[1]<-0
c(network_a, 0.00)->network_a #added last point intentially


#4. Plotting
png("SPL_distribution_random_network.png", width=7, height = 5, units = 'in', res = 300) 
plot(network_a, type='o', col="darkgreen",lwd=2, xlab = "Shortest Path", ylab = "Probability", xlim=c(1,8.5), ylim=c(0,1))
legend(7,1.0, c("Cold stress network"), col=c("darkgreen"), lty=c(1,1,1,1), lwd=2, cex=0.5)
for (i in 1:100)
{
lines(lowess(B[[i]],f=0.5), col=i, lwd=0.4, type="b") #p,l,o,b
}
lines(network_a, type='o', col="darkgreen",lwd=2)
dev.off()












