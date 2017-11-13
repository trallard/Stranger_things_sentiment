###############################################################################
####### Pull script data (would need to be edited based on data source) #######
###############################################################################

library(rvest)
library(tibble)

#Set up tibble for script storage
stranger=tibble(episode=1:17,script=rep(" ",17))

#Pull table of contents for scripts
url="https://www.springfieldspringfield.co.uk/episode_scripts.php?tv-show=stranger-things-2016"
download.file(url,destfile="scrapedpage.html",quiet=TRUE)
sthtml=read_html("scrapedpage.html")

#Find links to episode scripts from table of contents (source-specific code)
hrefs=sthtml %>% 
  html_nodes(".season-episode-title") %>% 
  html_attr("href")
hrefs=paste0("https://www.springfieldspringfield.co.uk/",hrefs)

#Loop through episode links, pull and clean scripts, and store data in tibble
trim <- function (x) gsub("^\\s+|\\s+$", "", gsub("\\s+", " ", x))
for(i in 1:17){
  download.file(hrefs[i],destfile="scrapedpage.html",quiet=TRUE)
  thisep=read_html("scrapedpage.html")
  
  script=thisep %>%
    html_nodes(".scrolling-script-container") %>%
    html_text() %>%
    gsub("[\r?\n|\n|\"|,|.|?|!|-]"," ",.) %>%
    tolower(.) %>% trim(.) %>% strsplit(.," ")
  
  stranger$script[i]=script
  cat("Pulled episode",i,"\n")
}


##########################################################################
####### Get overall episode means, and create episode trajectories #######
##########################################################################

library(tidytext)
library(dplyr)

means=NULL
epvecs=matrix(nrow=17,ncol=500)
for(i in 1:17){
  #Pull out words from episode i
  thisep=tibble(wordnum=1:length(stranger$script[i][[1]]),word=stranger$script[i][[1]])
  
  #Assign words a sentiment value based on the afinn
  epsent = thisep %>% inner_join(get_sentiments("afinn"))
  vec=NULL
  for(j in 21:(nrow(epsent)-20)){
    vec=c(vec,mean(epsent$score[(j-20):(j+20)]))
  }
  means=c(means,mean(epsent$score))
  vec=approx(vec,n=500)$y
  epvecs[i,]=vec
}


################################################
####### Create plot of average sentiment #######
################################################

library(ggplot2)

df=data.frame(Episode=as.factor(c(1:17)),Sentiment=means)
p=ggplot(df,aes(x=Episode,y=Sentiment,fill=Episode))+geom_bar(stat="identity")
p=p+theme_bw()+ylim(-.6,.6)+theme(legend.position="n")
p=p+ylab("Average Sentiment")+theme(axis.text=element_text(size=8))
p


############################################################################
####### Create ggplot-compatible data frame for trajectory animation #######
############################################################################

epdf=data.frame(Episode=as.factor(rep(1:17,each=500)),
                Season=as.factor(c(rep(1,8*500),rep(2,9*500))),
                Time=rep(1:500,17)/500,Sentiment=as.vector(t(epvecs)),
                a1=rep(1,500*17),a2=rep(1,500*17),a3=rep(1,500*17),
                a4=rep(1,500*17),a5=rep(1,500*17),a6=rep(1,500*17),
                a7=rep(1,500*17),a8=rep(1,500*17),a9=rep(1,500*17),
                a10=rep(1,500*17),a11=rep(1,500*17),a12=rep(1,500*17),
                a13=rep(1,500*17),a14=rep(1,500*17),a15=rep(1,500*17),
                a16=rep(1,500*17),a17=rep(1,500*17))

#Make alpha variables to be later used for hiding background episodes
for(i in 1:17){
  epdf[,i+4][epdf$Episode!=i]<-.3
}


###############################################################
####### Create ggplot animation of episode trajectories #######
###############################################################

library(animation)

saveGIF({
  for(i in 1:17){
    data=epdf
    alphas=rep(.2,17)
    alphas[i]=1
    p=ggplot(data)
    p=p+geom_line(aes(x=Time,y=Sentiment,group=Episode,color=Episode,alpha=data[,i+4]))
    p=p+ylim(-2.1,2.1)+scale_alpha(guide = 'none')
    p=p+guides(color=guide_legend(override.aes=list(alpha=alphas)))
    p=p+theme_bw()
    print(p)
  }
},interval=1.25,ani.width=700,ani.height=400)


##################################################
####### Create plot of season trajectories #######
##################################################

p=ggplot(epdf)+geom_line(aes(x=Time,y=Sentiment,group=Episode,color=Season),alpha=.3)
p=p+theme_bw()+geom_smooth(aes(x=Time,y=Sentiment,group=Season,color=Season),
                           se=TRUE,size=1.5,show.legend=FALSE)
p=p+geom_smooth(aes(x=Time,y=Sentiment),fill=NA,size=0)+ylim(-2.1,2.1)
p=p+theme(legend.key = element_rect(fill = "white"))
p=p+guides(color=guide_legend(override.aes=list(size=1.5,alpha=1)))
p


##############################################################
####### Create episode network for community detection #######
##############################################################

library(igraph)

adj.mat=cor(t(epvecs))-diag(1,17)

#Community detection was run in MATLAB using community_louvain from the brain connectivity toolbox
#BCT found here: https://sites.google.com/site/bctnet/Home/help
groups=c(2,1,3,1,1,3,2,2,2,3,3,1,3,1,2,1,2)


#################################################
####### Create plot of group trajectories #######
#################################################

epdf$Group=as.factor(rep(groups,each=500))
p=ggplot(epdf)+geom_line(aes(x=Time,y=Sentiment,group=Episode,color=Group),alpha=.3)
p=p+theme_bw()+geom_smooth(aes(x=Time,y=Sentiment,group=Group,color=Group),
                           se=TRUE,size=1.5,show.legend=FALSE)
p=p+geom_smooth(aes(x=Time,y=Sentiment),fill=NA,size=0)+ylim(-2.1,2.1)
p=p+theme(legend.key = element_rect(fill = "white"))
p=p+guides(color=guide_legend(override.aes=list(size=1.5,alpha=1)))
p


###################################################
####### Create animation of episode network #######
###################################################

library(scales)

#Create graph without negative associations for better visualization
adj.mat.no.neg=adj.mat
adj.mat.no.neg[adj.mat<0]<-0
graph=graph_from_adjacency_matrix(adj.mat.no.neg,weighted=TRUE,mode="undirected")

#Recreate colors from previous plot
red=col2rgb(hue_pal()(3)[groups])[1,]
green=col2rgb(hue_pal()(3)[groups])[2,]
blue=col2rgb(hue_pal()(3)[groups])[3,]

saveGIF({
  for(i in 1:17){
    alphas=rep(.4*255,17)
    alphas[i]<-1*255
    set.seed(2425)
    plot.igraph(graph,vertex.label.cex=1, 
                vertex.label.family="Helvetica",
                vertex.shape="circle",
                vertex.color=rgb(red,green,blue,alphas,maxColorValue=255),
                vertex.size=10,vertex.label.color=rgb(0,0,0,alphas,maxColorValue=255),
                edge.width=E(graph)$weight*5,
                xlim=c(-.95,.95),ylim=c(-.95,.95))
  }
},interval=1.25,ani.width=700,ani.height=500)


