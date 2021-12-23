#########################################################
# Create tree classification set (training set), validation set (final hold-out test set)
##########################################################

#Installing packages if it is necessary
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(purrr)) install.packages("purrr", repos = "http://cran.us.r-project.org")
if(!require(scales)) install.packages("scales", repos = "http://cran.us.r-project.org")
if(!require(treemapify)) install.packages("treemapify", repos = "http://cran.us.r-project.org")
if(!require(gridExtra)) install.packages("gridExtra", repos = "http://cran.us.r-project.org")
if(!require(randomForest)) install.packages("randomForest", repos = "http://cran.us.r-project.org")

#loading libraries
library(tidyverse)# organization and visualization data
library(caret)# machine learning procedure Comtrol validation
library(purrr)# to extract data info
library(scales)# to add big mark point as thousand separator
library(treemapify) # to create treemaps
library(gridExtra) # to arrange plots in a grid
library(randomForest)#to run random forest


# Tree classification dataset:
# https://archive.ics.uci.edu/ml/machine-learning-databases/covtype/covtype.info
# https://archive.ics.uci.edu/ml/machine-learning-databases/covtype/covtype.data.gz

#Download datasets
df <- tempfile()#temporal datatable
download.file("https://archive.ics.uci.edu/ml/machine-learning-databases/covtype/covtype.data.gz", df)
infod <- tempfile()#temporal data info
download.file("https://archive.ics.uci.edu/ml/machine-learning-databases/covtype/covtype.info", infod)

trees_c <-str_split_fixed(readLines(gzfile(df)),",",55) #dataset
info<-readLines(infod)# data info to assign column names to trees_c dataset

#dimension raw data
dimfr<-data.frame(dim(trees_c)[1],dim(trees_c)[2])
colnames(dimfr)<-c("Rows", "Columns")

#extracting data info to assign column names
#there are 55 columns where columns 1 to 10 are unique, and column
#11 are 4 columns and column 12 are 40 columns.
#they are located in the data info data rows 174 to 186
di<-str_split(info," ")
dil<-unlist(map(di[c(174:183,186)],1))# 11columns, included cover type

WA<-sapply(1:4, function(x){#wilderness area, 4 columns
  paste0(map(di[184],1),"_",x)
})
ST<-sapply(1:40, function(x){#soil type, 40 columns
  paste0(map(di[185],1),"_",x)
})

#For description table
variab<-unlist(map(di[c(174:185)],1))# 12columns
dat_name<-str_squish(info[172])
dat_name<-unlist(strsplit(dat_name, " "))#column name properties
dat_name<-str_replace(dat_name,"Data","Data type") #replace data by data type
dat_name<-dat_name[c(1,2,4,5)]#final selection
#datatype
dtype<-sapply(info[174:185],function(x){
  str_trim(paste(unlist(str_split(x,""))[41:56], collapse = ''))
})
dtype<-unname(dtype)
#measurement
measu<-sapply(info[174:185],function(x){
  str_trim(paste(unlist(str_split(x,""))[57:85], collapse = ''))
})
measu<-unname(measu)
#description
descrp<-sapply(info[174:185],function(x){
  str_trim(paste(unlist(str_split(x,""))[86:130], collapse = ''))%>%
    str_replace_all(c("N"=" ","A"=" "))%>%
    str_trim()
})
descrp<-unname(descrp)

dtable<-data.frame(variab,dtype,measu,descrp)#description table
colnames(dtable)<-dat_name#column name description table
save(dtable,file="rda_data/dtable.rda")

#organize the col names according to the dataset and assign col names.
colnames(trees_c) <- inf_list<-c(dil[1:10],WA,ST,dil[11])

# transform values to integer, then save as dataframe
trees_c<-apply(trees_c, 2,as.numeric)%>%as.data.frame() %>%rowid_to_column("ID")

#create new 4 columns to identify the north, east, south and west correspondingly.
#they are dummy variables
trees_c<-trees_c%>%mutate(
              Aspect_1= (Aspect>=0 & Aspect<45 | Aspect>=315 & Aspect<=360)*1,#North
              Aspect_2= (Aspect>=45 & Aspect<135)*1,#East
              Aspect_3= (Aspect>=135 & Aspect<225)*1,#South
              Aspect_4= (Aspect>=225 & Aspect<315)*1)#West


trees_c$Aspect_label<-factor(ifelse(trees_c$Aspect >=0 & trees_c$Aspect<45 | trees_c$Aspect>=315 & trees_c$Aspect<=360, "North",
                             ifelse(trees_c$Aspect >=45 & trees_c$Aspect<135, "East",
                                    ifelse(trees_c$Aspect >=135 & trees_c$Aspect <225, "South",
                                           "West"))),levels = c("North","East","West","South"))



#extracting cover tree types names 
ctt_name<-info[254:260]%>%str_trim()%>%str_remove("Forest Cover Type Classes:\t")%>%
  str_remove(paste0(1:7," -- "))

#Adding new column with cover tree types
trees_c$name_ctt<- ifelse(trees_c$Cover_Type == 1, ctt_name[1],
                          ifelse(trees_c$Cover_Type == 2, ctt_name[2],
                                 ifelse(trees_c$Cover_Type == 3, ctt_name[3],
                                        ifelse(trees_c$Cover_Type == 4, ctt_name[4],
                                               ifelse(trees_c$Cover_Type == 5, ctt_name[5],
                                                      ifelse(trees_c$Cover_Type == 6, ctt_name[6],
                                        ctt_name[7]))))))


#Adding new columns from soil type decreasing from 40 types to 11 types.
#Climate and geological classification from USFS Ecological Landtype Units
#Aggregated based on climatic zone and geological zone.
#climatic zone
#it takes time
trees_c<- trees_c%>%mutate(S_cli_2= sapply(1:dim(trees_c)[1], function(t){
  ifelse(any(trees_c[t,16:21] ==1),1,0)
}),#lower montane dry
S_cli_3= sapply(1:dim(trees_c)[1], function(t){
  ifelse(any(trees_c[t,22:23] ==1),1,0)
}),#montane dry
S_cli_4= sapply(1:dim(trees_c)[1], function(t){
  ifelse(any(trees_c[t,24:28] ==1),1,0)
}),#montane
S_cli_5= sapply(1:dim(trees_c)[1], function(t){
  ifelse(any(trees_c[t,29:30] ==1),1,0)
}),#montane dry and montane
S_cli_6= sapply(1:dim(trees_c)[1], function(t){
  ifelse(any(trees_c[t,31:33] ==1),1,0)
}),#montane and subalpine
S_cli_7= sapply(1:dim(trees_c)[1], function(t){
  ifelse(any(trees_c[t,34:49] ==1),1,0)
}),#subalpine
S_cli_8= sapply(1:dim(trees_c)[1], function(t){
  ifelse(any(trees_c[t,50:55] ==1),1,0)
}))#alpine

#geological zone
#it takes time
trees_c<- trees_c%>%mutate(S_geo_1= sapply(1:dim(trees_c)[1], function(t){
  ifelse(any(trees_c[t,c(29:32,34:36)] ==1),1,0)
}),#alluvium
S_geo_2= sapply(1:dim(trees_c)[1], function(t){
  ifelse(any(trees_c[t,c(24,37:38)] ==1),1,0)
}),#glacial
S_geo_5= sapply(1:dim(trees_c)[1], function(t){
  ifelse(any(trees_c[t,22:23] ==1),1,0)
}),#mixed sedimentary
S_geo_7= sapply(1:dim(trees_c)[1], function(t){
  ifelse(any(trees_c[t,c(16:21,25:28,33,39:55)] ==1),1,0)
}))#igneous and metamorphic

trees_c<-trees_c%>%select(-(Soil_Type_1:Soil_Type_40)) #removing previous soil
#types columns

#Adding factor columns
##soil climatic type
#it takes time
s_cli_type<-data.frame(s_clim_type=sapply(1:dim(trees_c)[1], function(t){
  as.factor(names(trees_c[22+which(trees_c[t,c(23:29)] ==1)]))}))#22 is added to assign the corresponding column
trees_c<-cbind(trees_c,s_cli_type)
##soil geological type
s_geo_type<-data.frame(s_geo_type=sapply(1:dim(trees_c)[1], function(t){
  as.factor(names(trees_c[29+which(trees_c[t,c(30:33)] ==1)]))}))#29 is added to assign the corresponding column
trees_c<-cbind(trees_c,s_geo_type)
##Wilderness area
warea<-data.frame(Warea=sapply(1:dim(trees_c)[1], function(t){
  as.factor(which(trees_c[t,c(12:15)] ==1))}))
trees_c<-cbind(trees_c,warea)

#Saving raw_edited_document
save(trees_c,file="rda_data/trees_c.rda")

# Validation set will be 10% of tree classification dataset
set.seed(10, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(1)`
test_index <- createDataPartition(y = trees_c$Cover_Type, times = 1, p = 0.2, list = FALSE)
train_tc <- trees_c[-test_index,]#creation training tree cover
validation <- trees_c[test_index,]

#remove unnecessary data
rm(df,di,dil, ST, WA, info, infod, inf_list,ctt_name,
   warea,s_clim_type,s_geo_type,test_index, measu, variab,descrp,dat_name,dtype)

#save and load
save(train_tc,file="rda_data/train_tc.rda")
save(validation,file="rda_data/validation.rda")
load(file="rda_data/train_tc.rda")
load(file="rda_data/validation.rda")
load(file="rda_data/dtable.rda")
##############################################################################
##Data description

#count cover tree types and expressed as decreasing bar chart

cover_tree_count <- sapply(unique(train_tc$name_ctt), function(g) {
  sum((train_tc$name_ctt== g))})%>%
  data.frame(count=.) %>% 
  arrange(desc(.))
cover_tree_count<-cover_tree_count%>%mutate(label=row.names(.),
  prop=round((count/sum(cover_tree_count$count))*100,2))

cover_tree_count %>% ggplot(aes(x= reorder(label,-count), y = count, fill=row.names(.))) +
 geom_bar(stat='identity') + scale_fill_hue(c=60) + 
  theme(legend.position="none",axis.text.x=element_text(angle = 40, hjust = 1)) + 
  scale_y_continuous(labels = comma_format(big.mark = ",", decimal.mark = "."))+
labs(x='', y='cover tree types count')


#Proportions

ggplot(cover_tree_count, aes(area=count, fill=label,
                             subgroup=label)) + 
  ggtitle("Cover tree types total proportion:")+
  geom_treemap()+
  #main group bordering with grey
  geom_treemap_subgroup_border(colour="grey")+
  #all other group text in white
  geom_treemap_text(aes(label=paste0(label," \n", prop,"%")), color="white",
                    min.size = 1)+
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_fill_brewer(palette = "Set2")+
  theme(plot.title = element_text(hjust = 0.5, size = 16),
       legend.title = element_blank())

# Aspect
train_tc %>% 
  ggplot()+geom_bar(aes(x=name_ctt,fill=name_ctt)) +scale_y_continuous(trans='log10')+
  labs(title="Using geom_label")+
  facet_wrap(~Aspect_label,nrow = 2,ncol=2)+
  labs(x=NULL, y='Count in log10')+
  labs(title="Cover tree type distribution by orientation",fill="cover tree types")+
  scale_x_discrete(labels = NULL,breaks=NULL)

#Frequency table Aspect
faspc<-train_tc %>% count(name_ctt ,Aspect_label , sort = TRUE)
faspc<-faspc%>%spread(Aspect_label,n)#wrangling table
#chi-squared analysis
# ho the aspects values are independent; ha the aspects variables are correlated
chisq_test <- faspc %>%dplyr::select(-name_ctt) %>% chisq.test()
chisq_test$p.value #the values are correlated, not independent

#####not in report##########################################################
chias_matr <- apply(as.matrix.noquote(faspc),  # Using apply function
                    2,
                    as.numeric)
chias_matr  <-chias_matr[,2:5]
sqrt(chisq_test$statistic / sum(chias_matr)) #the V value is high, so the aspect is correlated. 
#it can be dismissed aspect as research variable.
######################################################################
rm(chias_matr,chisq_test)


# Wilderness area
train_tc %>% 
  ggplot()+geom_bar(aes(x=name_ctt,fill=name_ctt)) +scale_y_continuous(trans='log10')+
  labs(title="Using geom_label")+
  facet_wrap(~Warea,nrow = 2,ncol=2)+
  labs(x=NULL, y='Count in log10')+
  labs(title="Cover tree type distribution by Wilderness area",fill="cover tree types")+
  scale_x_discrete(labels = NULL,breaks=NULL)
# It is not necessary  chi-square analysis. Visually it is identified that
# the areas has a different species distribution.


# Soil type
#soil climatic zone
train_tc %>% 
  ggplot()+geom_bar(aes(x=name_ctt,fill=name_ctt)) +scale_y_continuous(trans='log10')+
  labs(title="Using geom_label")+
  facet_wrap(~s_clim_type,labeller = labeller(s_clim_type = 
                                                c("S_cli_7" = "Subalpine",
                                                  "S_cli_2" = "Lower Montane",
                                                  "S_cli_3" = "Montane dry",
                                                  "S_cli_4"= "Montane",
                                                  "S_cli_5"= "Montane dry and montane",
                                                  "S_cli_6"= "Montane and subalpine",
                                                  "S_cli_8"= "Alpine")),nrow = 3,ncol=3)+
  labs(x=NULL, y='Count in log10')+
  labs(title="Cover tree type distribution by soil climatic zone",fill="cover tree types")+
  scale_x_discrete(labels = NULL,breaks=NULL)+                                                                
  theme(strip.text.x = element_text(size = 6))# Change font size


##soil geological zone

train_tc %>% 
  ggplot()+geom_bar(aes(x=name_ctt,fill=name_ctt)) +scale_y_continuous(trans='log10')+
  facet_wrap(~s_geo_type,labeller = labeller(s_geo_type = 
                                               c("S_geo_7" = "Igneous or Metamorphic",
                                                 "S_geo_1" = "Alluvium",
                                                 "S_geo_2" = "Glacial",
                                                 "S_geo_5"= "Mixed sedimentary"),nrow = 2,ncol=2))+
  labs(x=NULL, y='Count in log10')+
  labs(title="Cover tree type distribution by soil geological zone",fill="cover tree types")+
  scale_x_discrete(labels = NULL,breaks=NULL)+                                                                # Change font size
  theme(strip.text.x = element_text(size = 7))



#quantitative variables
#unfortunately did not work the automatization with for or sapply
a<-train_tc %>% 
  ggplot()+geom_boxplot(aes(x=name_ctt,y=Elevation))+
  labs(title="Elevation")+
  labs(x=NULL, y='meters')+
  theme(axis.text.x = element_text(angle=45, hjust=1),
        plot.title = element_text(size=10))
b<-train_tc %>% 
  ggplot()+geom_boxplot(aes(x=name_ctt,y=Slope))+
  labs(title="Slope")+
  labs(x=NULL, y='degrees')+
  theme(axis.text.x = element_text(angle=45, hjust=1),
        plot.title = element_text(size=14))
c<-train_tc %>% 
  ggplot()+geom_boxplot(aes(x=name_ctt,y=Horizontal_Distance_To_Hydrology))+
  labs(title="Horizontal distance to_hydrology")+
  labs(x=NULL, y='meters')+
  theme(axis.text.x = element_text(angle=45, hjust=1),
        plot.title = element_text(size=14))
d<-train_tc %>% 
  ggplot()+geom_boxplot(aes(x=name_ctt,y=Vertical_Distance_To_Hydrology))+
  labs(title="Vertical distance to hydrology")+
  labs(x=NULL, y='meters')+
  theme(axis.text.x = element_text(angle=45, hjust=1),
        plot.title = element_text(size=14))
e<-train_tc %>% 
  ggplot()+geom_boxplot(aes(x=name_ctt,y=Horizontal_Distance_To_Roadways))+
  labs(title="Horizontal distance to roadways")+
  labs(x=NULL, y='meters')+
  theme(axis.text.x = element_text(angle=45, hjust=1),
        plot.title = element_text(size=14))
f<-train_tc %>% 
  ggplot()+geom_boxplot(aes(x=name_ctt,y=Hillshade_9am))+
  labs(title="Hillshade 9am")+
  labs(x=NULL, y='0 to 255 index')+
  theme(axis.text.x = element_text(angle=45, hjust=1),
        plot.title = element_text(size=14))
g<-train_tc %>% 
  ggplot()+geom_boxplot(aes(x=name_ctt,y=Hillshade_Noon))+
  labs(title="Hillshade Noon")+
  labs(x=NULL, y='0 to 255 index')+
  theme(axis.text.x = element_text(angle=45, hjust=1),
        plot.title = element_text(size=14))
h<-train_tc %>% 
  ggplot()+geom_boxplot(aes(x=name_ctt,y=Hillshade_3pm))+
  labs(title="Hillshade 3pm")+
  labs(x=NULL, y='0 to 255 index')+
  theme(axis.text.x = element_text(angle=45, hjust=1),
        plot.title = element_text(size=14))
i<-train_tc %>% 
  ggplot()+geom_boxplot(aes(x=name_ctt,y=Horizontal_Distance_To_Fire_Points))+
  labs(title="Horizontal distance to fire points")+
  labs(x=NULL, y='meters')+
  theme(axis.text.x = element_text(angle=45, hjust=1),
        plot.title = element_text(size=14))

#Arranging hillshade variables
grid.arrange(f, g, h,ncol = 3)
#Arranging distances variables
grid.arrange(c,d,e,i,ncol = 2,nrow=2)

#one way ANOVA

#ANOVA allows to compare multiple groups of data. 
#measures the mean of independent groups 
#taking out outliers
train_tc%>%filter(Hillshade_Noon>=190) %>% 
  ggplot()+geom_boxplot(aes(x=name_ctt,y=Hillshade_Noon))+
  labs(title="Hillshade Noon")+
  labs(x=NULL, y='0 to 255 index')+
  theme(axis.text.x = element_text(angle=45, hjust=1),
        plot.title = element_text(size=14))
#calculating in-group variance and intra-group variance
#p-value below 0.05 the variable is not correlated
#p-value above 0.05 the variable is correlated
one.way <- aov(Hillshade_Noon ~name_ctt, data = train_tc)
summary(one.way)
#the alternative hypothesis is statistically significant
#the hillshade noon values are significantly different

#then the plots and ANOVA result are deleted 
rm(a, b,c,d,e,f,g,h,i, one.way)

#Random Forest approach 
#Creation of subsamples for the training and test dataset,
#due to imbalance of the cover type classification
#Is applied a down-sampling tuning 
set.seed(13, sample.kind="Rounding")# the seed is a random selection
down_train <- downSample(x = train_tc[, -ncol(train_tc)+1],
                         y = factor(train_tc$Cover_Type))
index<-sample(nrow(down_train))

x<-train_tc[index,]# 15449 units
y<-factor(train_tc$Cover_Type[index])

down_test <- downSample(x = validation[, -ncol(validation)+1],
                         y = factor(validation$Cover_Type))
test_rf_index<-sample(nrow(down_test))
x_rf_test<-validation[test_rf_index,] #3780 units
y_rf_test<-factor(validation$Cover_Type[test_rf_index])

#Saving & Loading
save(x_rf_test,file="rda_data/x_rf_test.rda")
save(y_rf_test,file="rda_data/y_rf_test.rda")
save(x,file="rda_data/x.rda")
save(y,file="rda_data/y.rda")
load("rda_data/x.rda")
load("rda_data/y.rda")
load("rda_data/x_rf_test.rda")
load("rda_data/y_rf_test.rda")

#aspect is not considered
col_index<-colnames(x)[c(2,4:11)]#Quantitative variables..
col_index2<-colnames(x)[c(2,4:11,34:36)]#Quantitative variables + Nominal Wilderness Area and Soil variables.
col_index3<-colnames(x)[c(2,4:15,23:33)]#Quantitative variables + Boolean Wilderness Area and Soil variables.
col_index4<-colnames(x)[c(2,4:11,34:35)]#Quantitative variables + Nominal Soil variables.
col_index5<-colnames(x)[c(2,4:11,23:33)]#Quantitative variables + Boolean Soil variables.
col_index6<-colnames(x)[c(2,4:11,36)]#Quantitative variables + Nominal Wilderness Area.
col_index7<-colnames(x)[c(2,4:15)]#Quantitative variables + Boolean Wilderness Area.

#identifying the best number of Nodes and combination of variables.
#it was compared which type of variable (boolean or qualitative) are the best option
#Additionally, different combinations with the selected variables


control <- trainControl(method="cv", number = 3)#three cross validation
grid <- data.frame(mtry = c(1,seq(5,25,5)))#number of randomly selected predictors 
#for each split.
train_rf <- train(x[, col_index], y,
                  method = "rf",#indicates random forest method
                  ntree = 150,#number of trees for each forest
                  trControl = control,
                  tuneGrid = grid,
                  nSamp = 10000)#random sample of observations for each tree


train_rf2 <- train(x[, col_index2], y,
                  method = "rf",#indicates random forest method
                  ntree = 150,#number of trees for each forest
                  trControl = control,
                  tuneGrid = grid,
                  nSamp = 10000)#random sample of observations for each tree


train_rf3 <- train(x[, col_index3], y,
                   method = "rf",#indicates random forest method
                   ntree = 150,#number of trees for each forest
                   trControl = control,
                   tuneGrid = grid,
                   nSamp = 10000)#random sample of observations for each tree


train_rf4 <- train(x[, col_index4], y,
                   method = "rf",#indicates random forest method
                   ntree = 150,#number of trees for each forest
                   trControl = control,
                   tuneGrid = grid,
                   nSamp = 10000)#random sample of observations for each tree


train_rf5 <- train(x[, col_index5], y,
                   method = "rf",#indicates random forest method
                   ntree = 150,#number of trees for each forest
                   trControl = control,
                   tuneGrid = grid,
                   nSamp = 10000)#random sample of observations for each tree


train_rf6 <- train(x[, col_index6], y,
                   method = "rf",#indicates random forest method
                   ntree = 150,#number of trees for each forest
                   trControl = control,
                   tuneGrid = grid,
                   nSamp = 10000)#random sample of observations for each tree

train_rf7 <- train(x[, col_index7], y,
                   method = "rf",#indicates random forest method
                   ntree = 150,#number of trees for each forest
                   trControl = control,
                   tuneGrid = grid,
                   nSamp = 10000)#random sample of observations for each tree

acc_max_t<-data.frame(c(1,2,3,4,5,6,7),c(max(train_rf$results$Accuracy),
             max(train_rf2$results$Accuracy),
             max(train_rf3$results$Accuracy),
             max(train_rf4$results$Accuracy),
             max(train_rf5$results$Accuracy),
             max(train_rf6$results$Accuracy),
             max(train_rf7$results$Accuracy)))
colnames(acc_max_t)<-c("tr_group","max. acc")#column names
max(acc_max_t$`max. acc`)# the best value was gotten with the sample 3
save(acc_max_t,file="rda_data/acc_max_t.rda")#save table accuracy


#Arranging distances variables
load(file="rda_data/train_rf.rda")
load(file="rda_data/train_rf2.rda")
load(file="rda_data/train_rf3.rda")
load(file="rda_data/train_rf4.rda")
load(file="rda_data/train_rf5.rda")
load(file="rda_data/train_rf6.rda")
load(file="rda_data/train_rf7.rda")

#plots comparisons of differents algorithms. Comparing accuracy with best tune
grid.arrange(ggplot(train_rf)+
               labs(title="train_rf1")+
               labs(x=NULL, y='accuracy')+
               ylim(0.55, 0.9)+
               geom_point(aes(x=train_rf$bestTune$mtry,y=max(train_rf$results$Accuracy),color='red'),size=2.5,
                          show.legend = FALSE),ggplot(train_rf2)+
               labs(title="train_rf2")+
               labs(x=NULL, y='accuracy')+
               ylim(0.55, 0.9)+
               geom_point(aes(x=train_rf2$bestTune$mtry,y=max(train_rf2$results$Accuracy),color='red'),size=2.5,
                          show.legend = FALSE),ggplot(train_rf3)+
               labs(title="train_rf3")+
               labs(x=NULL, y='accuracy')+
               ylim(0.55, 0.9)+
               geom_point(aes(x=train_rf3$bestTune$mtry,y=max(train_rf3$results$Accuracy),color='red'),size=2.5,
                          show.legend = FALSE),
             ggplot(train_rf4)+
               labs(title="train_rf4")+
               labs(x=NULL, y='accuracy')+
               ylim(0.55, 0.9)+
               geom_point(aes(x=train_rf4$bestTune$mtry,y=max(train_rf4$results$Accuracy),color='red'),size=2.5,
                          show.legend = FALSE),
             ggplot(train_rf5)+
               labs(title="train_rf5")+
               labs(x=NULL, y='accuracy')+
               ylim(0.55, 0.9)+
               geom_point(aes(x=train_rf5$bestTune$mtry,y=max(train_rf5$results$Accuracy),color='red'),size=2.5,
                          show.legend = FALSE),
             ggplot(train_rf6)+
               labs(title="train_rf6")+
               labs(x=NULL, y='accuracy')+
               ylim(0.55, 0.9)+
               geom_point(aes(x=train_rf6$bestTune$mtry,y=max(train_rf6$results$Accuracy),color='red'),size=2.5,
                          show.legend = FALSE),
              ggplot(train_rf7)+
                labs(title="train_rf7")+
                labs(x=NULL, y='accuracy')+
                ylim(0.55, 0.9)+
                geom_point(aes(x=train_rf7$bestTune$mtry,y=max(train_rf7$results$Accuracy),color='red'),size=2.5,
                            show.legend = FALSE),ncol = 3,nrow=3)


#save and load
save(train_rf,file="rda_data/train_rf.rda")
save(train_rf2,file="rda_data/train_rf2.rda")
save(train_rf3,file="rda_data/train_rf3.rda")
save(train_rf4,file="rda_data/train_rf4.rda")
save(train_rf5,file="rda_data/train_rf5.rda")
save(train_rf6,file="rda_data/train_rf6.rda")
save(train_rf7,file="rda_data/train_rf7.rda")
save(col_index3,file="rda_data/col_index3.rda")
load(file="rda_data/train_rf.rda")
load(file="rda_data/x_rf_test.rda")
load(file="rda_data/y_rf_test.rda")


#The 3th case is the best case with 0.8723 of accuracy. The best tune is
#with 20 variables.
#to check enough trees
fit_rf <- randomForest(x[, col_index3], y,ntree = 300,
                        minNode = train_rf3$bestTune$mtry)
plot(fit_rf)

#Now it is tested in the subsample test data.
y_hat_rf <- predict(fit_rf, x_rf_test[ ,col_index3])
cm <- confusionMatrix(y_hat_rf, y_rf_test)
cm$overall["Accuracy"]
cm$byClass[,1:2]


acc_max_t<-data.frame(c(1,2,3,4,5,6,7),c(max(train_rf$results$Accuracy),
                                         max(train_rf2$results$Accuracy),
                                         max(train_rf3$results$Accuracy),
                                         max(train_rf4$results$Accuracy),
                                         max(train_rf5$results$Accuracy),
                                         max(train_rf6$results$Accuracy),
                                         max(train_rf7$results$Accuracy)))
colnames(acc_max_t)<-c("tr_group","max. acc")#column names
max(acc_max_t$`max. acc`)# the best value was gotten with the sample 3
save(acc_max_t,file="rda_data/acc_max_t.rda")#save table accuracy
#variable importance
imp<-importance(fit_rf)
max(imp)#shows the maximal variable importance value
save(imp,file="rda_data/imp.rda")
save(fit_rf,file="rda_data/fit_rf.rda")
save(y_hat_rf,file="rda_data/y_hat_rf.rda")
####################################################################################