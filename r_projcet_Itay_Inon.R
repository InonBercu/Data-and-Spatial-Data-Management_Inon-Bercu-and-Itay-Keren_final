# library's
library(maptools)
library(sp)
library(rgdal)
library(raster)
library(rgeos)
library(ggmap)
library(mapview)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(reshape2)
library(MuMIn)
library(scales)

setwd("C:/Users/Inon/Desktop/data_menegment_Itay_Inon/database")


buffermaker <- function(b) {
  #aggrgeting the intersect of buffers in diffrent buffer radius
  #grouping by the site number and using weighted mean
  # the weight are the reltive size of the lmas zones in the over-all buffer area

            a<-data.frame(b$SitePolygo,b$Shape_Area,b$Ci,b$buff_area,b$AGE,b$POP15,
                          b$Sum_living, b$part_area)
            c<-c("SiteNum", "site_Area","Ci","buff_area","age","POP15",
                 "Built_area_sum","part")
            colnames(a)<-c

            d = a %>% group_by(SiteNum) %>% 
              summarise(
                area=mean(site_Area),
                Ci=mean(Ci),
                
                pop=weighted.mean(POP15,(part/sum(buff_area)), na.rm = TRUE),
                
                age = weighted.mean(age,(part/sum(buff_area)), na.rm = TRUE),
                
                weighted_Built_area_sum=sum(Built_area_sum, na.rm = TRUE)
                
                             )
               }

joiningthings = function(a,b,c,d,e,f){
 # joining all the tex
  
   y<-data.frame(1:58)
  x=c("SiteNum")
  colnames(y)<-x
  
  y= left_join(y,a, by = "SiteNum", all = TRUE)
  y= left_join(y,b, by = "SiteNum", all = TRUE)
  y= left_join(y,c, by = "SiteNum", all = TRUE)
  y= left_join(y,d, by = "SiteNum", all = TRUE)
  y= left_join(y,e, by = "SiteNum", all = TRUE)
  y= left_join(y,f, by = "SiteNum", all = TRUE)
  
  
  return(y) 
}

biotictable = function(a,b)  {
  # counting for each texa the number of ob
  # aggregating by site and joining
  
  r=a %>%  filter(TEX=="reptiles") 
  r=aggregate(Spname ~ SiteNum,r, length)
  x=c("SiteNum","reptiles")
  colnames(r)<-x
  
  bi=a %>%  filter(TEX=="poultry")
  bi = aggregate(Spname ~ SiteNum,bi, length)
  x=c("SiteNum","birds")
  colnames(bi)<-x
  
  am=a %>%  filter(TEX=="amphibians")
  am = aggregate(Spname ~ SiteNum,am, length)
  x=c("SiteNum","amphibians")
  colnames(am)<-x
  
  m=a %>%  filter(TEX=="mammals")
  m = aggregate(Spname ~ SiteNum,m, length)
  x=c("SiteNum","mammals")
  colnames(m)<-x
  
  bu=a %>%  filter(TEX=="butterflies")
  bu= aggregate(Spname ~ SiteNum,bu, length)
  x=c("SiteNum","butterflies")
  colnames(bu)<-x
  
  p=aggregate(index ~ SiteNum,b, length)
  x=c("SiteNum", "plants")
  colnames(p)<-x
  
  t= joiningthings(r,bi,am,m,bu,p)
  
  t[is.na(t)] = 0
  
  return(t)
  
} 

bioticandabiotic = function(a,b,c,d) {
  #joining tables on the base of site number
  ## and removing na values for ob
  ### creating the tables for testing
  
  x=full_join(a,b, by='SiteNum' )
  x=full_join(x,c, by='SiteNum' )
  x=full_join(x,d, by='SiteNum' )
  x[,7:9][is.na(x[,7:9])] = 0
  
  y=c("SiteNum","area","Ci","pop","age","weighted_Built_area_sum"
      ,"total_not_red" , "total_red","total_richness") 
  
  colnames(x)<-y
  
  return(x)
}


# creating table for each buffer
    b= read.csv("50c.csv")
    buffer50<-buffermaker(b)
    b= read.csv("100c.csv")
    buffer100<-buffermaker(b)
    b= read.csv("250c.csv")
    buffer250<-buffermaker(b)
    b= read.csv("500c.csv")
    buffer500<-buffermaker(b)


rm(b)    

#merging the animal ob with their details
dat = read.csv("animal1.csv", stringsAsFactors = F)
dat1 = read.csv("aminal_dit1.csv", stringsAsFactors = F)

      animalinsites=left_join(dat,dat1, by = "sindex") 

 
#merging the plants ob with their details and clening the data
    dat = read.csv("plants.csv", stringsAsFactors = F)
    dat$present = 1
    dat = dcast(dat,   SiteNum ~ VegSP, value.var = "present") 
    dat$SiteNum= NULL
    dat[is.na(dat)] = 0
    #write.csv(dat, "plantmatrix.csv") 
    #- manually fixing erors and special cases in the hebrew data 
    # and creating index

    dat = read.csv("plants2.csv", stringsAsFactors = F)
    dat = dat %>%  filter(index>0) 
    dat1= read.csv("plants1.csv", stringsAsFactors = F)
    plantsinsites= left_join(dat,dat1, by = "index")
   
    rm(dat, dat1) 


#aggregatung the observations by site
# for each  biotic  variable    
    
totalrichenss=biotictable(animalinsites,plantsinsites)
totalrichenss = totalrichenss %>% 
                group_by(SiteNum) %>% 
                 mutate(total=reptiles+birds+amphibians+mammals+butterflies+plants) 
  totalrichenss= totalrichenss[,c(1,8)]
        

  redanimal=animalinsites %>% 
                filter(red == 1) 
  redplants=plantsinsites %>% 
                filter(red == 1)
    
  red=biotictable(redanimal,redplants)
    red = red %>% 
            group_by(SiteNum) %>% 
                 mutate(red=reptiles+birds+amphibians+mammals+butterflies+plants)
    red= red[,c(1,8)]
    
    
      not_redanimal=animalinsites %>% 
                         filter(red == 0)
      not_redplants=plantsinsites %>% 
                         filter(red == 0)
       
       not_red=biotictable(not_redanimal,not_redplants)
        not_red = not_red %>% 
                   group_by(SiteNum) %>% 
                    mutate(not_red=reptiles+birds+amphibians+mammals+butterflies+plants)
        not_red= not_red[,c(1,8)]

        
  #joining biotic with abiotic by abiotic variables
  #  creating a table for each biotic variable vs abiotic variable in the 4 buffers scales
 
b50=bioticandabiotic(buffer50,not_red,red,totalrichenss) 
  b100=bioticandabiotic(buffer100,not_red,red,totalrichenss)  
   b250=bioticandabiotic(buffer250,not_red,red,totalrichenss)  
     b500=bioticandabiotic(buffer500,not_red,red,totalrichenss)  
          
        
     rm(red,not_red,totalrichenss,buffer50,buffer100,buffer250,buffer500,not_redanimal,not_redplants,
        plantsinsites,animalinsites,redanimal,redplants, bioticandabiotic,biotictable, buffermaker, joiningthings)        
  

###graph and test
  
## normalty
is_it_normal= function(b){
  # test normalty of the data
  shapirofun <- function(x) res <- shapiro.test(x)$p.value
  x=c(apply(b, 2, shapirofun))
  
  for(i in c(7:length(x))) {
    if(sum(x[1])<0.05){ 
      print(paste0(names(x[i]) , " is not in a normal distribution"))
    } else{
      print(paste0(names(x[i]) , " is is in a normal distribution"))
    }
    
  }
}

## area
just_area <- function(b) {
  
  fit=lm(total_not_red~log(area),b)
  r2=signif(summary(fit)$adj.r.squared)
  p=signif(summary(fit)$coef[2,4], 5)
  
  if(r2<0.5){x= print(paste0("the r2 value (", r2, ") of  area / general species is too low"))
  } else {
    if (p>0.05) {x=print(paste0("the p value (", p, ") of  area / general species is too high"))
    } else {
      x= ggplot(b, aes(x =log(area) , y = total_not_red))+
        geom_point() +ylab("Species") + geom_smooth(method = 'loess') +
        scale_x_continuous(name="log(area)", limits=c(9,17)) +
        labs(title = "Ratio between the area size and the number of general species",
             subtitle = paste("Adj R2 = ",signif(summary(fit)$adj.r.squared, 5),"Intercept =",signif(fit$coef[[1]],5 ),
                              " Slope =",signif(fit$coef[[2]], 5)," P =",signif(summary(fit)$coef[2,4], 5)))}}
  
  
  fit=lm(total_red~log(area),b)
  r2=signif(summary(fit)$adj.r.squared)
  p=signif(summary(fit)$coef[2,4], 5)
  
  if(r2<0.5){x1= print(paste0("the r2 value (", r2, ") of  area / red species is too low"))
  } else {
    if (p>0.05) {x1=print(paste0("the p value (", p, ") of  area / red species is too high"))
    } else {
      x1= ggplot(b, aes(x =log(area) , y = total_red))+
        geom_point() +ylab("Species") + geom_smooth(method = 'loess') +
        scale_x_continuous(name="log(area)", limits=c(9,17)) +
        labs(title = "Ratio between the area size and the number of red species",
             subtitle = paste("Adj R2 = ",signif(summary(fit)$adj.r.squared, 5),"Intercept =",signif(fit$coef[[1]],5 ),
                              " Slope =",signif(fit$coef[[2]], 5)," P =",signif(summary(fit)$coef[2,4], 5)))}}
  
  
  fit=lm(total_richness~log(area),b)
  r2=signif(summary(fit)$adj.r.squared)
  p=signif(summary(fit)$coef[2,4], 5)
  
  if(r2<0.5){x2= print(paste0("the r2 value (", r2, ") of  area / all species is too low"))
  } else {
    if (p>0.05) {x2=print(paste0("the p value (", p, ") of  area / all species is too high"))
    } else {
      x2= ggplot(b, aes(x =log(area) , y = total_not_red))+
        geom_point() +ylab("Species") + geom_smooth(method = 'loess') +
        scale_x_continuous(name="log(area)", limits=c(9,17)) +
        labs(title = "Ratio between the area size and the number of all species",
             subtitle = paste("Adj R2 = ",signif(summary(fit)$adj.r.squared, 5),"Intercept =",signif(fit$coef[[1]],5 ),
                              " Slope =",signif(fit$coef[[2]], 5)," P =",signif(summary(fit)$coef[2,4], 5)))}}
  
  t=list(x,x1,x2)
  t
}

# biotic vs buffer depended abiotic val 
biotic_vs_abiotic = function(b){
  a=b[,-1]
  for( i in c(6:sum(length(a)))) {
    for (j in c(2:sum(length(a)-3))){
      
      bb=cbind(a[,j],a[,i])
      d=names(bb)
      e=c("v1","v2")
      colnames(bb)=e
      
      fit=lm(v2~v1,bb)
      r2=signif(summary(fit)$adj.r.squared)
      p=signif(summary(fit)$coef[2,4], 5)
      
      if(r2<0.4){ print(paste0("the r2 value (", r2, ") of " , d[1] ,"/", d[2] , " is too low"))
        } else {
          if (p>0.05) {print(paste0("the p value (", p, ") of " , d[1] ,"/",  d[2] , " is too high"))
           } else {
              ggplot(bb, aes(x =v1 , y = v2))+
                geom_point() +ylab("Species") + geom_smooth(method = 'loess') +
                labs(subtitle = paste("Adj R2 = ",signif(summary(fit)$adj.r.squared, 5),"Intercept =",signif(fit$coef[[1]],5 ),
                                      " Slope =",signif(fit$coef[[2]], 5)," P =",signif(summary(fit)$coef[2,4], 5)))}}
    }}
}

# creating the coefficients table 
results_table = function(a,b,c) {
  
  t= data.frame(sapply(a[1], function(a) a$coefficients[2:6,1]))
  names(t) = "richness_model"
  t1= data.frame(sapply(b[1], function(b) b$coefficients[2:6,1]))
  names(t1) = "not_red_model"
  t2= data.frame(sapply(c[1], function(c) c$coefficients[2:5,1]))
  names(t2) = "red_model"
  t2[5,]="NA"
  d=cbind.data.frame(t,t1,t2)
  return(d)
  
}

# multi-reg best model finder 
richness_model = function(b) {
  
  options(na.action = "na.fail")    
  
  # Full model
  fit = glm( total_richness~area+pop+age+weighted_Built_area_sum+area:pop+area:age+area:weighted_Built_area_sum,
             data = b,  family=poisson  )
  summary(fit)
  
  # Run model selection procedure
  # Because the population and the built area represent 
  #the same thing, we will subtract the models that 
  # combine them together
  dd = dredge( fit, subset = !(pop && weighted_Built_area_sum), rank = "AIC" )

  # 'Best' model
  best = get.models(dd, 1)[[1]]
  x=summary(best)
  
  # Predicted values using 'best' model
  newdata = expand.grid( age = seq(min(b$age), max(b$age), length.out = 30),
                         area = quantile(b$area, c(0.1, 0.5, 0.9)),
                         pop = seq(min(b$pop), max(b$pop), length.out = 30))
  newdata$pred = predict(best, newdata, type = "response")
  
  
  
  y= ggplot(newdata, aes(x = age, y = pop, fill = pred)) +
    geom_tile() + geom_contour(aes(z = pred), colour = "black") +
    scale_x_continuous("Building age") + scale_y_continuous("Population", labels = comma) +
    labs(title = "prediction graph for multiple variables with all species") +
    scale_fill_distiller("Species", palette = "Spectral") +
    facet_grid(. ~ area) + theme_bw()
  
  t=list(x,y)
  
}
red_model = function(b) {
  
  options(na.action = "na.fail")    
  
  # Full model
  fit = glm( total_red~area+pop+age+weighted_Built_area_sum+area:pop+area:age+area:weighted_Built_area_sum,
             data = b,  family=poisson  )
  summary(fit)
  
  # Because the population and the built area represent 
  #the same thing, we will subtract the models that 
  # combine them together
  dd = dredge( fit, subset = !(pop && weighted_Built_area_sum), rank = "AIC" )

  # 'Best' model
  best = get.models(dd, 1)[[1]]
  x=summary(best)
  
  # Predicted values using 'best' model
  newdata = expand.grid( age = seq(min(b$age), max(b$age), length.out = 30),
                         area = quantile(b$area, c(0.1, 0.5, 0.9)),
                         pop = seq(min(b$pop), max(b$pop), length.out = 30))
  newdata$pred = predict(best, newdata, type = "response")
  
  
  
  y= ggplot(newdata, aes(x = age, y = pop, fill = pred)) +
    geom_tile() + geom_contour(aes(z = pred), colour = "black") +
    scale_x_continuous("Building age") + scale_y_continuous("Population", labels = comma) +
    labs(title = "prediction graph for multiple variables with red species") +
    scale_fill_distiller("Species", palette = "Spectral") +
    facet_grid(. ~ area) + theme_bw()
  
  t=list(x,y)
  
}
not_red_model = function(b) {
  
  # Full model
  fit = glm( total_not_red~area+pop+age+weighted_Built_area_sum+area:pop+area:age+area:weighted_Built_area_sum,
             data = b,  family=poisson  )
  summary(fit)
  
  # Because the population and the built area represent 
  #the same thing, we will subtract the models that 
  # combine them together
  dd = dredge( fit, subset = !(pop && weighted_Built_area_sum), rank = "AIC" )

  # 'Best' model
  best = get.models(dd, 1)[[1]]
  x=summary(best)
  
  # Predicted values using 'best' model
  newdata = expand.grid( age = seq(min(b$age), max(b$age), length.out = 30),
                         area = quantile(b$area, c(0.1, 0.5, 0.9)),
                         pop = seq(min(b$pop), max(b$pop), length.out = 30))
  newdata$pred = predict(best, newdata, type = "response")
  
  
  
  y= ggplot(newdata, aes(x = age, y = pop, fill = pred)) +
    geom_tile() + geom_contour(aes(z = pred), colour = "black") +
    scale_x_continuous("Building age") + scale_y_continuous("Population", labels = comma) +
    labs(title = "prediction graph for multiple variables with general species") +
    scale_fill_distiller("Species", palette = "Spectral") +
    facet_grid(. ~ area) + theme_bw()
  
  t=list(x,y)
  
}



####
### using the test funC
{
is_it_normal(b50)
 
x=just_area(b50)
x
biotic_vs_abiotic(b50)
biotic_vs_abiotic(b100) 
biotic_vs_abiotic(b250)
biotic_vs_abiotic(b500)

x= richness_model(b50)
y= not_red_model(b50)
z= red_model(b50)

results_table(x,y,z)

x[2]
y[2]
z[2]


x=richness_model(b100)
y= not_red_model(b100)
z= red_model(b100)

results_table(x,y,z)

x[2]
y[2]
z[2]


x=richness_model(b250)
y=not_red_model(b250)
z=red_model(b250)

results_table(x,y,z)

x[2]
y[2]
z[2]



x=richness_model(b500)
y=not_red_model(b500)
z=red_model(b500)

results_table(x,y,z)

x[2]
y[2]
z[2]
}

# poly plots

poly <- readOGR("C:/Users/Inon/Desktop/data_menegment_Itay_Inon/poly_data/base_spi_data.shp", 
                stringsAsFactors = FALSE)

# map view
mapview(poly)

# To table
poly$id <- as.character(1:nrow(poly))
poly_f <- fortify(poly, region = "id")
poly_f <- left_join(poly_f, poly@data, "id")

# by Shape Area
poly_Shape_Area <- ggplot() + 
  geom_polygon(
    data = poly_f, 
    aes(x = long, y = lat, group = group, fill = AREA)
  ) +
  scale_fill_distiller(palette = "Spectral") +
  coord_equal()

# by red
total_red_n <- as.numeric(poly_f$total_red)

poly_red <- ggplot() + 
  geom_polygon(
    data = poly_f, 
    aes(x = long, y = lat, group = group, fill = total_red_n)) +
  scale_fill_distiller(palette = "Spectral") +
  coord_equal()
  
# by not red
poly_no_red_n <- as.numeric(poly_f$total_not_)

poly_no_red <- ggplot() + 
  geom_polygon(
    data = poly_f, 
    aes(x = long, y = lat, group = group, fill = poly_no_red_n)) +
  scale_fill_distiller(palette = "Spectral") +
  coord_equal()

# by total
poly_total_n <- as.numeric(poly_f$total_rich)

poly_total <- ggplot() + 
  geom_polygon(
    data = poly_f, 
    aes(x = long, y = lat, group = group, fill = poly_total_n)) +
  scale_fill_distiller(palette = "Spectral") +
  coord_equal() 

# plot
poly_Shape_Area
poly_red
poly_no_red
poly_total
