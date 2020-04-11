#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

suppressMessages(if(require(shinyhttr)==FALSE){install.packages("shinyhttr")});library("shinyhttr")
suppressMessages(if(require(sf)==FALSE){install.packages("sf")});library("sf")
suppressMessages(if(require(stringr)==FALSE){install.packages("stringr")});library("stringr")
suppressMessages(if(require(R.utils)==FALSE){install.packages("R.utils")});library("R.utils")
suppressMessages(if(require(leaflet)==FALSE){install.packages("leaflet")});library("leaflet")
suppressMessages(if(require(leaflet.extras)==FALSE){install.packages("leaflet.extras")});library("leaflet.extras")
suppressMessages(if(require(tidyverse)==FALSE){install.packages("tidyverse")});library("tidyverse")
suppressMessages(if(require(parallel)==FALSE){install.packages("parallel")});library("parallel")
suppressMessages(if(!require(sf)){install.packages('sf'); library(sf)} else {library(sf)})
suppressMessages(if(!require(raster)){install.packages('raster'); library(raster)} else {library(raster)})
suppressMessages(if(!require(sp)){install.packages('sp'); library(raster)} else {library(sp)})
suppressMessages(if(!require(dplyr)){install.packages('dplyr'); library(dplyr)} else {library(dplyr)})
suppressMessages(if(require(RColorBrewer)==FALSE){install.packages("RColorBrewer")});library("RColorBrewer")



options(shiny.maxRequestSize=30*1024^2) 
options(timeout=180)


###### Functions ########


withBusyIndicatorServer <- function(buttonId, expr) {
  
  # UX stuff: show the "busy" message, hide the other messages, disable the button
  
  loadingEl <- sprintf("[data-for-btn=%s] .btn-loading-indicator", buttonId)
  
  doneEl <- sprintf("[data-for-btn=%s] .btn-done-indicator", buttonId)
  
  errEl <- sprintf("[data-for-btn=%s] .btn-err", buttonId)
  
  shinyjs::disable(buttonId)
  
  shinyjs::show(selector = loadingEl)
  
  shinyjs::hide(selector = doneEl)
  
  shinyjs::hide(selector = errEl)
  
  on.exit({
    
    shinyjs::enable(buttonId)
    
    shinyjs::hide(selector = loadingEl)
    
  })
  
  
  
  # Try to run the code when the button is clicked and show an error message if
  
  # an error occurs or a success message if it completes
  
  tryCatch({
    
    value <- expr
    
    shinyjs::show(selector = doneEl)
    
    shinyjs::delay(2000, shinyjs::hide(selector = doneEl, anim = TRUE, animType = "fade",
                                       
                                       time = 0.5))
    
    value
    
  }, error = function(err) { errorFunc(err, buttonId) })
  
}

download_CFSV2_CPT_1=function(firs_year,last_year,i_month,ic,dir_save,area1, lg){
  
  options(timeout=180)
  
  lg_s <- lg -1
  lead <- i_month-ic
  if(lead<0)lead <- lead + 12
  route <- paste0("http://iridl.ldeo.columbia.edu/SOURCES/.NOAA/.NCEP/.EMC/.CFSv2/.ENSEMBLE/.OCNF/.surface/.TMP/SOURCES/.NOAA/.NCEP/.EMC/.CFSv2/.REALTIME_ENSEMBLE/.OCNF/.surface/.TMP/appendstream/350/maskge/S/%280000%201%20",month.abb[ic],"%20",firs_year,"-",last_year,"%29VALUES/L/",lead,".5/",lead+lg_s,".5/RANGE%5BL%5D//keepgrids/average/M/1/24/RANGE%5BM%5Daverage/Y/%28",area1[4],"%29%28",area1[3],"%29RANGEEDGES/X/%28",area1[1],"%29%28",area1[2],"%29RANGEEDGES/-999/setmissing_value/%5BX/Y%5D%5BS/L/add%5Dcptv10.tsv.gz")
  
  trimestrel <- (ic+lead):(ic+lead+lg_s)
  if(sum(trimestrel>12)>0)trimestrel[which(trimestrel>12)]=trimestrel[which(trimestrel>12)]-12
  path_save <- paste0(dir_save,"/",month.abb[ic],"_",paste(month.abb[trimestrel],collapse = "-"),".tsv.gz")
  download.file(route,path_save)
  gunzip(path_save)
  
  return(paste("Successful download",path_save))
}

download_CFSV2_CPT_2=function(firs_year,last_year,i_month,ic,dir_save,area1,lg,area2){
  
  options(timeout=180)
  
  lg_s <-lg-1
  lead <- i_month-ic
  if(lead<0)lead <- lead + 12
  route <- paste0("http://iridl.ldeo.columbia.edu/SOURCES/.NOAA/.NCEP/.EMC/.CFSv2/.ENSEMBLE/.OCNF/.surface/.TMP/SOURCES/.NOAA/.NCEP/.EMC/.CFSv2/.REALTIME_ENSEMBLE/.OCNF/.surface/.TMP/appendstream/350/maskge/S/%280000%201%20",month.abb[ic],"%20",firs_year,"-",last_year,"%29/VALUES/L/",lead,".5/",lead+lg_s,".5/RANGE/%5BL%5D//keepgrids/average/M/1/24/RANGE/%5BM%5Daverage/X/",area1[1],"/",area1[2],"/flagrange/Y/",area1[3],"/",area1[4],"/flagrange/add/1/flaggt/X/",area2[1],"/",area2[2],"/flagrange/Y/",area2[3],"/",area2[4],"/flagrange/add/1/flaggt/add/mul/0/setmissing_value/-999/replaceNaN/%5BX/Y%5D%5BS/L/add/%5Dcptv10.tsv.gz")
  trimestrel <- (ic+lead):(ic+lead+lg_s)
  if(sum(trimestrel>12)>0)trimestrel[which(trimestrel>12)]=trimestrel[which(trimestrel>12)]-12
  path_save <- paste0(dir_save,"/",month.abb[ic],"_",paste(month.abb[trimestrel],collapse = "-"),".tsv.gz")
  download.file(route,path_save)
  gunzip(path_save)
  
  return("Successful download")
  
}

transform_raster=function(x,y){
  mapa_base=raster(ext=y, res=c(1,1))
  val=c(as.matrix(t(x),ncol=1,byrow = T))
  val=as.numeric(val)
  val[val==-999|val== 0]=NA
  raster::values(mapa_base)=val
  return(mapa_base)
}

data_raster=function(dates){
  
  year_month=dates[1,][!is.na(dates[1,])]
  year=ifelse(substr(year_month[-1],6,7)=="12",substr(year_month[-1],9,12),substr(year_month[-1],1,4))
  data_cpt1=na.omit(dates)
  pos=which(data_cpt1[,1]=="")
  pos=sort(rep(year,pos[2]-pos[1]))
  list_dates=split(data_cpt1,pos)
  lon=as.numeric(as.character(list_dates[[1]][1,-1]))
  lat=as.numeric(as.character(list_dates[[1]][-1,1]))
  cos_lat=diag(sqrt(cos((pi/180)*lat)))
  tables=lapply(list_dates,"[",-1,-1)
  tables_numeric=lapply(tables,function(x)sapply(x,function(y)as.numeric(as.character(y))))
  ex=raster::extent(min(lon)-0.5,max(lon)+0.5,min(lat)-0.5,max(lat)+0.5)
  all_raster=lapply(tables_numeric,transform_raster,ex)
  layers=stack(all_raster)
  return(layers)
  
}

run_cpt=function(x,y,run,output,modes_x,modes_y,modes_cca,trans,type_trans){
  
  file_y=read.table(y,sep="\t",dec=".",skip =3,fill=TRUE,na.strings =-999,stringsAsFactors=FALSE)
  p=dim(file_y)[2]-1
  mode_y=modes_y
  if(p<10)mode_y=p
  mode_cca=modes_cca
  if(p<5)mode_cca=p
  
  t=ifelse(trans==1,541," ")
  
  GI=paste0(output,"_GI.txt"); pear=paste0(output,"_pearson.txt"); afc=paste0(output,"_2afc.txt")
  prob=paste0(output,"_prob.txt");roc_a=paste0(output,"_roc_a.txt");roc_b=paste0(output ,"_roc_b.txt")
  pca_eigen_x=paste0(output,"_pca_eigen_x.txt"); pca_load_x=paste0(output,"_pca_load_x.txt"); pca_scores_x=paste0(output,"_pca_scores_x.txt")
  pca_eigen_y=paste0(output,"_pca_eigen_y.txt"); pca_load_y=paste0(output,"_pca_load_y.txt"); pca_scores_y=paste0(output,"_pca_scores_y.txt")
  cca_load_x=paste0(output,"_cca_load_x.txt"); cca_cc=paste0(output,"_cca_cc.txt"); cca_scores_x=paste0(output,"_cca_scores_x.txt")
  cca_load_y=paste0(output,"_cca_load_y.txt"); cca_scores_y=paste0(output,"_cca_scores_y.txt")
  
  hit_s=paste0(output,"_hit_s.txt")
  hit_ss=paste0(output,"_hit_ss.txt")
  
  cmd <- "@echo off
  (
  echo 611
  echo 545
  echo 1
  echo %path_x% 
  echo /
  echo /
  echo /
  echo /
  echo 1
  echo %modex%
  echo 2
  echo %path_y%
  echo /
  echo /
  echo /
  echo /
  echo 1
  echo %modey%
  echo 1
  echo %modecca%
  echo 9
  echo 1
  echo 532
  echo /
  echo /
  echo N
  echo 2
  echo 554
  echo %typetrans%
  echo %trans%
  echo 112
  echo %path_GI%
  echo 311
  echo 451
  echo 455
  echo 413
  echo 1
  echo %path_pear%
  echo 413
  echo 3
  echo %path_2afc%
  echo 413
  echo 4 
  echo %path_hit_s%
  echo 413  
  echo 5
  echo %path_hit_ss% 
  echo 413
  echo 10
  echo %path_roc_b%
  echo 413
  echo 11
  echo %path_roc_a%
  echo 111
  echo 301
  echo %path_pca_eigen_x%
  echo 302
  echo %path_pca_load_x%
  echo 303
  echo %path_pca_scores_x%
  echo 311
  echo %path_pca_eigen_y%
  echo 312
  echo %path_pca_load_y%
  echo 313
  echo %path_pca_scores_y%
  echo 401
  echo %path_cca_cc%
  echo 411
  echo %path_cca_load_x%
  echo 412
  echo %path_cca_scores_x%
  echo 421
  echo %path_cca_load_y%
  echo 422
  echo %path_cca_scores_y%
  echo 501
  echo %path_prob%
  echo 0
  echo 0
  ) | CPT_batch.exe"
  
  cmd<-gsub("%path_x%",x,cmd)
  cmd<-gsub("%path_y%",y,cmd)
  cmd<-gsub("%path_GI%",GI,cmd)
  cmd<-gsub("%path_pear%",pear,cmd)
  cmd<-gsub("%path_2afc%",afc,cmd)
  cmd<-gsub("%path_roc_b%",roc_b,cmd)
  cmd<-gsub("%path_roc_a%",roc_a,cmd)
  cmd<-gsub("%path_prob%",prob,cmd)
  cmd<-gsub("%modey%",mode_y,cmd)
  cmd<-gsub("%modex%",modes_x,cmd)
  cmd<-gsub("%modecca%",mode_cca,cmd)
  cmd<-gsub("%typetrans%",type_trans,cmd)
  cmd<-gsub("%trans%",t,cmd)
  cmd<-gsub("%path_cca_load_x%",cca_load_x,cmd)
  cmd<-gsub("%path_cca_cc%",cca_cc,cmd)
  
  cmd<-gsub("%path_pca_eigen_x%",pca_eigen_x,cmd)
  cmd<-gsub("%path_pca_load_x%",pca_load_x,cmd)
  cmd<-gsub("%path_pca_scores_x%",pca_scores_x,cmd)
  cmd<-gsub("%path_pca_eigen_y%",pca_eigen_y,cmd)
  cmd<-gsub("%path_pca_load_y%",pca_load_y,cmd)
  cmd<-gsub("%path_pca_scores_y%",pca_scores_y,cmd)
  cmd<-gsub("%path_cca_scores_x%",cca_scores_x,cmd)
  cmd<-gsub("%path_cca_scores_y%",cca_scores_y,cmd)
  cmd<-gsub("%path_cca_load_y%",cca_load_y,cmd)
  
  cmd<-gsub("%path_hit_s%",hit_s,cmd)
  cmd<-gsub("%path_hit_ss%",hit_ss,cmd)
  
  
  write(cmd,run)
  #shell.exec(run)
  #print(run)
  #Sys.chmod(run,mode="777")
  system2(run)
  
  
}

correl <- function(x,y){
  
  y[1,1]=""
  loadings <- na.omit(y)
  loadings[loadings==-999]=NA
  pos=which(loadings[,1]=="")
  if(length(pos)==1){list_dates=list(loadings)}else{vector_split <- sort(rep(pos,pos[2]-1));list_dates <- split(loadings,vector_split)}
  tables=lapply(list_dates,"[",-1,-1)
  cor_ca=x[1:length(tables),1]
  final=Reduce("+",Map(function(x,y) abs(x)*y ,tables,cor_ca))/sum(cor_ca)
  final_vec=as.vector(as.matrix(t(final)))
  
  return(final_vec)
}

files_x=function(raster,cor,na,years,i){
  
  coor_min=apply(coordinates(raster),2,min) 
  coor_max=apply(coordinates(raster),2,max) 
  coor_all=cbind(coor_min,coor_max)
  
  year_p=paste0("cpt:T=",years)
  
  
    
    #pos_data=which(!is.na(values(raster)[,1]))
    pos_selec=which(cor<quantile(cor,i,na.rm=T))
    #pos_final=pos_data*pos_selec
    val=raster::values(raster)
    val[pos_selec,]=NA
    val[which(is.na(val),arr.ind = T)]= -999
    val_l=split(val,col(val))
    
    
    lat=sort(seq(coor_all[2,1],coor_all[2,2]),decreasing = T)
    lon=sort(seq(coor_all[1,1],coor_all[1,2]))
    val_matrix=lapply(val_l,function(x)matrix(x,length(lat),length(lon),byrow=TRUE,dimnames=list(lat,lon)))
    
    
    p="xmlns:cpt=http://iri.columbia.edu/CPT/v10/"
    p1="cpt:nfields=1"
    p2=paste0("cpt:field=ssta, ",year_p[1],", cpt:nrow=",length(lat),", cpt:ncol=",length(lon),", cpt:row=Y, cpt:col=X, cpt:units=Kelvin_scale, cpt:missing=-999")
    
    name_file=paste0(na,"_",i,".txt")
    sink(name_file)
    cat(p)
    cat("\n")
    cat(p1) 
    cat("\n")
    cat(p2) 
    cat("\n")
    u=Map(function(x,y){write.table(t(c(" ",lon)),sep="\t",col.names=F,row.names=F,quote = F);write.table(x,sep="\t",col.names=F,row.names=T,quote = F);cat(y);cat("\n")},val_matrix,c(year_p[-1],""))
    sink()
  
  
  return("Successful process")   
}

best_GI=function(x){
  
  all_path=paste0(x,"_",seq(0,0.9,0.1),"_GI.txt")
  names=substr(basename(all_path),1,nchar(basename(all_path))-4)
  all_GI=lapply(all_path,function(x)read.table(x,header=T,dec=".",skip=5))  
  best=lapply(all_GI,function(x) x[dim(x)[1],dim(x)[2]] )
  pos=which(unlist(best)==max(unlist(best)))[1]
  output=substr(names[pos],1,nchar(names[pos])-3)
  return(output)
  
}

metricas=function(x){
  
  p=read.table(paste0(x,"_pearson.txt"),header=T,dec=".",skip=2,col.names = c("id","latitud","longitud","pearson"))
  k=read.table(paste0(x,"_2afc.txt"),header=T,dec=".",skip=2,col.names = c("id","latitud","longitud","kendall"))
  g=read.table(paste0(x,"_GI.txt"),header=T,dec=".",skip=5)
  goodness=g[dim(g)[1],dim(g)[2]]
  roc_b=read.table(paste0(x,"_roc_b.txt"),header=T,dec=".",skip=2,col.names = c("id","latitud","longitud","roc_b"))
  roc_a=read.table(paste0(x,"_roc_a.txt"),header=T,dec=".",skip=2,col.names = c("id","latitud","longitud","roc_a"))
  
  hit_s=read.table(paste0(x,"_hit_s.txt"),header=T,dec=".",skip=2,col.names = c("id","latitud","longitud","hit_s"))
  hit_ss=read.table(paste0(x,"_hit_ss.txt"),header=T,dec=".",skip=2,col.names = c("id","latitud","longitud","hit_ss"))
  
  hit=merge(hit_s,hit_ss)
  
  roc=merge(roc_b,roc_a)
  p_k=merge(p,k)
  all=merge(p_k,roc)
  all_final=merge(all,hit)
  metrics=cbind(file=basename(x),all_final,goodness)
  
  below=read.table(paste0(x,"_prob.txt"),header=T,nrow=3,dec=".",fill=T,skip=3,check.names = FALSE)[-1:-2,]
  normal=read.table(paste0(x,"_prob.txt"),header=T,nrow=3,dec=".",fill=T,skip=8,check.names = FALSE)[-1:-2,]
  above=read.table(paste0(x,"_prob.txt"),header=T,nrow=3,dec=".",fill=T,skip=13,check.names = FALSE)[-1:-2,]
  coor=read.table(paste0(x,"_prob.txt"),header=T,nrow=2,dec=".",fill=T,skip=3,check.names = FALSE)
  prob=cbind(id=names(below),below=as.matrix(below)[1,],normal=as.matrix(normal)[1,],above=as.matrix(above)[1,])
  
  all_data=merge(metrics,prob)
  
  return(all_data)
}

save_areas=function(ras,cor,all_name){
  
  name=basename(all_name) 
  dec=str_split(name,"_")[[1]][3]
  cor_raster=ras[[1]]
  values(cor_raster)=cor
  q1=quantile(cor,as.numeric(dec),na.rm=T)
  jBrewColors <- brewer.pal(n = 9, name = "Reds")
  tiff(paste0(all_name,".tiff"),compression = 'lzw',height = 6.5,width = 5.7,units="in", res=150)
  par(mfrow=c(2,1))
  plot(cor_raster,main="Weighted loadings",col=jBrewColors,colNA="gray",legend.width=1,legend.shrink=1)
  plot(cor_raster>= q1,main="Selected pixels",colNA="gray",legend=F,col=jBrewColors)
  dev.off()
  
  return(print("Área seleccionada guardada en formato Raster"))
  
}

shinyServer(function(input, output, session) {

  base_map <- function(){
    leaflet("map1",options =leafletOptions(zoomControl = FALSE,minZoom = 2,maxZoom = 2,dragging = FALSE) ) %>%
      
      setMaxBounds(0, -57, 360, 85) %>%
      fitBounds(0, -90, 360, 90) %>%
      
      setView(lat = 0 ,lng = 180, zoom = 2) %>%
      addDrawToolbar(polylineOptions = F,
                     polygonOptions = F,
                     circleOptions = F,
                     markerOptions = F,
                     circleMarkerOptions = F,
                     rectangleOptions = T,
                     singleFeature = T,
                     targetGroup = "init",
                     editOptions = editToolbarOptions()) %>%
      addTiles(options = providerTileOptions(noWrap = F)) %>%
      setMaxBounds(0, -90, 360, 90) %>%
      
      addRectangles(layerId = "init" ,lng1 = 0, lng2 = 360, lat1= -45, lat2 = 45) 
  }
    
  paths<- reactiveValues()
  react_map <- reactiveVal(base_map())
  
  paths$succes1 = "FALSE"
  paths$succes2 = "FALSE"
  paths$succes3 = "FALSE"
  
  
  output$status <- renderText({
    
    if(file.exists("C:/CPT/CPT_batch.exe")){
      
     "CPT NO está instalado"
      
    }else{
      
      "CPT NO está instalado"
    }
    
  })
  
  shinyDirChoose(input, "main_dir",
                 roots = c('Documents' = Sys.getenv("HOME"), 'Local Disk' = Sys.getenv("HOMEDRIVE") ),
                 defaultRoot = "Documents",
                 session=session)
  
  observeEvent(input$main_dir, {
    
    paths$main_dir <- parseDirPath( c('Documents' = Sys.getenv("HOME"), 'Local Disk' = Sys.getenv("HOMEDRIVE") ), input$main_dir)
    
    updateTextInput(session, "text1", 
                    label = "Dir path chosen",
                    value = as.character(paths$main_dir))
  })
  
  observeEvent(input$action,{
    
    if (nchar(as.character(input$text))>0 & is.null(paths$main_dir)==FALSE ){
      
      paths$main_path <-as.character(paste0(paths$main_dir,"/",input$text))
      paths$x_path <- as.character(paste0(paths$main_path,"/input/","sst_cfsv2"))
      paths$y_path <- as.character(paste0(paths$main_path,"/input/","stations"))
      paths$outputs <- as.character(paste0(paths$main_path,"/","output"))
      paths$path_run <- as.character(paste0(paths$main_path,"/","bat_files"))
        
        
        
      dir.create(paths$main_path)
      dir.create(paths$x_path,recursive = T)
      dir.create(paths$y_path,recursive = T)
      dir.create(paths$outputs)
      dir.create(paths$path_run)
      dir.create(paste0(paths$outputs,"/raw_output"),recursive = T)
      dir.create(paste0(paths$outputs,"/all_domain"),recursive = T)
      dir.create(paste0(paths$outputs,"/opt_domain"),recursive = T)
      
      sendSweetAlert(
        session = session,
        title = "Carpeta creada",
        text = paste("En la siguiente ruta:",paths$main_path),
        type = "succes"
      )
      
      updateButton(session, "action", label = "Directorios creados", style = "success")
      paths$succes1 = "TRUE"
        
    }else{
      sendSweetAlert(
        session = session,
        title = "Error !!",
        text = "Por favor escriba un nombre de carpeta y/o seleccione una ubicación",
        type = "error"
      )
    }
    
  })


  observeEvent(input$copi,{
    
    if (nchar(as.character(input$text))>0 & is.null(paths$main_dir)== FALSE & is.null(input$upload$datapath)== FALSE){
      
      shiny::withProgress(message = "Application loading", value = 0, {
        
        shiny::incProgress(1/10)
        Sys.sleep(1)
        shiny::incProgress(5/10)
        
        path_copy <- paste0(paths$y_path,"/", input$upload$name)
        file.copy(input$upload$datapath,path_copy, overwrite = TRUE)
        updateButton(session, "copi", label = "Archivo guardado", style = "success")
        paths$succes2 = "TRUE"
      })
      
      sendSweetAlert(
        session = session,
        title = "Archivo guardado",
        text = paste("En la siguiente ruta:",paths$y_path),
        type = "succes"
      )
      
    }else{
      sendSweetAlert(
        session = session,
        title = "Error !!",
        text = "Por favor escriba un nombre de carpeta y/o seleccione una ubicación y/o seleccione un archivo",
        type = "error"
      )
    }
    
    
      
  })
  
  
  
  
  
  
  ##3 leaflet map
  
  output$map1 <- renderLeaflet({
  
    react_map()
  })
  
  shinyjs::onclick("Check1",expr = {
    updateNavbarPage(session, "nvpg1",selected = "Selector de área")
  })
    
  shinyjs::onclick("lon1_1",expr = {
    updateNavbarPage(session, "nvpg1",selected = "Selector de área")
  })
  shinyjs::onclick("lon2_1",expr = {
    updateNavbarPage(session, "nvpg1",selected = "Selector de área")
  })
  shinyjs::onclick("lat1_1",expr = {
    updateNavbarPage(session, "nvpg1",selected = "Selector de área")
  })
  shinyjs::onclick("lat2_1",expr = {
    updateNavbarPage(session, "nvpg1",selected = "Selector de área")
  })
  
observeEvent(c(input$Check1,input$lon1_1, input$lon2_1,input$lat1_1, input$lat2_1,input$lon1_2, input$lon2_2,input$lat1_2, input$lat2_2),{
  
  if(input$Check1=="1"){ 
  req(input$lon1_1, input$lon2_1,input$lat1_1, input$lat2_1) 
  
  paths$lon1 <- input$lon1_1
  paths$lon2 <- input$lon2_1
  paths$lat1 <- input$lat1_1
  paths$lat2 <- input$lat2_1
  
  react_map(base_map())
  leafletProxy("map1") %>% 
  clearShapes()  %>%
  addRectangles(layerId = "init",lng1 = paths$lon1, lng2 =  paths$lon2, lat1= paths$lat1, lat2 = paths$lat2 )
  
  }else{
    
    req(input$lon1_2, input$lon2_2,input$lat1_2, input$lat2_2) 
    
    paths$lon1_2 <- input$lon1_2
    paths$lon2_2 <- input$lon2_2
    paths$lat1_2 <- input$lat1_2
    paths$lat2_2 <- input$lat2_2
    
    react_map(base_map())
    leafletProxy("map1") %>% 
      clearShapes()  %>%
      addRectangles(layerId = "init",lng1 = paths$lon1_2, lng2 =  paths$lon2_2, lat1= paths$lat1_2, lat2 = paths$lat2_2 )
    
    
  }
  
})  



observeEvent(input$map1_draw_new_feature,{
    
    if(input$Check1=="1"){
    req(input$map1_draw_new_feature)
    
    feature <- input$map1_draw_new_feature$geometry$coordinates[[1]]
    
    
    paths$lon1 <- round(feature[[1]][[1]])
    paths$lon2 <- round(feature[[3]][[1]])
    paths$lat1 <- round(feature[[1]][[2]])
    paths$lat2 <- round(feature[[2]][[2]])
    
    
    #addRectangles(lng1 = feature[[1]][[1]], lng2 = feature[[3]][[1]], lat1= feature[[1]][[2]], lat2 = feature[[2]][[2]])
    updateNumericInput(session, "lon1_1",value=round(feature[[1]][[1]]))
    updateNumericInput(session, "lon2_1",value=round(feature[[3]][[1]]))
    updateNumericInput(session, "lat1_1",value=round(feature[[1]][[2]]))
    updateNumericInput(session, "lat2_1",value=round(feature[[2]][[2]]))
    
    }else{
      
    req(input$map1_draw_new_feature)
      
    feature <- input$map1_draw_new_feature$geometry$coordinates[[1]]
      
      
    paths$lon1_2 <- round(feature[[1]][[1]])
    paths$lon2_2 <- round(feature[[3]][[1]])
    paths$lat1_2 <- round(feature[[1]][[2]])
    paths$lat2_2 <- round(feature[[2]][[2]])
      
      
      #addRectangles(lng1 = feature[[1]][[1]], lng2 = feature[[3]][[1]], lat1= feature[[1]][[2]], lat2 = feature[[2]][[2]])
    updateNumericInput(session, "lon1_2",value=round(feature[[1]][[1]]))
    updateNumericInput(session, "lon2_2",value=round(feature[[3]][[1]]))
    updateNumericInput(session, "lat1_2",value=round(feature[[1]][[2]]))
    updateNumericInput(session, "lat2_2",value=round(feature[[2]][[2]]))
      
      
      
    }
  })
  

  
  
  shinyjs::onclick("start",expr = {
    print(list.files(paths$x_path,full.names = T))
  })
  
  observeEvent(input$download,{
    
    if (nchar(as.character(input$text))>0 & is.null(paths$main_dir)== FALSE){
    
         if(input$Check1=="1"){
           
               if(!is.na(input$syear) & !is.na(input$lyear)& !is.na(input$lon1_1) & !is.na(input$lon2_1) & !is.na(input$lat1_1) & !is.na(input$lat2_1)){
               
                   area <- c(as.numeric(input$lon1_1),as.numeric(input$lon2_1),as.numeric(input$lat1_1),as.numeric(input$lat2_1))
                   print(area)
                   
                   withBusyIndicatorServer("download",{ 
                     withProgress(message = "Descargando TSM", value = 0, {
                        download_CFSV2_CPT_1(firs_year=input$syear,last_year=input$lyear,i_month=as.numeric(input$start),ic=as.numeric(input$ic),dir_save=paths$x_path,area1=area,lg=as.numeric(input$length))
                     })
                   })  
                   
                   sendSweetAlert(
                     session = session,
                     title = "TSM descargada",
                     text = paste("En la siguiente ruta:",paste(paths$main_path,"/sst")),
                     type = "succes"
                   )
                   
                   updateButton(session, "download", label = "TSM descargada", style = "success")
                   paths$succes3 = "TRUE"
               }else{
                 sendSweetAlert(
                   session = session,
                   title = "Error !!",
                   text = "Por favor escriba las coordenadas en el primer área predictora y/o los años de descarga",
                   type = "error"
                 )
                }   
                 
         }else{
           
           if(!is.na(input$syear) & !is.na(input$lyear)& !is.na(input$lon1_1) & !is.na(input$lon2_1) & !is.na(input$lat1_1) & !is.na(input$lat2_1) & !is.na(input$lon1_2) & !is.na(input$lon2_2) & !is.na(input$lat1_2) & !is.na(input$lat2_2)){
             
             area <- c(as.numeric(input$lon1_1),as.numeric(input$lon2_1),as.numeric(input$lat1_1),as.numeric(input$lat2_1))
             area1 <- c(as.numeric(input$lon1_2),as.numeric(input$lon2_2),as.numeric(input$lat1_2),as.numeric(input$lat2_2))
             
             a_1=cbind(x=c(area[1],area[2],area[2],area[1],area[1]),y=c(area[3],area[3],area[4],area[4],area[3]))
             a_2=cbind(x=c(area1[1],area1[2],area1[2],area1[1],area1[1]),y=c(area1[3],area1[3],area1[4],area1[4],area1[3]))
             
             pol = st_polygon(list(a_1))
             pol1 = st_polygon(list(a_2))
             
             lst = st_intersects(pol,pol1,sparse = FALSE)
             
                  if( lst =="FALSE"){           
             
                       withBusyIndicatorServer("download",{ 
                         withProgress(message = "Descargando TSM", value = 0, {
                           download_CFSV2_CPT_2(firs_year=input$syear,last_year=input$lyear,i_month=as.numeric(input$start),ic=as.numeric(input$ic),dir_save=paths$x_path,area1=area,lg=as.numeric(input$length),area2 = area1)
                         })
                       })  
                    
                    sendSweetAlert(
                      session = session,
                      title = "TSM descargada",
                      text = paste("En la siguiente ruta:",paste(paths$main_path,"/sst")),
                      type = "succes"
                    )
                    
                    
                       updateButton(session, "download", label = "TSM descargada", style = "success")
                       paths$succes3 = "TRUE"
                  }else{
                    sendSweetAlert(
                      session = session,
                      title = "Error !!",
                      text = "Areas mal definidas, las areas no se deben sobreponer",
                      type = "error"
                    )
                    
                  } 
             
           }else{
             
             sendSweetAlert(
               session = session,
               title = "Error !!",
               text = "Por favor escriba las coordenadas en la primera y/o segunda área predictora y/o los años de descarga",
               type = "error"
             )
             
           }
           
           
         }
      
    }else{
      
      sendSweetAlert(
        session = session,
        title = "Error !!",
        text = "Por favor escriba un nombre de carpeta y/o seleccione una ubicación",
        type = "error"
      )
      
      
    }
    
    
    
  })
  
  observeEvent(c(input$start,input$length,input$ic),{
    
    meses <- c("Enero","Febrero","Marzo","Abril","Mayo","Junio","Julio",
               "Agosto","Septiembre","Octubre","Noviembre","Diciembre","Enero","Febrero")
    
    text=paste(meses[as.numeric(input$start):(as.numeric(input$start)+as.numeric(input$length)-1)],collapse="-")
    text1=paste0(text," Inicializado en ",meses[as.numeric(input$ic)])
    
    updateTextInput(session, "text3", 
                    label= "Seleccion Actual",
                    value = text1)
    
  } )
  
  
  output$sliderInputUI <- renderUI({
    if (paths$succes1 == TRUE & paths$succes2 == TRUE & paths$succes3 == TRUE) {
      bsButton("run", label = "Generar predicciones", style = "primary") 
      
    }
  })
  
  output$bsTooltipUI <- renderUI({
    if (paths$succes1 == TRUE & paths$succes2 == TRUE & paths$succes3 == TRUE) {
      #bsButton("run", label = "Generar predicciones", style = "primary") 
      bsTooltip(id = "run", title = "Realiza una corrida de CPT", placement = "left", trigger = "hover")
      
    }
  })
  
  observeEvent(input$run,{
     
    withProgress(message = 'Definiendo Parametros', value = 0,detail="0%", {
    
    modes_x <- 2
    modes_y <- 2
    modes_cca <- 1
    trans <- 0       ###### 1 si quiere hacer transformacion y 0 si no quiere hacer transformacion
    type_trans <- 2
    
    x_file=list.files(paths$x_path,full.names = T)
    y_file=list.files(paths$y_path,full.names = T)
    names_x <-  substr(basename(x_file),1,nchar(basename(x_file))-4)
    out_file = paste0(paths$main_dir,"/",input$text,"/output/raw_output/",names_x,"_0")
    run_file = paste0(paths$main_dir,"/",input$text,"/bat_files/",names_x,"_0",".bat")
    
    print(x_file)
    print(y_file)
    print(names_x)
    print(out_file)
    print(run_file)
    
   incProgress(1/10,message = "Realizando primera corrida",detail = paste0(10,"%")) 
    
   first_run <- run_cpt(x_file,y_file,run_file,out_file,modes_x,modes_y,modes_cca,trans,type_trans)
   
   cat("\n Primera corrida realizada")
   
   incProgress(1/10,message = "Cargando datos en formato raster",detail = paste0(20,"%"))
   
   tsm_list <- read.table(x_file,sep="\t",dec=".",skip =2,fill=TRUE,na.strings =-999,stringsAsFactors=FALSE)
   time=as.character(tsm_list[1,])[-1]
   time_sel=time[time!="NA"]
   tsm_raster <- data_raster(tsm_list)
   
   
   cat("\n Datos cargados en formato raster")
   
   incProgress(1/10,message = "Calculando correlación",detail = paste0(30,"%"))
   
   path_cc <- list.files(paste0(paths$main_dir,"/",input$text,"/output/raw_output/"),full.names = T,pattern = "cca_cc")
   path_load <- list.files(paste0(paths$main_dir,"/",input$text,"/output/raw_output/"),full.names = T,pattern = "cca_load_x")
   cc <-  read.table(path_cc,sep="\t",dec=".",header = T,row.names = 1,skip =2,fill=TRUE,na.strings =-999,stringsAsFactors=FALSE)
   load <- read.table(path_load,sep="\t",dec=".",skip =2,fill=TRUE,na.strings =-999,stringsAsFactors=FALSE)
   cor_tsm <-correl(cc,load)
   print(cor_tsm)
   print(tsm_raster)
  
   print(time_sel)
   
   
   cat("\n Correlación calculada")
   
   incProgress(1/10,message = "Construyendo archivos tsm",detail = paste0(40,"%"))
   
   decil <- seq(0.1,0.9,0.1)
   names_selec <-paste0(paths$main_dir,"/",input$text,"/input/sst_cfsv2/",names_x)
   print(names_selec)
   
   numCores <- detectCores()
   numCores
   cl <- makeCluster(numCores-1)
   clusterExport(cl,list("files_x","tsm_raster","cor_tsm","names_selec","time_sel","decil"),envir=environment()) 
   clusterEvalQ(cl, library("sp"))
   clusterEvalQ(cl, library("raster"))
   
   o_empty_1 <- clusterMap(cl,files_x,list(tsm_raster),list(cor_tsm),list(names_selec),list(time_sel),decil)
   
   cat("\n Archivos de la TSM construidos por deciles para CPT \n")
   
   incProgress(1/10,message = "Realizando segunda corrida",detail = paste0(50,"%"))
   
   path_x_2 <- list.files(paste0(paths$main_dir,"/",input$text,"/input/sst_cfsv2"),full.names = T,pattern = "0.")
   print(path_x_2)
   names_x_2 <-  substr(basename(path_x_2),1,nchar(basename(path_x_2))-4)
   print(names_x_2)
   path_run_2 <- paste0(paths$main_dir,"/",input$text,"/bat_files/",names_x_2,".bat")
   print(path_run_2)
   path_output_2 <-  paste0(paths$main_dir,"/",input$text,"/output/raw_output/",names_x_2)
   print(path_output_2)
   
   
   clusterExport(cl,list("run_cpt","path_x_2","y_file","path_run_2","path_output_2","modes_x","modes_y","modes_cca","trans","type_trans"),envir=environment()) 
   
   
   o_empty_2 <-clusterMap(cl,run_cpt,path_x_2,y_file,path_run_2,path_output_2,modes_x,modes_y,modes_cca,trans,type_trans)
   
   stopCluster(cl)
   
   cat("\n Segunda corrida realizada\n")
   
   incProgress(1/10,message = "seleccionando mejor corrida",detail = paste0(70,"%"))
   
   folder_output <- paste0(paths$main_dir,"/",input$text,"/output/raw_output/",names_x)
   best_decil=lapply(folder_output,best_GI) %>% lapply(.,unlist)
   print(best_decil)
   cat("\n Mejor corrida seleccionada \n")
   
   incProgress(1/10,message = "Guardando metricas del área optimizada",detail = paste0(80,"%"))
   
   best_path <- paste0(paths$main_dir,"/",input$text,"/output/raw_output/",best_decil)
   all_metricas<- metricas(best_path)
   o_empty_3= write.csv(all_metricas,paste0(paths$main_dir,"/",input$text,"/output/opt_domain/metrics.csv"),row.names=FALSE)
  
   cat("\n Metricas con dominio optimizado almacenadas \n")
   
   incProgress(1/10,message = "Guardando metricas de toda el área",detail = paste0(90,"%"))
   
   normal_path <-  paste0(folder_output,"_0")
   all_metricas_n<- metricas(normal_path)
   o_empty_4=write.csv(all_metricas_n,paste0(paths$main_dir,"/",input$text,"/output/all_domain/metrics.csv"),row.names=FALSE)
   
   cat("\n Metricas con todo el dominio almacenadas \n")
   
   incProgress(1/10,message = "Guardando áreas optimizadas",detail = paste0(95,"%"))
   o_empty_5 <- save_areas(tsm_raster,cor_tsm,best_path)
   
   cat("\n Pixeles selecionados almacenados en .tiff  \n")
   incProgress(1/10,message = "Proceso finalizado",detail = paste0(100,"%"))
   
   
   
   
   
  })
  
  })
  
  
})
