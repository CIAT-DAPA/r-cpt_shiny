#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#
pacman::p_load(tidyverse,shinyhttr,sf,stringr,R.utils , leaflet, leaflet.extras)

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
  
  shinyDirChoose(input, "xdir",
                 roots = c('Documents' = Sys.getenv("HOME"), 'Local Disk' = Sys.getenv("HOMEDRIVE") ),
                 defaultRoot = "Documents",
                 session=session)
  
  observeEvent(input$xdir, {
    
    paths$dir_x <- parseDirPath( c('Documents' = Sys.getenv("HOME"), 'Local Disk' = Sys.getenv("HOMEDRIVE") ), input$xdir)
    
    updateTextInput(session, "text1", 
                    label = "Dir path chosen",
                    value = as.character(paths$dir_x))
  })
  
  observeEvent(input$action,{
    
    if (nchar(as.character(input$text))>0 & is.null(paths$dir_x)==FALSE ){
      
      paths$main_path <-as.character(paste0(paths$dir_x,"/",input$text))
      paths$x_path <- as.character(paste0(paths$main_path,"/","sst"))
      paths$y_path <- as.character(paste0(paths$main_path,"/","stations"))
      paths$outputs <- as.character(paste0(paths$main_path,"/","output"))
      
      dir.create(paths$main_path)
      dir.create(paths$x_path)
      dir.create(paths$y_path)
      dir.create(paths$outputs)
      
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
    
    if (nchar(as.character(input$text))>0 & is.null(paths$dir_x)== FALSE & is.null(input$upload$datapath)== FALSE){
      
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
    print(input$upload$datapath)
  })
  
  observeEvent(input$download,{
    
    if (nchar(as.character(input$text))>0 & is.null(paths$dir_x)== FALSE){
    
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
  
  
  
})
