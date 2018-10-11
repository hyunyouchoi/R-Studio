
library(RColorBrewer) 
library(scales) 
library(lattice) 
library(dplyr) 
library(shiny)
library(ggplot2)
library(markdown)


server <- function(input, output, session) {

  
  shiny_studentInfo <- read.csv("shiny_studentInfo.csv")
  shiny_assessments <- read.csv("shiny_assessments.csv")
  
  shiny_assessments_courses <- read.csv("assessments_courses.csv")
  
  
  
  output$plot2 <- renderPlot({
    b = shiny_studentInfo[which(shiny_studentInfo$id == input$id ),]
    
    # myinput <-  input$info
    if (input$info == 'gender'){      
      aaa<- b%>% group_by(id, final_result,gender)%>% dplyr::summarise(k = n())
      
      p1 <- ggplot(data=aaa, aes(x=final_result, y=k,fill = final_result)) +
        geom_bar(stat="identity")+
        theme(legend.position="none")+theme(plot.title = element_text(size=20, hjust=0.5))+
        facet_grid(.~ gender)+ggtitle("Student Final Result by demography")+
        labs(x = "results", y = "counts" )+ theme(axis.text.x = element_text(face="bold", size=14),axis.text.y = element_text(face="bold", size=14))
      
      print(p1)}
    
    else if(input$info == 'region'){
      aaa<- b%>% group_by(id, final_result,region)%>% dplyr::summarise(k = n())
      p1 <- ggplot(data=aaa, aes(x=final_result, y=k,fill = final_result)) +
        geom_bar(stat="identity")+
        theme(legend.position="none")+theme(plot.title = element_text(size=20,  face="bold", hjust=0.5))+
        facet_grid(.~ region)+ggtitle("Student Final Result Group by Demography")+
        labs(x = "results", y = "counts" )+ theme(axis.text.x = element_text(size=10, angle=90),axis.text.y = element_text(face="bold", size=14))
      
      print(p1)
    }
    else if(input$info == 'highest_education'){
      aaa<- b%>% group_by(id, final_result,highest_education)%>% dplyr::summarise(k = n())
      p1 <- ggplot(data=aaa, aes(x=final_result, y=k,fill = final_result)) +
        geom_bar(stat="identity")+
        theme(legend.position="none")+theme(plot.title = element_text(size=20,  face="bold", hjust=0.5))+
        facet_grid(.~ highest_education)+ggtitle("Student Final Result by demography")+
        labs(x = "results", y = "counts" )+ theme(axis.text.x = element_text(size=10, angle=90),axis.text.y = element_text(face="bold", size=14) )
      
      print(p1)
    }
    else if(input$info == 'imd_band'){
      aaa<- b%>% group_by(id, final_result,imd_band)%>% dplyr::summarise(k = n())
      p1 <- ggplot(data=aaa, aes(x=final_result, y=k,fill = final_result)) +
        geom_bar(stat="identity")+
        theme(legend.position="none")+theme(plot.title = element_text(size=20,  face="bold", hjust=0.5))+
        facet_grid(.~ imd_band)+ggtitle("Student Final Result by demography")+
        labs(x = "results", y = "counts" )+ theme(axis.text.x = element_text(size=10, angle=90),axis.text.y = element_text(face="bold", size=14))
      
      print(p1)
    }
    else if(input$info == 'age_band'){
      aaa<- b%>% group_by(id, final_result,age_band)%>% dplyr::summarise(k = n())
      p1 <- ggplot(data=aaa, aes(x=final_result, y=k,fill = final_result)) +
        geom_bar(stat="identity")+
        theme(legend.position="none")+theme(plot.title = element_text(size=20, hjust=0.5))+
        facet_grid(.~ age_band)+ggtitle("Student Final Result by demography")+
        labs(x = "results", y = "counts" )+ theme(axis.text.x = element_text( size=14),axis.text.y = element_text(face="bold", size=14))
      
      print(p1)
    }
    else if(input$info == 'disability'){
      aaa<- b%>% group_by(id, final_result,disability)%>% dplyr::summarise(k = n())
      p1 <- ggplot(data=aaa, aes(x=final_result, y=k,fill = final_result)) +
        geom_bar(stat="identity")+
        theme(legend.position="none")+ theme(plot.title = element_text(size=19, hjust=0.5))+
        facet_grid(.~ disability)+ggtitle("Student Final Result by demography")+
        labs(x = "results", y = "counts" )+ theme(axis.text.x = element_text(size=14),axis.text.y = element_text(face="bold", size=14))
      
      print(p1)
    }
  })
  
    

     output$table <- renderTable({
       c= shiny_assessments[which(shiny_assessments$id == input$id ),]
       #print(c)
        })

  
    output$plot<-renderPlot({
    
      b = shiny_studentInfo[which(shiny_studentInfo$id == input$id ),]
        aaaa<- b%>% group_by(id, final_result)%>% dplyr::summarise(k = n())
        
        p <- ggplot(data=aaaa, aes(x=final_result, y=k, fill = final_result)) +
      geom_bar(stat="identity")+
      theme(legend.position="none")+
      ggtitle("Distribution of Student Final Result") + theme(plot.title = element_text(size=22,  hjust=0.5))+
      labs(x = "results", y = "counts" ) + theme(axis.text.x = element_text(size=14),axis.text.y = element_text(face="bold", size=14))
    
      print(p) 
    
  }, height = 400, width = 500)

    output$plot3<-renderPlot({
      shiny_assessments_courses$id <- paste(shiny_assessments_courses$code_module, shiny_assessments_courses$code_presentation)
      cccc = shiny_assessments_courses[which(shiny_assessments_courses$id == input$id ),]
      
      
      p3 <- ggplot(data=cccc, aes(x=date, y=id_assessment, color = assessment_type)) +
        ggtitle("Submission Date of Assessments") + theme(plot.title = element_text(size=22,   hjust=0.5))+
        geom_text(aes(label = date, y = id_assessment, size = weight))+ theme(axis.text.x = element_text(size=14),axis.text.y = element_text(face="bold", size=14))
      
      
      
      print(p3) 
      
    }, height = 300, width = 800)
}