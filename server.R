library(shiny)
library(plotly)
library(ggplot2)
library(leaflet)
library(dplyr)
library(reshape2)

dat<-read.csv("EconomistData.csv")
landdata<-read.csv("landdata-states.csv",stringsAsFactors = FALSE)
dat$cpisquare<-dat$CPI^2
landdata<-melt(landdata,id.vars=c("State","Date","region"))
landdata$year<-substr(landdata$Date,1,4)
landdata$month<-paste("0",substr(landdata$Date,5,6),sep="")
landdata$Date<-paste("01",landdata$month,landdata$year,sep="-")
landdata$Date<-as.Date(landdata$Date,"%d-%m-%Y")
landdata$month<-NULL
landdata$year<-NULL


shinyServer(function(input, output){
        dat$cpisquare<-dat$CPI^2
        model1 <- lm(HDI ~ CPI, data = dat)
        model2 <- lm(HDI ~ CPI + cpisquare, data = dat)
        
        model1pred<-reactive({
                cpi_input<-input$CPIslider
                predict(model1,newdata=data.frame(CPI=cpi_input))
                
        })
        
        model2pred<-reactive({
                cpi_input<-input$CPIslider
                predict(model2,newdata=data.frame(CPI=cpi_input,cpisquare=cpi_input^2))
                
        })
        
        output$plot1<-renderPlotly({
                cpi_input<-input$CPIslider
                dat$cpisquare<-dat$CPI^2
                
                g<-ggplot(data=dat,aes(x=CPI,y=HDI,color=Region,label=Country))
                g<-g + ggtitle("Human Development and Corruption")+xlab("Corruption Index")+ylab("Human Development Index")
                g<-g+geom_point()
                
                
                
                if(input$showModel1){
                        g<-g+geom_smooth(aes(group=1),method = "lm", formula = y ~ x,color="red")
                        
                        g
                }
                if(input$showModel2){
                        g<-g+geom_smooth(aes(group=1),method = "lm", formula = y ~ x+I(x^2),color="blue")
                        
                        g      
                }
                g<-g+geom_point(aes(x=cpi_input,y=model1pred()), color="red",size=3) 
                g<-g+geom_point(aes(x=cpi_input,y=model2pred()), color="blue",size=3) 
                print(ggplotly(g))
                
        })
        output$pred1 <- renderText({
                model1pred()
        })
        
        output$pred2 <- renderText({
                model2pred()
        })
        
        data_in<-reactive({
                data_input<-input$DataInput
                state_input<-input$StateInput
                landdata<-landdata[landdata$variable==data_input & landdata$State==state_input,] 
                landdata<-landdata[order(landdata$Date),]
        })
        datatype<-reactive({
                dataname<-gsub("\\.", " ", input$DataInput)
        
        })        
        output$text <- renderText(datatype())
        output$plot2<-renderPlotly({
                g<-ggplot(data=data_in(), aes(x=Date,y=value)) 
                g<-g+ggtitle("Time Series of Land and Housing Prices by State in the United States")+xlab("Date")+ylab("Price/Index Value")
                g<-g+geom_point()+geom_line()+theme_bw()
                g<-g+theme(axis.text.y = element_text(colour="black",size=8,face="plain"))
                g<-g+ theme(axis.title.y = element_text(colour="black",size=10,angle=90,hjust=-.5,vjust=-100,face="bold"))
                g<-g+theme(plot.margin = unit(c(1,1,1,1), "cm"))
                print(ggplotly(g))       
        })
})       



                
        
