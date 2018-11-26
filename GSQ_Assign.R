library(plotly)
require(plotly)
library(plyr)
library(dplyr)
library(ggplot2)
library(scales)

gsq<-read.csv("/Users/alokapte/Desktop/R/GL_R/payments data.csv", header=T)
attach(gsq)
new <- do.call( rbind , strsplit( as.character( gsq$Date.and.Time) , " " ) )
gsq<-cbind( gsq , Date = new[,1] , Time = new[,2] )
gsq$Date<-as.Date(gsq$Date, format = "%m/%d/%Y")
gsq$Month<-cut(gsq$Date, breaks = "month")

gsq_reg <- lm(Amount ~ State+Merchant, gsq)

j<-gsq%>%filter(Transaction_Type=='Sale')%>%
  ggplot(
    aes(x=Date,
        y=Amount, color=Status
    )) + geom_line() + xlab("Month") +
  theme(plot.margin=unit(c(1,0.25,1.5,1.2),"lines")) +ggtitle("Sale trend")
ggplotly(j)

k<-gsq%>%filter(Transaction_Type=='Refund')%>%
  ggplot(
    aes(x=Date,
        y=Amount, color=Status
    )) + geom_line() + xlab("Month") +
  theme(plot.margin=unit(c(1,0.25,1.5,1.2),"lines")) +ggtitle("Refund trend")
ggplotly(k)

q <- list(
  l = 100,
  r = 50,
  b = 200,
  t = 100,
  pad = 4
)
#
plot_ly(gsq, x = sort(Amount), y = State,
        mode = "markers", color = Status, type= 'bar') %>%
  layout(autosize = F, width = 800, height = 1000, title = "State wise Amt breakdown",  margin = q)

a<- gsq%>%filter(Transaction_Type=='Sale')
b<- gsq%>%filter(Transaction_Type=='Refund')

plot_ly(a, x = a$State, y = a$Amount,
        mode = "markers", color = a$Status, type= 'bar') %>%
  layout(autosize = F, width = 800, height = 500, title = "Sale Figures", margin = q)

plot_ly(b, x = b$State, y = b$Amount,
        mode = "markers", color = b$Status, type= 'bar') %>%
  layout(autosize = F, width = 800, height = 500, title = "Refund Figures", margin = q)

plot_ly(gsq, x = State, y = Amount,
        mode = "markers", color = Transaction_Type, type= 'bar') %>%
  layout(autosize = F, width = 800, height = 500, title = "State wise Amt breakdown", margin = q)

plot_ly(gsq, x = Merchant, y = Amount, text = paste("Mode: ", Status),
        mode = "markers", color = Mode.of.Payment, type= 'bar') %>%
  layout(autosize = F, width = 800, height = 500, title = "Merc wise Amt breakdown", margin = q)

#text = paste("TransT: ", Transaction_Type),
plot_ly(gsq, x = Merchant_Category, y = Amount, text = paste("Date: ", gsq$Date),
        mode = "markers", color = Mode.of.Payment, type= 'bar') %>%
  layout(autosize = F, width = 800, height = 500, title = "Merc_Cat wise Amt breakdown", margin = q)

plot_ly(gsq, x = State, y = Amount,
        mode = "markers", color = Device, type= 'bar') %>%
  layout(autosize = F, width = 800, height = 500, title = "Device wise Amt breakdown", margin = q)

plot_ly(gsq, x = State, y = Product_Category, text = paste("Amt: ", Amount),
        mode = "markers", size=Amount, color =  Transaction_Type, type= 'scatter') %>%
  layout(autosize = F, width = 800, height = 500, title = "Prod_cat wise Amt breakdown", margin = q)


state_wise <- split(gsq, gsq$State)
l <- htmltools::tagList()

for (i in 1:26){
  l[[i]] <-  plot_ly(
    x = state_wise[[i]]$Month, y = state_wise[[i]]$Transaction_Type,
    z = state_wise[[i]]$Amount, type = "heatmap",
    colorscale = "Greys")%>%
    layout(autosize = F, width = 800, height = 500, title = paste(unique(state_wise[[i]]$State), 'Spent TimeSeries', sep = " "), margin = q)
}
l

for (i in 1:26){
  
  l[[i]] <- plot_ly(state_wise[[i]], x = droplevels(state_wise[[i]]$City), y = state_wise[[i]]$Amount,
                    mode = "markers", color = state_wise[[i]]$Status, type= 'bar') %>%
    layout(autosize = F, width = 800, height = 500, title = paste(unique(state_wise[[i]]$State), 'City Figures', sep = " "), margin = q)
}
l

for (i in 1:26){
  a<- state_wise[[i]]%>%filter(Transaction_Type=='Sale')
  l[[i]] <- plot_ly(a, x = droplevels(a$City), y = a$Amount,
                    mode = "markers", color = a$Status, type= 'bar') %>%
    layout(autosize = F, width = 800, height = 500, title = paste(unique(state_wise[[i]]$State), 'Sale Figures', sep = " "), margin = q)
}
l

for (i in 1:26){
  b<- state_wise[[i]]%>%filter(Transaction_Type=='Refund')
  l[[i]] <- plot_ly(b, x = droplevels(b$City), y = b$Amount,
                    mode = "markers", color = b$Status, type= 'bar') %>%
    layout(autosize = F, width = 800, height = 500, title = paste(unique(state_wise[[i]]$State), 'Refund Figures', sep = " "), margin = q)
}
l

for (i in 1:26){
  l[[i]] <- plot_ly(state_wise[[i]], x = droplevels(state_wise[[i]]$City), y = state_wise[[i]]$Amount,
                    mode = "markers", color = state_wise[[i]]$Transaction_Type, type= 'bar') %>%
    layout(autosize = F, width = 800, height = 500, title = paste(unique(state_wise[[i]]$State), 'City Txn Figure', sep = " "), margin = q)
}
l

for (i in 1:26){
  l[[i]] <- plot_ly(state_wise[[i]], x = state_wise[[i]]$Merchant, y = state_wise[[i]]$Amount, text = paste("Mode: ", state_wise[[i]]$Status),
                    mode = "markers", color = state_wise[[i]]$Mode.of.Payment, type= 'bar') %>%
    layout(autosize = F, width = 800, height = 500, title = paste(unique(state_wise[[i]]$State), 'merc dist', sep = " "), margin = q)
}
l

for (i in 1:26){
  l[[i]] <- plot_ly(state_wise[[i]], x = state_wise[[i]]$Merchant_Category, y = state_wise[[i]]$Amount, text = paste("Date: ", state_wise[[i]]$Date),
                    mode = "markers", color = state_wise[[i]]$Mode.of.Payment, type= 'bar') %>%
    layout(autosize = F, width = 800, height = 500, title = paste(unique(state_wise[[i]]$State), 'spent in merc cat', sep = " "), margin = q)
}
l

for (i in 1:26){
  l[[i]] <- plot_ly(state_wise[[i]], x = droplevels(state_wise[[i]]$City), y = state_wise[[i]]$Amount,
                    mode = "markers", color = state_wise[[i]]$Device, type= 'bar') %>%
    layout(autosize = F, width = 800, height = 500, title = paste(unique(state_wise[[i]]$State), 'spent via device', sep = " "), margin = q)
}
l

for (i in 1:26){
  l[[i]] <- plot_ly(state_wise[[i]], x = droplevels(state_wise[[i]]$City), y = state_wise[[i]]$Product_Category, text = paste("Amt: ",state_wise[[i]]$Amount),
                    mode = "markers", size = state_wise[[i]]$Amount, type= 'scatter') %>%
    layout(autosize = F, width = 800, height = 500, title = paste(unique(state_wise[[i]]$State), 'spent product wise', sep = " "), margin = q)
}
l

for (i in 1:26){
  l[[i]] <- plot_ly(state_wise[[i]], x = droplevels(state_wise[[i]]$City), y = state_wise[[i]]$Product_Category, text = paste("Amt: ", state_wise[[i]]$Amount),
                    mode = "markers", size=state_wise[[i]]$Amount, color =  state_wise[[i]]$Transaction_Type, type= 'scatter') %>%
    layout(autosize = F, width = 800, height = 500, title = paste(unique(state_wise[[i]]$State), 'spent txn type', sep = " "), margin = q)
}
l


8695754681
