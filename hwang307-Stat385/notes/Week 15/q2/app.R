library(shiny)

ui <- fluidPage(
    img(src='220px-Magic8ball.jpg', align = "right"),
    textInput(inputId="question",label="Ask the magic 8 ball a question:"),
    actionButton(inputId="click",label="Ask question"),
    verbatimTextOutput(outputId="ans")
)

server <- function(input,output){
    reply <- c("It is certain.","It is decidedly so.","Without a doubt.","Yes – definitely.",
               "You may rely on it.","As I see it, yes.","Most likely.","Outlook good.","Yes.",
               "Signs point to yes.","Reply hazy, try again.","Ask again later.","Better not tell you now.",
               "Cannot predict now.","Concentrate and ask again.","Don't count on it.","My reply is no.",
               "My sources say no.","Outlook not so good.","Very doubtful.")
    observeEvent(input$click,{
        output$ans <- renderPrint({print(sample(reply,1))})
    })
}

shinyApp(ui=ui,server=server)