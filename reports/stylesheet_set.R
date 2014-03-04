options(rstudio.markdownToHTML =
          function(inputFile, outputFile) {     
            require(markdown)
            markdownToHTML(inputFile, outputFile, stylesheet='stylesheet1.css')  
          }
)
