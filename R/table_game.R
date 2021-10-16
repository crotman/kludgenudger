


create_latex_table <-  function(){
  

  table <- googlesheets4::read_sheet(
    "https://docs.google.com/spreadsheets/d/1KOwAe0Vd-VoHgVjwSqk_Gen9F0c-Itp8wMzC0ubFnGE",
    sheet = "table"    )
  
  

  latex <- xtable::xtable(
    table,
    align = c("p{0.6in}","p{0.3in}","p{0.3in}",'p{0.5in}','p{1in}','p{1in}',"p{1in}","p{1in}")
    
  )

  
  teste <- print(
    latex, 
    include.rownames=FALSE,
    size="\\fontsize{7pt}{9pt}\\selectfont"
  ) %>% 
    writeClipboard()
  
  
}


