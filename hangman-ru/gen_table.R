gen_table <- function(input, output, session) {
  
  dt = data.frame(x = c(rep(1:8,4)),
                  y = c(rep(4,8),rep(3,8),rep(2,8),rep(1,8)),
                  val = c('А', 'Б', 'В', 'Г', 'Д', 'Е', 'Ж', 'З',
                          'И', 'Й', 'К', 'Л', 'М', 'Н', 'О', 'П',
                          'Р', 'С', 'Т', 'У', 'Ф', 'Х', 'Ц', 'Ч',
                          'Ш', 'Щ', 'Ъ', 'Ы', 'Ь', 'Э', 'Ю', 'Я'),
                  col = c(rep('blue',32)),
                  stringsAsFactors = FALSE)
  
  return(dt)
}
