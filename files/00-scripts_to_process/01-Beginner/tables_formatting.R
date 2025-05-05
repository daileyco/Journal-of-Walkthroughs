# tables with flextable


flextable(table_df) %>% 
  font(part = "all", fontname = "Arial") %>%
  fontsize(part = "header", size = 9) %>% #toprow
  fontsize(part = "body", size = 8) %>% 
  border_remove() %>%
  border(part = "header", border.bottom = fp_border_default(color = "black", width = 2)) %>%
  width(j=1:2, width = 1) %>% #column width
  merge_v(j=1) %>% 
  valign(j=1, valign = "top") %>%
  bold(part = "header") %>% 
  set_caption(caption = "caption")


flextable(table2) %>%
  delete_part(part = "header") %>%
  border_remove() %>%
  
  border(part = "body", i = which(), border.top = fp_border_default(color="black", width = 0.5)) %>% 
  border(part = "body", i = which(), border.bottom = fp_border_default(color="black", width = 2)) %>%
  font(part = "all", fontname = "Arial") %>% 
  fontsize(part = "header", size = 9) %>%
  fontsize(part = "body", size = 8) %>% 
  fontsize(part = "body", i = which(), size = 9) %>%
  
  bold(part = "body", i = c(), j = 1) %>% 
  fontsize(part = "body", i = c(), j = 1, size = 12) %>% 
  
  bold(part = "header") %>% 
  bold(part = "body", i = c()) %>%
  merge_v(j=2) %>% 
  width(j=, width = 1) %>%
  width(j=, width = 1.5) %>%
  align(part = "header", align = "center") %>%
  align(part = "body", j=, align = "right") %>% 
  align(part = "body", i = c(), align = "center") %>% 
  valign(part = "body", j=, valign = "top") %>%
  bold(part = "body", j=, i = c()) %>% 
  bold(part = "body", j=, i = c()) %>% 
  italic(part = "body", j=, i = ) %>%
  set_caption(caption = "(ref:tablecaption)") %>% 
  compose(part="body", j=, i=, value = as_paragraph("P\U0302"))

