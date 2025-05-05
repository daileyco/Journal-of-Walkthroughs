# script to format tables and save to word docs

## packages
library(dplyr)
library(flextable)
library(officer)


## flextables

myflextable <- mydata %>%
  flextable() %>%
  
  # merge_v(j = 1:2) %>%
  # valign(j = 1:2, valign = "top") %>%
  
  font(part = "all", fontname = "Arial") %>% 
  fontsize(part = "header", size = 10) %>%
  fontsize(part = "body", size = 9) %>% 
  
  align(part = "header", align = "center") %>%
  align(part = "body", j=1:4, align = "left") %>%
  align(part = "body", j=5:9, align = "right") %>% 
  
  bold(part = "header") %>% 
  
  border_remove() %>%
  border(part = "header", border.bottom = fp_border_default(color = "black", width = 2)) %>% 
  
  
  width(., width = dim_pretty(.)$widths) %>% 
  # height_all(., height = max(dim_pretty(.)$heights)) %>%
  add_body_row(values = NA) %>%
  height_all(., height = 13/72) %>%
  height(part = "body", i = 1, height = 2/72) %>%
  hrule(part = "all", rule = "exact") %>%
  
  padding(part = "all", padding = 0) %>% 
  
  set_caption(., 
              caption = as_paragraph(as_chunk(caps[i], 
                                              props = fp_text(color = "black", 
                                                              font.size = 11, 
                                                              font.family = "Arial"))),
              fp_p = fp_par(
                text.align = "left",
                # padding = 0,
                line_spacing = 1,
                padding.bottom = 5, 
                padding.top = 0, 
                padding.left = 0, 
                padding.right = 0,
                border = fp_border(width = 0),
                word_style = "Normal"),
              align_with_table = FALSE)
                 


## write to word docs


the.page.margins <- list(bottom = 1, left = 1, top = 1, right = 1, 
                         header = 0.5, footer = 0.5, 
                         gutter = 0.5)
### one inch = 72 points
### font size is in pt
### line height for single line spacing is 1.15*font size

### word defaults 
#### Cambria size 12
#### 5pt spacing before and after
#### 0.07" indent left and right

### even without specifying title for flextable, save_as_docx adds a blank line before table
extra.width <- (0
                # # for indents
                # + 0.07*2 
                # for page margins
                + sum(unlist(the.page.margins[which(names(the.page.margins)%in%c("left", "right", "gutter"))]))
)
extra.height <- (0 
                 # for extra line
                 # + 1.15*12/72
                 # + 1.15*10/72
                 + 2*11/72
                 # + 10/72
                 + 2*2/72
                 # # for space before table first row
                 # + 5/72
                 # # for space before each row in table
                 # + 5/72*sum(sapply(c("header", "body", "footer"),
                 #                    function(part){
                 #                      nrow(sft.list[[i]][[part]]$dataset)
                 #                    }))
                 # # for space after last row in table 
                 # + 5/72 
                 # for two lines after
                 + 2*1*10/72
                 # for page margins
                 + sum(unlist(the.page.margins[which(names(the.page.margins)%in%c("top", "bottom"))])) 
)

the.page.sizes <- list(width = sft.dims[[i]][1]+extra.width, 
                       height = sft.dims[[i]][2]+extra.height) %>%
  c(., 
    list(orient = ifelse(.[["width"]]<.[["height"]], 
                         "portrait", 
                         "landscape")))


temp <- read_docx()
temp <- set_doc_properties(temp, 
                           creator = "Cody Dailey")

temp <- cursor_begin(temp)

temp <- body_add_flextable(temp, 
                           sft.list[[i]], 
                           align = "center", 
                           pos = "after", 
                           split = FALSE, 
                           topcaption = TRUE)

temp <- docx_set_paragraph_style(temp, 
                                 style_id = "Normal", 
                                 style_name = "Normal", 
                                 base_on = "Normal", 
                                 fp_p = fp_par(
                                   text.align = "left",
                                   padding = 0,
                                   line_spacing = 1,
                                   border = fp_border(width = 0),
                                   # padding.bottom,
                                   # padding.top,
                                   # padding.left,
                                   # padding.right,
                                   # border.bottom,
                                   # border.left,
                                   # border.top,
                                   # border.right,
                                   shading.color = "transparent",
                                   keep_with_next = FALSE,
                                   word_style = "Normal"
                                 ), 
                                 fp_t = fp_text(
                                   color = "black",
                                   font.size = 10,
                                   bold = FALSE,
                                   italic = FALSE,
                                   underlined = FALSE,
                                   font.family = "Arial",
                                   cs.family = NULL,
                                   eastasia.family = NULL,
                                   hansi.family = NULL,
                                   vertical.align = "baseline",
                                   shading.color = "transparent"
                                 ))
# temp <- docx_set_paragraph_style(temp, 
#                                  style_id = "TableCaption", 
#                                  style_name = "Table Caption", 
#                                  base_on = "Normal", 
#                                  fp_p = fp_par(
#                                    text.align = "left",
#                                    # padding = 0,
#                                    line_spacing = 2,
#                                    border = fp_border(width = 0),
#                                    padding.bottom = 0,
#                                    padding.top = 0,
#                                    padding.left = 0,
#                                    padding.right = 0,
#                                    # border.bottom,
#                                    # border.left,
#                                    # border.top,
#                                    # border.right,
#                                    shading.color = "transparent",
#                                    keep_with_next = FALSE,
#                                    word_style = "Normal"
#                                  ), 
#                                  fp_t = fp_text(
#                                    color = "black",
#                                    font.size = 11,
#                                    bold = FALSE,
#                                    italic = FALSE,
#                                    underlined = FALSE,
#                                    font.family = "Arial",
#                                    cs.family = NULL,
#                                    eastasia.family = NULL,
#                                    hansi.family = NULL,
#                                    vertical.align = "baseline",
#                                    shading.color = "transparent"
#                                  ))

temp <- body_set_default_section(temp, 
                                 prop_section(page_size = do.call("page_size", 
                                                                  args = the.page.sizes), 
                                              page_margins = do.call("page_mar", 
                                                                     args = the.page.margins)))


print(temp, 
      target = paste0("./03-Output/01-Tables/summary_table_", 
                      names(sft.list)[i], 
                      ".docx"))



# save_as_docx(values = list(sft.list[[i]]) %>% setNames(., nm = caps[i]), 
#              # sft.list[[i]], 
#              path = paste0("./03-Output/01-Tables/summary_table_", 
#                            names(sft.list)[i], 
#                            ".docx"), 
#              pr_section = prop_section(page_size = do.call("page_size", 
#                                                            args = the.page.sizes), 
#                                        page_margins = do.call("page_mar", 
#                                                               args = the.page.margins)))






## save



## clean environment
rm(list=ls())
gc()




