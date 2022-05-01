library(glue)
`%>%` <- magrittr::`%>%`

source(here::here("marking_define.R"))
source(here::here("marking_render.R"))


# We first define a new question - Q1. Total mark is 4.
q1 <- new_question("Q1", 4) %>%

  # Define a subquestion under Q1. Total mark is 2.
  new_subquestion("a", 2) %>%

  # Add a slider for "Plot" under Q1a. Total mark is 1, along with a checklist.
  #
  # The checklist will give you a suggested mark, but
  # it will not affect the final mark calculation. So you still need to choose a
  # value using the slider.
  #
  # The checklist is defined with the syntax `{description} = {feedback}, marks = c(mark1, mark2, ...)`. Description
  # is the text you see. You need to check or not check it. If the checkbox is checked
  # Feedback will be automatically generated in the comment section. `marks` is the corresponding
  # mark for each item in the list. It can be either positive or negative values. If it is
  # positive, that means it is doing mark addition. If it is negative, that means it is doing
  # mark deduction. This mark only affects the suggested mark, not the final mark.
  add_slider("Plot", 1, new_checklist("Plot is basically correct" = "",
                                      "x axis doesn't look right" = "You need to fix x axis.",
                                      "y axis doesn't look right" = "You need to fix y axis.",
                                      marks = c(1, -0.5, -0.5))) %>%

  # Add another slider for the "Answer" under Q1a. Total mark is 1.
  add_slider("Answer", 1, new_checklist("Answer is basically correct"= "",
                                        "x doesn't look right" = "You need to fix x.",
                                        "y doesn't look right" = "You need to fix y.",
                                        marks = c(1, -0.5, -0.5))) %>%

  # Add a new subquestion under Q1. Total mark is 2.
  new_subquestion("b", 2) %>%

  # Add a slider for the "Answer" under Q1b. Total mark is 2. This checklist is only doing mark addition, so markers
  # are not supposed to check multiple checkbox, otherwise the suggested mark may greater than 2.
  add_slider("Answer", 2, new_checklist("Answer is Great" = "",
                                        "Answer is OK" = "Have a look at the solution.",
                                        "Insufficient details" = "You need to provide more details in this question",
                                        marks = c(2, 1, 0.5)))

# Define a new question Q2. Total mark is 2.
q2 <- new_question("Q2", 2) %>%

  # Define a new subquestion under Q2. Total mark is 2.
  new_subquestion("a", 2) %>%

  # Similar to how we define sliders in Q1.
  add_slider("Plot", 1, new_checklist("Plot is basically correct" = "",
                                      "x axis doesn't look right" = "You need to fix x axis.",
                                      "y axis doesn't look right" = "You need to fix y axis.",
                                      marks = c(1, -0.5, -0.5))) %>%
  add_slider("Answer", 1, new_checklist("Answer is basically correct"= "",
                                        "x doesn't look right" = "You need to fix x.",
                                        "y doesn't look right" = "You need to fix y.",
                                        marks = c(1, -0.5, -0.5)))

# The output of `q1` is a list, we need to convert this list to some HTML code.
# q1

# Render the HTML code
# Because `render_questions` will use `cat()` to print out the HTML code, we need to capture the output and redirect it
# to an HTML file.
render_questions(q1, q2) %>%
  capture.output(file = "marking.html")
