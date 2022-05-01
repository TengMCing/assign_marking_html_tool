
# Define question structure -----------------------------------------------

#' Define a new question
#'
#' This function defines a new question.
#'
#' @param name Character. Name of the question.
#' @param mark Numeric. Total marks of this question
#' @return A list.
new_question <- function(name, mark) {
  return(list(name = name, mark = mark, sub_questions = list()))
}

#' Define a new subquestion
#'
#' This function defines a new subquestion.
#'
#' @param question List. The question this subquestion belongs to.
#' @param name Character. Name of the subquestion.
#' @param mark Numeric. Total marks of this subquestion
#' @return The question list.
new_subquestion <- function(question, name, mark) {
  question$sub_questions[[name]] <- list(name = name, mark = mark, sliders = list())

  return(question)
}

#' Add a slider for a subquestion
#'
#' This function add a slider for a subquestion. This slider can be used to
#' give marks to parts of the subquestion.
#'
#' @param question List. The question this slider belongs to.
#' @param name Character. Name of the slider.
#' @param mark Numeric. Maximum mark of this slider.
#' @param checklist List. A checklist.
#' @param sub_question Character/Integer. the subquestion this slider belongs to. "last" means the last subquestion.
#' @return The question list.
add_slider <- function(question, name, mark, checklist, sub_question = "last") {

  if (sub_question == "last") sub_question <- length(question$sub_questions)

  question$sub_questions[[sub_question]]$sliders[[name]] <- list(name = name,
                                                                 mark = mark,
                                                                 checklist = checklist)
  return(question)
}

#' Define a new checklist
#'
#' This function defines a new checklist.
#'
#' A checklist is a list of items that marker would like to check. An item needs to contain three elements: description,
#' feedback, and mark. Description is something like "Equation is provided.", and the corresponding feedback would be
#' "Equation needs to be provided.". Each mark is either positive or negative, which means you could define both mark addition
#' and mark deduction. The inputs of this function needs to be specified in such a way:
#' `{description1} = {feedback1}, {description2} = {feedback2}, marks = c(mark1, mark2)`.
#'
#' @param ... Items. See above for more details.
#' @param marks Numeric. Marks for each item.
#' @return A checklist.
new_checklist <- function(..., marks = NULL) {
  call_list <- as.list(sys.call())[-1]
  call_list[["marks"]] <- NULL
  if (is.null(names(call_list)) || "" %in% names(call_list)) stop("All items in checklist need to be provided with descriptions and feedbacks.")
  if (is.null(marks) || length(marks) != length(call_list)) stop("All items in checklist need to be provided with suggested marks.")
  for (i in 1:length(call_list)) {
    call_list[[i]] <- list(desc = names(call_list)[i], feedback = call_list[[i]], mark = marks[i])
  }
  return(call_list)
}
