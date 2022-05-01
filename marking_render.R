# Render_questions --------------------------------------------------------

#' Render a slider widget
#'
#' This function renders a slider widget.
#'
#' @param id Character. Valid JS id for the slider.
#' @param min_limit Numeric. Min.
#' @param max_limit Numeric. Max.
#' @param default Numeric. Default value.
#' @return The id of the slider.
render_slider_widget <- function(id, min_limit, max_limit, default = 0) {
  cat(glue("<input type='range' value='{min_limit*2}' max='{max_limit*2}' oninput='{id}.value = this.value/2'>\n\n"))
  cat(glue("<output id='{id}'>{default}</output>\n\n"))
  cat("<br>\n\n")

  return(id)
}


#' Render a checklist widget
#'
#' This function renders a checklist widget.
#'
#' @param id Character. Valid JS id for the checklist.
#' @param checklist List. A checklist.
#' @param default Numeric. Default value of the suggested marks.
#' @return The id of the text area.
render_checklist_widget <- function(id, checklist, default) {
  cat(glue("<p>Suggested marks: <output id='{id}'>{default}</output></p>\n\n"))
  for (i in 1:length(checklist)) {
    cat(glue("<input type='checkbox' id='{id}_{i}' onclick='handle_checklist(\"{id}\", \"{id}_{i}\", \"{id}_text_area\",  Number(({id}_{i}.checked*2-1)*{checklist[[i]]$mark}), \"{checklist[[i]]$feedback}\")'>\n\n"))
    cat(glue("<label for='{id}_{i}'>{checklist[[i]]$desc} ({checklist[[i]]$mark})</label>\n\n"))
    cat("<br>\n\n")
  }
  cat("<br>\n\n")
  cat(glue("<textarea type='text' style='width:500px;height:100px' placeholder='comment' id='{id}_text_area'></textarea>\n\n"))

  return(glue("{id}_text_area"))
}


#' Render a slider
#'
#' This function renders a slider.
#'
#' @param slider List. A slider.
#' @param prefix Character. Prefix of the JS id.
#' @return The id of the slider widget and the text area.
render_slider <- function(slider, prefix = "a") {
  cat(glue("<p>{slider$name}</p>\n\n"))
  slider_id <- render_slider_widget(glue("{prefix}_{slider$name}_slider"), 0, slider$mark)
  text_area_id <- render_checklist_widget(glue("{prefix}_{slider$name}_checklist"), slider$checklist, 0)

  return(c(slider_id, text_area_id))
}


#' Render a subquestion
#'
#' This function renders a subquestion.
#'
#' @param sub_question List. A subquestion.
#' @param prefix Character. Prefix of the JS id.
#' @return A list of ids of the sliders and text areas of the subquestion.
render_subquestion <- function(sub_question, prefix = "a") {
  cat(glue("<h2>{sub_question$name} ({sub_question$mark})</h2>\n\n"))
  sub_question_ids <- list(slider_id = c(), text_area_id = c())

  if (length(sub_question$sliders) == 0) stop("Need to provide at least one slider for a subquestion.")

  for (slider in sub_question$sliders) {
    id_list <- render_slider(slider, prefix = glue("{prefix}_{sub_question$name}"))
    sub_question_ids$slider_id <- c(sub_question_ids$slider_id, id_list[1])
    sub_question_ids$text_area_id <- c(sub_question_ids$text_area_id, id_list[2])
  }

  return(sub_question_ids)
}


#' Render the handle_checklist JS function
#'
#' This function renders the handle_checklist JS function
#'
#' When the checkbox being clicked, the suggested marks and the text areas need to be updated.
#' @return No return.
render_handle_checklist <- function() {
  cat("<script>\n\n")
  cat("function handle_checklist(value_id, checkbox_id, text_area_id, add_value, add_text) {
  document.getElementById(value_id).value = Number(document.getElementById(value_id).value) + Number(add_value);
  if (add_text == '') {return 0;}
  add_text = add_text + ' ';
  document.getElementById(text_area_id).value = document.getElementById(text_area_id).value.replace(add_text, '');
      if (document.getElementById(checkbox_id).checked) {
        document.getElementById(text_area_id).value = document.getElementById(text_area_id).value + add_text
      }
  }\n\n")
  cat("</script>\n\n")
}


#' Render a question
#'
#' This function renders a question
#'
#' @param question List. A question.
#' @param align Character. Alignment method of this question.
#' @return A list contains all sliders and text areas ids of this question.
render_question <- function(question, align = "center") {
  cat(glue("<div align='{align}'>\n\n"))
  cat(glue("<h1>{question$name}</h1>\n\n"))

  question_id <- list()

  for (sub_question in question$sub_questions) {
    question_id[[sub_question$name]] <- render_subquestion(sub_question, prefix = question$name)
  }
  cat(glue("</div>\n\n"))

  return(question_id)
}


#' Render a submit button
#'
#' This function renders a submit button
#'
#' @param name Character. Button content.
#' @param align Character. Alignment method of the button and the text area.
#' @return No return.
render_submit_button <- function(name = "Get comments!", align = "center") {
  cat(glue("<div align='{align}'>\n\n"))
  cat("<h1>Results</h1>\n\n")
  cat(glue("<button type='button' onclick='handle_button()'>{name}</button>\n\n"))
  cat("<br>\n\n")
  cat("<br>\n\n")
  cat("<textarea id='final_comments' style='width:500px;height:300px;'></textarea>")
  cat("</div>")
}


#' Render the handle_button JS function
#'
#' This function renders the handle_button JS function
#'
#' When the button being clicked, the JS function will collect all the inputs then generate the comments. The ids are
#' hard coded in the HTML file via this R function.
#'
#' @param all_question_id List. A list contains all the slider ids and text area ids in all questions.
#' @param question_list List. A list of all questions.
#' @return No return.
render_handle_button <- function(all_question_id, question_list) {
  cat("<script>\n\n")
  cat("function get_value(id) {return document.getElementById(id).value}\n\n")
  cat("function handle_button() {\n\n")

  cat("    final_str = '';\n\n")

  for (i in 1:length(all_question_id)) {

    cat(glue("    // {question_list[[i]]$name}\n\n", .trim = FALSE))

    cat("    question_total = 0;\n\n")

    for (j in 1:length(all_question_id[[i]])) {

      cat(glue("    // {question_list[[i]]$sub_questions[[j]]$name}\n\n", .trim = FALSE))

      var_name <- glue("{names(all_question_id)[i]}{names(all_question_id[[i]])[j]}")
      var_name_ <- glue("{names(all_question_id)[i]}_{names(all_question_id[[i]])[j]}")

      cat(glue("    {var_name_}_sliders = [\"{paste0(all_question_id[[i]][[j]]$slider_id, collapse = '\", \"')}\"];\n", .trim = FALSE))
      cat(glue("    {var_name_}_text_areas = [\"{paste0(all_question_id[[i]][[j]]$text_area_id, collapse = '\", \"')}\"];\n\n", .trim = FALSE))

      cat("    total = 0;\n\n")
      cat("    all_text = '';\n\n")
      cat(glue("    for (id in {{var_name_}}_sliders) {total = Number(total) + Number(get_value({{var_name_}}_sliders[id]))}\n\n", .trim = FALSE, .open = "{{", .close = "}}"))
      cat(glue("    for (id in {{var_name_}}_text_areas) {all_text = all_text + get_value({{var_name_}}_text_areas[id])}\n\n", .trim = FALSE, .open = "{{", .close = "}}"))
      cat("    if (all_text == '') all_text = ['Excellent!', 'Great work!', 'Well done!', 'Good!', 'Good job!', 'Nice!', 'Nice work!'][Math.floor(Math.random() * 7)];\n\n")
      cat(glue("    final_str = final_str + `{{var_name}}: ${total}/{{question_list[[i]]$sub_questions[[j]]$mark}} ${all_text} \\n`\n\n", .open = "{{", .close = "}}", .trim = FALSE))
      cat("    question_total = question_total + total;\n\n")
    }

    cat(glue("    final_str = final_str + `{{names(all_question_id)[i]}} total: ${question_total}/{{question_list[[i]]$mark}}\\n\\n`\n\n", .open = "{{", .close = "}}", .trim = FALSE))
  }

  cat("    document.getElementById('final_comments').value = final_str;")

  cat("}\n\n")
  cat("</script>\n\n")
}


#' Render all the questions
#'
#' This function renders all the questions. Notice that all the rendered HTML code are outputted to the standard output via
#' `cat()`. So you can use `capture.output()` to redirect the output, or set `results = 'asis'` and knit it to an HTML file.
#'
#' @param ... Questions.
#' @return No return.
render_questions <- function(...) {
  question_list <- list(...)
  all_question_id <- list()

  for (question in question_list) {
    all_question_id[[question$name]] <- render_question(question)
  }
  render_handle_checklist()
  render_submit_button()
  render_handle_button(all_question_id, question_list)
}
