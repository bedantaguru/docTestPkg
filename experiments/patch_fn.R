
# ftest <- function(x, y){
#   u <- x+y
#   v <- u^2
#   z <- v+x
#   if(x%%2==0){
#     z0 <- z+1
#   }else{
#     if(y%%2 == 0){
#       v <- 11
#       z1 <- 14
#       u <- 10
#     }else{
#       z1 <- 10
#     }
#     z0 <- z^2+z1
#   }
#
#   if(z0%%5 == 1){
#     z2 <- z0*2
#   }else{
#     z2 <- z0*3
#   }
#
#   z2
#
# }

locate_section <- function(fbody, search_str){
  fbody_n <- fbody
  tloc <- integer(0)
  repeat{

    fbc <- as.character(fbody_n)
    si <- grepl(search_str, fbc)
    if(any(si)){
      tloc <- c(tloc, which(si))
      fbody_n <- fbody_n[[which(si)]]
      if(length(as.list(fbody_n))==1) break()
    }else{
      break()
    }
  }
  tloc

}


get_section <- function(fbody, tloc){
  obj <- NULL
  if(length(tloc)>0){
    obj <- eval(parse(text = paste0("fbody",paste0("[[",tloc,"]]",collapse = ""))))
  }
  obj
}

replace_in_section <- function(fbody, tloc, expr, env = NULL){
  fbody_mod <- fbody
  if(length(tloc)>0){
    if(is.null(env)){
      env_str <- ")"
    }else{
      env_str <- ", env = env)"
    }
    eval(parse(text = paste0(
      paste0("fbody_mod",paste0("[[",tloc,"]]",collapse = ""))," <- ", "substitute(expr", env_str
    )))

  }
  fbody_mod
}

append_in_section <- function(fbody, tloc, expr, env = NULL, after = TRUE){
  fbody_mod <- fbody

  if(length(tloc)>0){
    if(is.null(env)){
      env_str <- ")"
    }else{
      env_str <- ", env = env)"
    }

    if(length(tloc)>1){
      taget_brackets <- paste0("[[",tloc[-length(tloc)],"]]",collapse = "")
    }else{
      # length(tloc)==1 case
      taget_brackets <- ""
    }

    eval(
      parse(
        text = paste0(
          "fbm_part <- ", paste0("fbody_mod",taget_brackets),"\n",
          "fbm_part <- fbm_part[c(",paste0(c(1:tloc[length(tloc)], tloc[length(tloc)]), collapse = ","),":length(fbm_part))]", "\n",
          paste0("fbm_part[[",ifelse(after, tloc[length(tloc)]+1, tloc[length(tloc)]),"]]")," <- ", "substitute(expr", env_str,"\n",
          "fbm_part -> ", paste0("fbody_mod",taget_brackets)
        )
      )
    )

  }
  fbody_mod
}

# env to store functions
pre_patch_function_store <- new.env()

store_original_function<- function(f, env = NULL){
  if(!is.null(env)){
    fn <- as.character(substitute(f, env))
  }else{
    fn <- as.character(substitute(f))
  }
  if(is.null(pre_patch_function_store[[fn]])){
    assign(fn, f, envir = pre_patch_function_store)
  }else{
    return(get(fn, envir = pre_patch_function_store))
  }
  return(f)
}

patch_function <- function(f, search_str, expr, replace_it = FALSE, append_after = TRUE, chop_locator_to = NULL, env = NULL){


  if(is.null(env)){
    env <- environment()
  }

  fs <- store_original_function(f, env = env)
  if(missing(search_str)){
    return(fs)
  }


  fbody <- body(f)
  # match first occurrence
  if(length(search_str)>1){
    for(ss in search_str){
      tloc <- locate_section(fbody, ss)
      if(length(tloc)>0) break()
    }
  }else{
    tloc <- locate_section(fbody, search_str)
  }

  if(length(chop_locator_to)>0){
    chop_locator_to <- min(chop_locator_to, length(tloc))
    tloc <- tloc[seq(chop_locator_to)]
  }

  if(replace_it){
    fbody_new <- replace_in_section(fbody, tloc, expr, env = env)
  }else{
    fbody_new <- append_in_section(fbody, tloc, expr, env = env, after = append_after)
  }


  body(f) <- fbody_new
  f
}

# patch_function(ftest, "z1 <- 14", z1 <- z1 + length(letters))
# patch_function(ftest, c("v <- 11","z1 <- 14","u <- 10"), z1 <- z1 + length(letters))

