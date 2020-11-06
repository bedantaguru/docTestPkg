




# sample auto completion for future::plan

.rs.addJsonRpcHandler("get_completions",
                      patch_function(.rs.rpc.get_completions, ".rs.getCompletionsEnvironmentVariables",
                                     # addition portion
                                     if (length(string) &&
                                         ("future" %in% .packages()) &&
                                         string[[1]] == "plan" &&
                                         numCommas[[1]] == 0){

                                       candidates <- c("sequential",
                                                       "transparent",
                                                       "multisession",
                                                       "multicore",
                                                       "multiprocess",
                                                       "cluster",
                                                       "remote")
                                       results <- .rs.selectFuzzyMatches(candidates, token)

                                       return(.rs.makeCompletions(token = token,
                                                                  results = results,
                                                                  quote = FALSE,
                                                                  type = .rs.acCompletionTypes$VECTOR))

                                     },
                                     chop_locator_to = 1)
)


# other usage
#  adding browser in case filter is triggered
# .rs.addFunction("getCompletionsFunction",
#                 patch_function(.rs.getCompletionsFunction, ".rs.resolveObjectFromFunctionCall",
#                                if(identical(object, get("filter", envir = asNamespace("dplyr")))){
#                                  browser()
#                                },
#                                chop_locator_to = 1)
# )


# .rs.addFunction("getCompletionsFunction",
#                 patch_function(.rs.getCompletionsFunction, ".rs.getAnywhere",
#                                browser(),
#                                chop_locator_to = 5)
# )
#



# .rs.addFunction("getCompletionsDoubleBracket",
#                 patch_function(.rs.getCompletionsDoubleBracket, ".rs.getNames",
#                                if(length(completions)>0) browser(),
#                                chop_locator_to = 1)
# )

#
# .rs.addFunction("getRChainCompletions",
#                 patch_function(.rs.getRChainCompletions, ".rs.getNames",
#                                if(length(objectNames )>0) browser(),
#                                chop_locator_to = 5)
# )


# attempt filter
.rs.addJsonRpcHandler("get_completions",
                      patch_function(.rs.rpc.get_completions, ".rs.getCompletionsEnvironmentVariables",

                                     # addition portion
                                     if (length(string) &&
                                         ("dplyr" %in% .packages())  &&
                                         string[[1]] == "filter" &&
                                         any(grepl("==", line))){

                                       chainObject <- .rs.getAnywhere(chainObjectName, envir)

                                       if(!is.null(chainObject)){

                                         if(is.data.frame(chainObject)){
                                           pline <- gsub(" +","",line)
                                           cname_attempt <- rev(unlist(strsplit(rev(unlist(strsplit(pline, "==")))[1], "\\(|[ ]+|,")))[[1]]
                                           if(cname_attempt %in% colnames(chainObject)){
                                             # safe limit
                                             safe_lim <- getOption("filter_auto_complete_row_limit")
                                             safe_lim <- ifelse(is.null(safe_lim), 10^6, safe_lim)
                                             if(nrow(chainObject)<safe_lim){
                                               this_col <- chainObject[[cname_attempt]]
                                               if(is.character(this_col) |is.factor(this_col)){
                                                 if(is.factor(this_col)){
                                                   choices <- levels(this_col)
                                                 }else{
                                                   choices <- unique(this_col)
                                                 }

                                                 # safe limit
                                                 if(length(choices)<safe_lim/2){
                                                   results <- .rs.selectFuzzyMatches(choices, token)

                                                   # show only first 200 (max)
                                                   results <- results[seq(min(200, length(results)))]

                                                   return(.rs.makeCompletions(token = token,
                                                                              results = results,
                                                                              quote = TRUE,
                                                                              type = .rs.acCompletionTypes$STRING))
                                                 }
                                               }
                                             }
                                           }
                                         }
                                       }

                                     },
                                     chop_locator_to = 1)
)

# reset
.rs.addJsonRpcHandler("get_completions",
                      patch_function(.rs.rpc.get_completions))

.rs.addFunction("getCompletionsArgument",
                patch_function(.rs.getCompletionsArgument))

.rs.addFunction("getCompletionsArgument",
                patch_function(.rs.getCompletionsArgument, '"knitr"',
                               {
                                 # one has to add {} in such cases

                                 list_match_args <- function(fun){
                                   out <- list()
                                   if(is.function(fun)){
                                     fbody <- body(fun)
                                     fbodyl <- as.character(fbody)
                                     fargs <- formals(fun)
                                     chk_strs1 <- paste0("arg_match[[:space:]]*\\([[:space:]]*",names(fargs),"[[:space:]]*\\)")
                                     chk_strs2 <- paste0("arg_match0[[:space:]]*\\([[:space:]]*",names(fargs),"[[:space:]]*\\)")
                                     chk_strs3 <- paste0("match.arg[[:space:]]*\\([[:space:]]*",names(fargs),"[[:space:]]*\\)")
                                     is_marg <- logical(length(chk_strs1))
                                     for(i in seq_along(chk_strs1)){
                                       is_marg[i] <-  any(grepl(chk_strs1[i], fbodyl)) |
                                         any(grepl(chk_strs2[i], fbodyl)) |
                                         any(grepl(chk_strs3[i], fbodyl))
                                     }
                                     if(any(is_marg)){
                                       out <- fargs[is_marg]
                                     }
                                   }
                                   out

                                 }

                                 fun <- .rs.getAnywhere(as.character(functionCall)[1], envir)
                                 m_args <- list_match_args(fun)

                                 if(activeArg %in% names(m_args)){
                                   choices <- eval(m_args[[activeArg]])
                                   results <- .rs.selectFuzzyMatches(choices, token)

                                   return(.rs.makeCompletions(token = token,
                                                              results = results,
                                                              quote = TRUE,
                                                              type = .rs.acCompletionTypes$STRING))
                                 }
                               },
                               chop_locator_to = 1))




# arg match case completion


#
# Check this
#
# f1<- function(zi=c("hi","hello"), yi =c("g","t"), phi = c("cos", "sin"), fo= c(1,5), x){
#   yi <- match.arg(yi)
#   zi <- rlang::arg_match(zi)
#   paste0(zi, yi, phi, fo)
# }
#
#
#

