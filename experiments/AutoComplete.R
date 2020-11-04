




# sample auto completion for future::plan
.rs.addFunction("getCompletionsFuturePlans", function(token)
{
  candidates <- c("sequential",
                  "transparent",
                  "multisession",
                  "multicore",
                  "multiprocess",
                  "cluster",
                  "remote")
  results <- .rs.selectFuzzyMatches(candidates, token)

  .rs.makeCompletions(token = token,
                      results = results,
                      quote = FALSE,
                      type = .rs.acCompletionTypes$VECTOR)
})

.rs.addJsonRpcHandler("get_completions",
                      patch_function(.rs.rpc.get_completions, ".rs.getCompletionsEnvironmentVariables",
                                     # addition portion
                                     if (length(string) &&
                                         "future" %in% loadedNamespaces() &&
                                         string[[1]] == "plan" &&
                                         numCommas[[1]] == 0)
                                       return(.rs.getCompletionsFuturePlans(token)),
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


.rs.addFunction("getRChainCompletions",
                patch_function(.rs.getRChainCompletions, ".rs.getNames",
                               if(length(objectNames )>0) browser(),
                               chop_locator_to = 5)
)


# attempt filter
.rs.addJsonRpcHandler("get_completions",
                      patch_function(.rs.rpc.get_completions, ".rs.getCompletionsEnvironmentVariables",
                                     # addition portion
                                     if (length(string) &&
                                         string[[1]] == "filter" &&
                                         any(grepl("==", line))){

                                       if(exists(chainObjectName, envir = envir)){
                                         chainObject <- envir[[chainObjectName]]
                                         if(is.data.frame(chainObject)){
                                           pline <- gsub(" +","",line)
                                           cname_attempt <- rev(unlist(strsplit(rev(unlist(strsplit(pline, "==")))[1], "\\(|[ ]+|,")))[[1]]
                                           if(cname_attempt %in% colnames(chainObject)){
                                             # safe limit
                                             if(nrow(chainObject)<10^5){
                                               this_col <- chainObject[[cname_attempt]]
                                               if(is.factor(this_col)){
                                                 choices <- levels(this_col)
                                               }else{
                                                 choices <- unique(this_col)
                                               }

                                               # safe limit
                                               if(length(choices)<50){
                                                 results <- .rs.selectFuzzyMatches(choices, token)

                                                 return(.rs.makeCompletions(token = token,
                                                                     results = results,
                                                                     quote = TRUE,
                                                                     type = .rs.acCompletionTypes$STRING))
                                               }
                                             }
                                           }
                                         }
                                       }

                                     },
                                     chop_locator_to = 1)
)
