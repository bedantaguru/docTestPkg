




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



.rs.addFunction("getCompletionsDoubleBracket",
                patch_function(.rs.getCompletionsDoubleBracket, ".rs.getNames",
                               if(length(completions)>0) browser(),
                               chop_locator_to = 1)
)


