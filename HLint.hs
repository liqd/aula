import "hlint" HLint.Default
import "hlint" HLint.Dollar
import "hlint" HLint.Generalise
import "hlint" HLint.HLint

ignore "Avoid lambda" = Action.mkRunAction

ignore "Redundant do"
ignore "Use const"
ignore "Use fmap"
ignore "Use list literal"
ignore "Use record patterns"
