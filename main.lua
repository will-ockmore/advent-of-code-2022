local fennel = require("lib.fennel")
table.insert(package.loaders or package.searchers, fennel.make_searcher({correlate=true}))
debug.traceback = fennel.traceback

local mylib = require("wrap") 
