#!/usr/bin/env lua

local io        = require ("io")
local argv      = arg
local argc      = table.maxn (argv)

assert (argc == 1)
local spec_file = io.open (argv[1])
assert (spec_file)

--
-- Print pair
--

local function print_pair (package, orig_name, ada_name)
  assert (type (package)   == "string")
  assert (type (orig_name) == "string")
  assert (type (ada_name)  == "string")

  local item = package.."."..ada_name

  local mod_item = item:lower ()
  mod_item = mod_item:gsub ("%.", "_")

  io.write ([[(t-row (item "]]..orig_name..[[")]])
  io.write ([[(item (link "]]..mod_item..[[" "]]..item..[[")))
]])
end

--
-- Read package name
--

local package = ""

while true do
  local spec_line = spec_file:read ("*l")

  if not spec_line then error ("unexpected EOF before package name") end
  spec_line = spec_line:gsub ("^[%s]*", "")

  local pkg_match = spec_line:match ("^package ([a-zA-Z0-9%._]+) is")
  if pkg_match then
    package = pkg_match
    break
  end
end

--
-- Read spec
--

orig_name      = ""
expecting_name = false

while true do
  local spec_line = spec_file:read ("*l")

  if not spec_line then break end
  spec_line = spec_line:gsub ("^[%s]*", "")

  if not expecting_name then
    local spec_match = spec_line:match ("^-- proc_map : ([a-zA-Z0-9_]+)$")
    if spec_match then
      expecting_name   = true
      orig_name        = spec_match
    end
  else
    local spec_match = spec_line:match ("^procedure ([a-zA-Z0-9_]+)")
    if spec_match then
      print_pair (package, orig_name, spec_match)
      expecting_name = false
    end
    local spec_match = spec_line:match ("^function ([a-zA-Z0-9_]+)")
    if spec_match then
      print_pair (package, orig_name, spec_match)
      expecting_name = false
    end
  end
end
