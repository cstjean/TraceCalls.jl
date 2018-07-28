## Code for removing warnings. Copied from ClobberingReload.jl (which is deprecated and
## dying)

# O nameless stranger, please improve my ailing regexes.
redefinition_regexes =
    [r"WARNING: Method definition .* in module .* at .* overwritten at .*\n",
     r"WARNING: Method definition .* in module .* overwritten.\n",
     r"WARNING: replacing docs for .*\n",
     # 0.6 updated its doc warnings with color. Looks like this:
     # \e[1m\e[33mWARNING: \e[39m\e[22m\e[33mreplacing
     r".*WARNING: .*replacing docs for .*\n",
     r"WARNING: redefining constant .*\n"]


""" `scrub_stderr(body::Function, pats::Regex...)` executes `body` without
outputting any warning that matches one of the `pats`.

Pattern example: r"WARNING: redefining constant .*\n" """
function scrub_stderr(body::Function, pats::Regex...)
    mktemp() do _, f
        old_stderr = STDERR
        redirect_stderr(f)
        try
            res = body()
            flush(f)
            seekstart(f)
            text = readstring(f)
            for pat in pats
                text = replace(text, pat, "")
            end
            write(old_stderr, text)
            res
        finally
            redirect_stderr(old_stderr)
        end
    end
end


""" `scrub_redefinition_warnings(body::Function)` executes `body` without
outputting any redefinition warnings """
function scrub_redefinition_warnings(body::Function)
    scrub_stderr(redefinition_regexes...) do
        body()
    end
end


""" `no_warnings(body::Function)` executes `body` without outputting any
warnings (no STDERR output) """
function no_warnings(body::Function)
    scrub_stderr(r".*\n") do
        body()
    end
end


""" `sinclude(filename)` calls `include(filename)`, but doesn't show any 
redefinition warnings (it's a _silent_ `include`) """
function sinclude(filename)
    scrub_redefinition_warnings() do
        include(filename)
    end
end



module_string(mod::String) = mod
module_string(mod::Module) = string(module_name(mod))

# I believe there's a better, modern way of doing this in 0.6
module_definition_file_(mod) = Base.find_in_node_path(module_string(mod), nothing, 1)
function module_definition_file(mod)
    path = module_definition_file_(mod)
    if path === nothing
        error("Cannot find path of module $mod. To be usable by `TraceCalls`, the module has to be defined in a file called $mod.jl, and that file's directory must be pushed onto LOAD_PATH. See the Julia docs on `using`.")
    end
    path
end
