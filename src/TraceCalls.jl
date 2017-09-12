__precompile__()
module TraceCalls

using Requires
using MacroTools
#using MacroTools: combinedef, combinearg, longdef1
using ClobberingReload: combinedef, combinearg, longdef1, splitdef, splitarg
using Base.Test: @inferred
using DataStructures: OrderedDict, OrderedSet
using Memoize
using Base: url
using Crayons: Crayon, inv
import Base: +, -

export @traceable, @trace, Trace, prune, FontColor, Bold,
    is_inferred, map_is_inferred, redgreen, greenred, @trace_inferred,
    compare_past_trace, filter_func, apply_macro, @stacktrace, measure, tree_size,
    is_mutating, REPR, filter_cutting, NoTraceable, trace_log, filter_lineage,
    bottom, top, highlight, @show_val_only_type, objects_in, signature, groupby,
    map_groups, trace_benchmark, @compilation_times

include("code_update.jl")

const traceable_definitions = OrderedDict()  # We use an OrderedDict because in case we
                                             # accidentally store the same definition
                                             # twice, at least the latter one takes
                                             # precedence.

################################################################################
# Utils

macro ignore_errors(error_rval, expr)
    quote
        try
            $(esc(expr))
        catch e
            $(esc(error_rval))
        end
    end
end

macro return_error(expr)
    quote
        try
            $(esc(expr))
        catch e
            return e
        end
    end
end

return_error_fn(f) = x->@return_error(f(x))

################################################################################

""" A `Trace` object represents a function call. It has fields `func, args, kwargs,
called, value`, with `func(args...; kwargs...) = value`. `called::Vector{Trace}` of the
immediate traceable function calls that happened during the execution of this trace.

There are no accessors for `Trace`; please reference its fields directly.
For instance, `filter(tr->isa(tr.args[1], Int), trace)` will select all calls whose
first argument is an `Int`. """
struct Trace
    func             # the function/callable called
    args::Tuple      # the positional arguments
    kwargs::Tuple    # the keyword arguments
    called::Vector{Trace}  
    value       # This is the return value of the func(args...) call, but it's also where
                # the result of `map(f, ::Trace)` will be stored.
end
# Convenience constructors
Trace(tr::Trace, new_value) = Trace(tr.func, tr.args, tr.kwargs, tr.called, new_value)
Trace(tr::Trace, called::Vector{Trace}) =
    Trace(tr.func, tr.args, tr.kwargs, called, tr.value)

function Base.copy!(dest::Trace, src::Trace)
    dest.func = src.func
    dest.args = src.args
    dest.kwargs = src.kwargs
    dest.called = src.called
    dest.value = src.value
end

tree_size(tr::Trace) = 1 + mapreduce(tree_size, +, 0, tr.called)
Base.copy(tr::Trace) = Trace(tr.func, tr.args, tr.kwargs, tr.called, tr.value)
Base.push!(tr::Trace, sub_trace::Trace) = push!(tr.called, sub_trace)
Base.getindex(tr::Trace, i::Int, args...) = tr.called[i][args...]
Base.getindex(tr::Trace) = tr
Base.length(tr::Trace) = length(tr.called)
Base.endof(tr::Trace) = length(tr)
Base.url(tr::Trace) = @ignore_errors "" Base.url(which(tr))
Base.which(tr::Trace) = apply_macro(:@which, tr)
Base.code_llvm(tr::Trace) = apply_macro(:@code_llvm, tr)
Base.code_lowered(tr::Trace) = apply_macro(:@code_lowered, tr)
Base.code_native(tr::Trace) = apply_macro(:@code_native, tr)
Base.code_typed(tr::Trace) = apply_macro(:@code_typed, tr)
Base.code_warntype(tr::Trace) = apply_macro(:@code_warntype, tr)
Base.less(tr::Trace) = apply_macro(:@less, tr)
Base.edit(tr::Trace) = apply_macro(:@edit, tr)
Base.sum(f::Function, tr::Trace) = sum(f, collect(tr))
Base.sum(tr::Trace) = sum(t->t.value, tr)
Base.all(f::Function, tr::Trace) = all(f, collect(tr))
Base.all(tr::Trace) = all(t->value(t), collect(tr))

narrow_typeof{T}(t::Type{T}) = Type{T}
narrow_typeof{T}(t::T) = T
""" `signature(tr::Trace) = (tr.func, map(typeof, tr.args)...)` - the arguments that this
call dispatches on. """
signature(tr::Trace) = (tr.func, map(narrow_typeof, tr.args)...)
arg_names(method::Method) = [Symbol(first(a))
                             for a in Base.arg_decl_parts(method)[2][2:end]
                             if first(a)!=""]
arg_names(tr::Trace) = arg_names(which(tr))  # positional arguments only
function Base.get(tr::Trace, arg::Symbol, default)
    if (i = findfirst(a->a==arg, arg_names(tr))) > 0
        tr.args[i]
    elseif (k = findfirst(p->p[1]==arg, tr.kwargs)) > 0
        tr.kwargs[k][2]
    else
        default
    end
end
struct Dummy end
Base.getindex(tr::Trace, arg::Symbol) =
    (r=Base.get(tr, arg, Dummy())) == Dummy() ? throw(KeyError(arg)) : r
Base.keys(tr::Trace) = [arg_names(tr)...; map(first, tr.kwargs)...]
Base.haskey(tr::Trace, key::Symbol) = key in arg_names(tr) || any(key==first(kwa)
                                                                  for kwa in tr.kwargs)
""" `objects_in(tr::Trace)` returns the call's arguments and return value in a vector.
Useful for filtering, eg. `filter(tr->any(obj isa Number && obj < 0 for obj in objects_in(tr)), trace)` will keep all calls that contain some negative number. """
objects_in(tr::Trace) = [tr.args..., map(last, tr.kwargs)..., value(tr)]
            
# I've disabled iteration because it doesn't align with our desired `Base.map`'s
# behaviour, and it's kinda useless anyway.
# Base.start(tr::Trace) = 1
# Base.next(tr::Trace, i::Int) = (tr[i], i+1)
# Base.done(tr::Trace, i::Int) = i == length(tr)+1
(tr::Trace)() = tr.func(tr.args...; tr.kwargs...)

empty_trace() = nothing
const empty_trace_dummy = Trace(empty_trace, (), (), [], nothing)


struct Bottom
    delta::Int   # a positive number
end
""" `some_trace[bottom-2]` returns the subtrace that is the caller of the caller of the
deepest call in `some_trace`. """
const bottom = Bottom(0)
deepest_call(tr) = tr.called[findmax(map(depth, tr.called))[2]]
-(bot::Bottom, d::Int) = Bottom(bot.delta + d)
Base.getindex(tr::Trace, bot::Bottom, args...) =
    (depth(tr) <= bot.delta + 1) ? tr[args...] : deepest_call(tr)[bot, args...]
depth(tr::Trace) = isempty(tr.called) ? 1 : 1 + maximum(depth, tr.called)


struct Top
    delta::Int   # a positive number
end
""" `some_trace[top+2]` is equivalent to `some_trace[i, j]` where `i::Int` and `j::Int`
are the indexes of the deepest call trees in `some_trace` and `some_trace[i]`,
respectively. """
const top = Top(0)
+(top::Top, d::Int) = Top(top.delta + d)
Base.getindex(tr::Trace, top::Top, args...) =
    top.delta==0 ? tr : deepest_call(tr)[Top(top.delta-1), args...]


apply_macro(mac::Symbol, tr::Trace, mod::Module=Main) =
    eval(mod, Expr(:macrocall, mac,
                   (isempty(tr.kwargs) ?
                    # Needs special casing because @inferred chokes on kwargs-less
                    # funcalls otherwise.
                    :($(tr.func)($(map(QuoteNode, tr.args)...))) :
                    :($(tr.func)($(map(QuoteNode, tr.args)...); $(tr.kwargs...))))))
apply_macro(mac::Expr, tr::Trace, mod::Module=Main) =
    (mac.head==:macrocall ? apply_macro(only(mac.args), tr, mod) :
     error("Unable to call macro $mac"))

""" `map(f, tr::Trace)` recursively applies the function f to each `Trace` in `tr`,
and stores the result in `Trace.value` """
Base.map(f::Function, tr::Trace) = Trace(tr.func, tr.args, tr.kwargs,
                                         [map(f, c) for c in tr.called], f(tr))
""" `map_trace(f, tr)` is like `map`, but we expect `f(trace)` to return a new Trace """ 
map_trace(f::Function, tr::Trace) = f(Trace(tr.func, tr.args, tr.kwargs,
                                            [map_trace(f, c) for c in tr.called],
                                            tr.value))::Trace
apply_macro_fn(mac::Union{Symbol, Expr}) = sub->apply_macro(mac, sub)
Base.map(mac::Union{Symbol, Expr}, tr::Trace) = map(apply_macro_fn(mac), tr)
""" Similar to `map`, but we apply `filter_fun(f(tr))` to each call, and if it's false,
we filter out that call and all its descendents. Efficiently implemented. """
function map_filter(f::Function, filter_fun::Function, tr::Trace)
    # This was written for measure, but we turned out not to use it. Delete?
    res = f(tr)
    if filter_fun(res)
        Trace(tr.func, tr.args, tr.kwargs,
              filter(x->x!==empty_trace_dummy,
                     [map_filter(f, filter_fun, c) for c in tr.called]), res)
    else
        empty_trace_dummy
    end
end
handle_mac(f::Function) = f
handle_mac(mac::Union{Symbol, Expr}) = apply_macro_fn(mac)

const current_called =
    fill(Trace[]) # the current vector onto which to tack the next function calls

is_traceable(def) =
    (is_function_definition(def) && (di=splitdef(def); !is_call_definition(di)) &&
     !is_fancy_constructor_definition(di))

""" `@traceable_loose(expr)` is like `traceable(expr)`, but doesn't error on non-function
definitions (it's a helper for `@traceable begin ... end`) """
macro traceable_loose(expr)
    is_traceable(expr) ? esc(:($TraceCalls.@traceable $expr)) : esc(expr)
end

""" Turns `::Int=5` into `some_gensym::Int=5` """
function handle_missing_arg(arg)
    arg_name, arg_type, is_splat, default = splitarg(arg)
    combinearg(arg_name === nothing ? gensym() : arg_name, arg_type, is_splat, default)
end

""" Overload `TraceCalls.store(x)` for specific types to change how `TraceCalls` stores
its arguments. For example, `TraceCalls.store(x::Vector) = copy(x)`. See also ?REPR. """
store(x) = x

"""  Takes a function definition, and returns a traceing version of it. """
function tracing_code(fdef::Expr)::Expr
    arg_name(arg) = splitarg(arg)[1]
    is_splat(arg) = splitarg(arg)[3]
    arg_name_splat(arg) = is_splat(arg) ? Expr(:..., arg_name(arg)) : arg_name(arg)
    function typed_arg(arg)
        name, arg_type = splitarg(arg)
        if is_splat(arg) arg_type = Any end
        return :($name::$arg_type)
    end
    
    di = splitdef(fdef)
    di[:args] = map(handle_missing_arg, di[:args])
    di[:kwargs] = map(handle_missing_arg, di[:kwargs])
    fname = di[:name]

    do_body_di = copy(di)
    do_body_di[:name] = do_body = isa(fname, Expr) ? gensym() : gensym(fname)

    updated_fun_di = copy(di)
    passed_args = map(arg_name_splat, di[:args])
    passed_kwargs = [is_splat(kwa) ?
                     arg_name_splat(kwa) :
                     :(($(Expr(:quote, arg_name(kwa))), $(arg_name(kwa))))
                     for kwa in di[:kwargs]]
    @gensym prev_called trace_args trace_kwargs e res called
    updated_fun_di[:body] = quote
        local $res
        $prev_called = $TraceCalls.current_called[]
        $trace_args = map($TraceCalls.store, ($(passed_args...),))
        # We don't call store on kwargs. Does any function ever mutate a kwarg? Seems
        # unlikely.
        $trace_kwargs = ($(passed_kwargs...),)
        $TraceCalls.current_called[] = $called = $TraceCalls.Trace[]
        try
            $res = $do_body($(map(arg_name_splat, di[:args])...);
                            $(passed_kwargs...))
        catch $e
            push!($prev_called,
                  $TraceCalls.Trace($fname, $trace_args, $trace_kwargs,
                                    $called, $e))
            rethrow()
        finally
            $TraceCalls.current_called[] = $prev_called
        end
        push!($prev_called,
              $TraceCalls.Trace($fname, $trace_args, $trace_kwargs,
                                $called, $TraceCalls.store($res)))
        return $res
    end
    quote
        @inline $(combinedef(do_body_di))
        $(combinedef(updated_fun_di))
    end
end

""" `@traceable function foo(...) ... end` makes that function definition traceable
with the `@trace` macro (eg. as `@trace () foo(...)`). """
macro traceable(fdef::Expr)
    @assert !is_call_definition(fdef) "@traceable cannot handle callable object definitions"

    if fdef.head == :block
        # TODO: just process it right away... It will be faster
        return esc(quote
            $([:($TraceCalls.@traceable_loose $expr) for expr in fdef.args]...)
        end)
    end

    file = (@__FILE__) == "" ? nothing : (@__FILE__)
    mod = current_module()
    di = splitdef(fdef)
    signature = (module_name(mod),         # loose approximation of the real signature
                 :($(di[:name])($([splitarg(arg)[2] for arg in di[:args]]...))))
    
    traceable_definitions[signature] =
          RevertibleCodeUpdate(tracing_code,
                               CodeUpdate(ModDict(mod=>OrderedSet([MakeRelocatableExpr(fdef)]))))

    return esc(fdef)
end

# There might be an issue with the memo if we decide to macroexpand the code, since the
# macroexpansion depends on the environment. It's probably a negligible issue.
@memoize Dict{Tuple{Expr}, Any} function traceable_update_handle_expr(expr0::Expr)
    expr = strip_docstring(expr0)
    is_traceable(expr) ? tracing_code(expr) : nothing
end
traceable_update_handle_expr(::Any) = nothing
clear_handle_expr_memo!() =
    empty!(eval(TraceCalls, Symbol("##traceable_update_handle_expr_memoized_cache")))

""" `traceable_update(obj)::RevertibleCodeUpdate` produces the traceable code for the
given object. """
function traceable_update end

custom_when_missing(x) = warn(x)
custom_when_missing(fail::UpdateInteractiveFailure) =
    warn("Cannot trace $(fail.fn); use `@traceable` to trace methods defined interactively. See ?@traceable and the user manual for details.")
traceable_update(obj::Union{Module, String}) =
    update_code_revertible(traceable_update_handle_expr, obj)
traceable_update(f::Union{Function, Type}) =
    update_code_revertible(traceable_update_handle_expr, f, when_missing=custom_when_missing)

traceable_update(tup::Tuple) = merge(map(traceable_update, tup)...)
traceable_update(tup::Tuple{}) = EmptyRevertibleCodeUpdate()

""" The `RevertibleCodeUpdate` for the code from the `@traceable` macros. """
traceable_macro_update() = merge(EmptyRevertibleCodeUpdate(),
                                 values(traceable_definitions)...)


""" `@trace (foo, NoTraceable()) ...` will only trace `foo`; not the `@traceable`
functions. """
struct NoTraceable end
traceable_update(::NoTraceable) = EmptyRevertibleCodeUpdate()
""" `traceable_update_code(x)` returns the code of the update. For debugging. """
traceable_update_code(x) =
    map(to_expr, collect(reduce((s1,s2)->OrderedSet([collect(s1); collect(s2)]),
                                collect(values(traceable_update(x).apply.md)))))
traceable_update_revert_code(x) =
    map(to_expr, collect(reduce(merge, collect(values(traceable_update(x).revert.md)))))


function with_tracing_definitions(body::Function, obj)
    if obj isa Tuple && NoTraceable() in obj
        upd = traceable_update(obj)
    else
        upd = merge(traceable_update(obj), traceable_macro_update())
    end
    upd() do
        body()
    end
end

""" `get_error_or_value(f::Function)` runs `f()` in a try and returns either its outcome
or the caught exception. """
function get_error_or_value(f)
    try
        f()
    catch e
        e
    end
end

""" `recording_trace(fun::Function)` sets up a fresh Trace recording state, then executes
`fun` and returns the resulting Trace object. """
function recording_trace(fun::Function)
    current_called[] = called = Trace[]
    res = get_error_or_value(fun)
    top_trace = Trace(fun, (), (), called, res)
    current_called[] = Trace[] # don't hang on to that memory unnecessarily
    top_trace
end    

function tracing(body::Function, to_trace=())
    with_tracing_definitions(to_trace) do
        # To debug, just use `with_tracing_definitions()` interactively
        recording_trace(body)
    end
end

macro trace(expr)
    :($TraceCalls.tracing(()->$(esc(expr))))
end

macro trace(to_trace, expr)
    :($TraceCalls.tracing(()->$(esc(expr)), $(esc(to_trace))))
end

################################################################################

const tab_def = """<style type="text/css">
<!--
.tab { margin-left: 40px; }
-->
</style>"""

function with_crayon(f::Function, io::IO, cr::Crayon)
    print(io, cr)
    try
        f()
    finally
        print(io, inv(cr))
    end
end

""" `FontColor(:red, 15)` will display the value 15 in the color red (in HTML-capable
environments) """
struct FontColor
    color
    content
end
show_return_val(io::IO, mime, x::Exception) = try_show_val(io, mime, FontColor("red", x))
""" `TraceCalls.show_val(io::IO, mime, x)` is the HTML used by `TraceCalls` to display
each value (arguments and return values). Customize it by overloading. Defaults to
`show(io, x)`. """
show_val(io::IO, _, x) = show(io, x)
try_show_val(io::IO, m::MIME"text/html", x::T) where T =
    @ignore_errors(write(io, "<font color=orange>&lterror displaying $T instance&gt</font>"),
                  show_val(io, m, x))
try_show_val(io::IO, m::MIME"text/plain", x::T) where T =
    @ignore_errors(print(io, Crayon(foreground=:light_red),
                         "<error displaying $T instance>",
                         inv(Crayon(foreground=:light_red))),
                   show_val(io, m, x))

"""    @show_val_only_type SomeObjectType

Display objects of type SomeObjectType as <SomeObjectType object>."""
macro show_val_only_type(typ)
    typ_name = string(typ)
    quote
        TraceCalls.show_val(io::IO, ::MIME"text/html", ::$(esc(typ))) =
            print(io, "&lt", $typ_name, " object&gt")
        TraceCalls.show_val(io::IO, ::MIME"text/plain", ::$(esc(typ))) =
            print(io, "<", $typ_name, " object>")
    end
end

html_color(c) = c
html_color(c::NTuple{3, AbstractFloat}) = bytes2hex([map(to_int8, c)...])
function show_val(io::IO, mime::MIME"text/html", x::FontColor)
    write(io, """<font color=$(html_color(x.color))>""")
    try_show_val(io, mime, x.content)
    write(io, """</font>""")
end
crayon_color(c::String) = Symbol(c)
to_int8(frac::AbstractFloat) = round(UInt8, frac*255)
crayon_color(c::NTuple{3, AbstractFloat}) = map(Int ∘ to_int8, c)
crayon_color(c::NTuple{3, Integer}) = c

function show_val(io::IO, mime::MIME"text/plain", x::FontColor)
    with_crayon(io, Crayon(foreground=crayon_color(x.color))) do
        try_show_val(io, mime, x.content)
    end
end

""" When `Highlight(content, color)` is in the `.value` field, it highlights the whole
call. """
struct Highlight
    # It's a bit kludgey to implement highlighting this way; maybe there should be a
    # `Trace.highlight` field instead?
    color
    content
end
Highlight(content) = Highlight(:default, content)
show_val(io::IO, mime, h::Highlight) = show_val(io, mime, h.content)
""" `highlight(pred, tr::Trace)` highlights every part of the trace for which
`pred(tr)` is true. """
highlight(pred::Function, tr::Trace) = map(t->maybe_highlight(pred(t), t.value), tr)
maybe_highlight(ishigh::Bool, val) = ishigh ? Highlight(val) : val

struct Bold
    content
end
""" `TraceCalls.show_return_val(io::IO, mime, x)` is the function used by `TraceCalls` to
display each return value. Defaults to calling `show_val`. """
show_return_val(io::IO, mime, x) = try_show_val(io, mime, Bold(FontColor("green", x)))

function show_val(io::IO, mime::MIME"text/html", x::Bold)
    write(io, "<b>")
    try_show_val(io, mime, x.content)
    write(io, "</b>")
end
function show_val(io::IO, mime::MIME"text/plain", x::Bold)
    with_crayon(io, Crayon(bold=true)) do
        try_show_val(io, mime, x.content)
    end
end


const largest_show_size = fill(100)

function is_tree_very_large(tr::Trace, warn=true)
    size = tree_size(tr)
    if (r = (size > largest_show_size[] && largest_show_size[] > 0)) && warn
        Base.warn("Trace is very large (size $size > $(TraceCalls.largest_show_size[])); it was pruned before displaying. Enter `TraceCalls.largest_show_size[] = -1` to disable this behaviour.")
    end
    r
end

# Cut down tr until it's below largest_show_size
function cut_tree_reasonable(tr::Trace)
    s = Int(floor(sqrt(largest_show_size[]))) # using sqrt is silly; should be log?
    while tree_size(tr) >= largest_show_size[]
        tr = prune(tr, s, s)
        s -= 1
    end
    tr
end
   
function Base.show(io::IO, mime::MIME"text/html", tr::Trace)
    if is_tree_very_large(tr) return show(io, mime, cut_tree_reasonable(tr)); end
    show_call(io::IO, mime, tr)
    margin = "4px"  # a decent compromise, default is 9px
    write(io, """<ul style="  margin-top: $margin; margin-bottom: $margin;">""")
    for called in tr.called
        write(io, "<li>")
        show(io, MIME"text/html"(), called)
        write(io, "</li>")
    end
    write(io, "</ul>")
end

call_indentation = 3
indent = 0
function Base.show(io::IO, mime::MIME"text/plain", tr::Trace)
    if is_tree_very_large(tr,
                          # Avoid printing the warning twice in IJulia.
                          # https://github.com/JuliaLang/IJulia.jl/issues/574
                          !isdefined(Main, :IJulia))
        return show(io, mime, cut_tree_reasonable(tr))
    end
    write(io, string(" " ^ indent, "- "))
    show_call(io, mime, tr.func, tr)
    write(io, "\n")
    global indent += call_indentation
    try
        for called in tr.called
            show(io, mime, called)
        end
    finally
        indent -= call_indentation
    end
end

function show_kwargs(io::IO, mime, kwargs)
    write(io, "; ")
    for (i, (sym, val)) in enumerate(kwargs)
        write(io, string(sym))
        write(io, sym isa Symbol ? "=" : " = ")  # to eventually support @tracelog
        try_show_val(io, mime, val)
        if i != length(kwargs) write(io, ", ") end
    end
end
function show_args(io::IO, mime, args)
    for (i, arg) in enumerate(args)
        try_show_val(io, mime, arg)
        if i != length(args) write(io, ", ") end
    end
end
show_kwargs(io::IO, mime, kwargs::Tuple{}) = nothing

is_atom = false

@require Juno begin
    import Media
    using Juno: Tree

    if Juno.isactive()
        global is_atom = true

        juno_tree(tr::Trace) =
            Tree(HTML(TraceCalls.call_html(tr)), map(juno_tree, tr.called))
        function Juno.render(inl::Juno.Inline, tr::Trace)
            Juno.render(inl, juno_tree(tr))
        end
    end
end

function show_func_name(io::IO, mime::MIME"text/html", tr::Trace)
    color = is_atom ? "" : "color: black;"
    write(io, url(tr) == "" ? string(tr.func) :
          """<a href="$(url(tr))" target="_blank" style="$color text-decoration: none; border-bottom: 1px #C3C3C3 dotted">$(tr.func)</a>""")
end

show_func_name(io::IO, mime::MIME"text/plain", tr::Trace) = write(io, string(tr.func))

""" `show_call(io::IO, mime, ::Any, tr::Trace)` is called to display
each trace.  Overload it for specific functions with
`TraceCalls.show_call(io::IO, mime, ::typeof(function_name), tr::Trace) = ...` """
function show_call end

function show_call_base(io, mime, tr) # show_call without the return value
    show_func_name(io, mime, tr)
    write(io, "(")
    show_args(io, mime, tr.args)
    show_kwargs(io, mime, tr.kwargs)
    write(io, ")")
end

function show_call_(io, mime, tr)
    show_call_base(io, mime, tr)
    write(io, " => ")
    show_return_val(io, mime, tr.value)
end

is_highlighted(tr::Trace) = tr.value isa Highlight
highlight_default(mime::MIME"text/html") = :greenyellow
highlight_default(mime::MIME"text/plain") = :light_gray
highlight_color(tr::Trace, mime) =
    tr.value.color == :default ? highlight_default(mime) : tr.value.color
function show_call(io::IO, mime::MIME"text/html", ::Any, tr::Trace)
    # Could use CSS https://www.computerhope.com/issues/ch001034.htm
    background = is_highlighted(tr) ? "background-color:$(highlight_color(tr, mime))" : ""
    write(io, """<pre style="display: inline; $background">""")
    show_call_(io, mime, tr)
    write(io, "</pre>")
end

function show_call(io::IO, mime::MIME"text/plain", ::Any, tr::Trace)
    cr = is_highlighted(tr) ? Crayon(background=highlight_color(tr, mime)) : Crayon()
    with_crayon(io, cr) do
        show_call_(io, mime, tr)
    end
end
show_call(io::IO, mime, tr::Trace) = show_call(io, mime, tr.func, tr)

call_html(tr::Trace) = get_io_output(io->show_call(io, MIME"text/html"(), tr))

""" `get_io_output() do io ... write(io) ...` returns everything that was written
to `io`, as a string. """
function get_io_output(fn::Function)
    buf = IOBuffer()
    fn(buf)
    return String(take!(buf))
end

struct REPR
    text
    html
    function REPR(x)
        s_text = 
        s_html = IOBuffer(); try_show_val(s_html, MIME"text/html"(), x)
        new(get_io_output() do io; try_show_val(io, MIME"text/plain"(), x) end,
            get_io_output() do io; try_show_val(io, MIME"text/html"(), x) end)
    end
end
show_val(io::IO, ::MIME"text/plain", r::REPR) = write(io, r.text)
show_val(io::IO, ::MIME"text/html", r::REPR) = write(io, r.html)

################################################################################

""" `filter_cutting(f::Function, tr::Trace)` removes all subtraces (and their
callees) for which `f(tr)` is false. """
filter_cutting(f::Function, tr::Trace) =
    Trace(tr, Trace[filter_cutting(f, sub_tr) for sub_tr in tr.called if f(sub_tr)])

"""    filter_lineage(f::Function, tr::Trace; highlight=true, keep_descendents=true)

keeps all subtraces for which `f(::Trace)` is true of some of its descendents OR
ancestors. """
function filter_lineage(f::Function, tr::Trace; highlight=true, keep_descendents=true)
    if f(tr)
        # FIXME: we should probably go down and keep those descendents for which f(tr) is
        # true.
        res = keep_descendents ? tr : prune(tr)
    else
        called0 = Trace[filter_lineage(f, sub_tr; highlight=false,
                                       keep_descendents=keep_descendents)
                        for sub_tr in tr.called]
        called = filter(c->c!=empty_trace_dummy, called0)
        res = isempty(called) ? empty_trace_dummy : Trace(tr, called)
    end
    return highlight ? TraceCalls.highlight(f, res) : res
end


filter_descendents(f, tr) = # helper
    # Special casing because of #18852
    isempty(tr.called) ? Trace[] : Trace[t for sub in tr.called for t in filter_(f, sub)]
filter_(f, tr) =
    f(tr) ? [Trace(tr, filter_descendents(f, tr))] : filter_descendents(f, tr)
Base.filter(f::Function, tr::Trace) = Trace(tr, filter_descendents(f, tr))

""" `filter_func(functions::Vector, tr::Trace)` keeps only Trace objects whose function
is one of `functions` """
filter_func(functions::Vector, tr::Trace) = filter(tr->tr.func in functions, tr)
filter_func(func::Function, tr::Trace) = filter_func([func], tr)

""" `collect(tr::Trace)` returns a vector of all `Trace` objects within `tr`. """
Base.collect(tr::Trace) = Trace[tr; mapreduce(collect, vcat, [], tr.called)]

""" `prune(tr::Trace, max_depth::Int=0, max_length::Int=1000000000)` prunes the Trace-tree
maximum tree depth, and maximum length (number of branches in each node). 
(convenient to first explore a trace at a high-level).

`prune(tr)` prunes all of `tr`'s children (so all that remains is the function call). """
prune(tr::Trace, max_depth::Int=0, max_length::Int=1000000000) =
    Trace(tr, Trace[prune(sub_tr, max_depth-1, max_length)
                    for sub_tr in tr.called[1:min(length(tr), max_length)]
                    if max_depth > 0])

is_mutating(tr::Trace) = is_mutating(tr.func)
is_mutating(f::Function) = last(string(f)) == '!'
is_mutating(typ::Type) = typ

function is_inferred(tr::Trace)
    try
        apply_macro(:@inferred, tr, Base.Test)
        return true
    catch e
        if isa(e, ErrorException)&&contains(e.msg, "does not match inferred return type")
            return false
        else
            rethrow()
        end
    end
end
map_is_inferred(tr::Trace) = redgreen(map(is_inferred, tr))
""" `@trace_inferred ...some_expression...` computes the trace of `some_expression`,
and shows `true` for all type-stable function calls in the trace, and `false` otherwise.
"""
macro trace_inferred(expr)
    esc(:($TraceCalls.map_is_inferred($TraceCalls.@trace $expr)))
end

function redgreen(x::Number)
    # red is ff0000, of course...
    f = clamp(x, 0.0, 1.0) * 1.0
    return (1-f, f, 0.0)
end
redgreen(x::Bool) = x ? "green" : "red"

""" `redgreen(tr::Trace; map=identity)` colors all `value`s as shades of red/green, with
`map(value) == 0/false` being pure red and `1/true` being pure green. `map` can be
used to normalize the values into the `[0, 1]` range. """
redgreen(tr::Trace; map=identity) =
    TraceCalls.map(sub->FontColor(redgreen(map(sub.value)), sub.value), tr)
""" `greenred(tr::Trace)` is like `redgreen`, but with 0/false->green, 1/true->red. """
greenred(tr::Trace; map=identity) = redgreen(tr::Trace; map=x->1-number(map(x)))

# This is not so great... Wonder what a better design would be? Can't really use
# broadcasting because a) not documented b) we just want to apply through the
# cosmetic stuff, not the actual value if it turns out to be a vector.
apply_to_value(f::Function, tr::Trace) = apply_to_value(f, tr.value)
apply_to_value(f::Function, x) = f(x)
apply_to_value(f::Function, x::FontColor) =
    FontColor(x.color, apply_to_value(f, x.content))
apply_to_value_fn(f::Function) = tr->apply_to_value(f, tr)
value(x) = x
value(tr::Trace) = value(tr.value)
value(x::FontColor) = x.content

Base.maximum(tr::Trace) = maximum(sub.value for sub in collect(tr))
Base.round(tr::Trace, n::Int) = map(sub->round(sub.value, n), tr)
Base.signif(tr::Trace, n::Int) = map(sub->signif(sub.value, n), tr)
Base.normalize(tr::Trace, div=value(tr)) = map(apply_to_value_fn(x->x/div), tr)

"""
    measure(mac_or_fun::Union{Expr, Function}, tr::Trace; normalize=false,
            threshold=0, explore_worst=false)`

Apply `mac_or_fun` to every function call in `tr`, with a `green->red` color scheme.
Remove all function calls whose result is below `threshold`. `measure` runs `tr()` first
to get rid of the compile time, but that is by not always sufficient. Call `measure`
twice for best accuracy (esp. with `@allocated`).

When `explore_worst=true`, `measure` will only expand the worst child of each call.
This is a useful way of quickly exploring a trace that would be too large to profile
otherwise.
"""
function measure(mac_or_fun::Union{Expr, Function}, trace::Trace; normalize=false,
                 threshold=0, explore_worst=false)
    f = handle_mac(mac_or_fun)
    trace() # run it once, to get (at least some of) the JIT behind us
    top_value = f(trace)
    if normalize && top_value == 0
        warn("The normalization is 0 - disabling normalization")
        normalize = false
    end
    norm(x) = normalize ? (x / top_value) : x
    shorten(x) = x
    shorten(x::AbstractFloat) = signif(x, 4)
    function trav(tr, res)
        new_called = Trace[]
        called_res = map(norm∘f, tr.called)
        worst = isempty(tr.called) ? nothing : maximum(called_res)
        for (t, t_res) in zip(tr.called, called_res)
            if number(t_res) >= threshold
                push!(new_called,
                      ((!explore_worst || t_res == worst) ?
                       trav(t, t_res) :
                       Trace(t.func, t.args, t.kwargs, [], shorten(t_res))))
            end
        end
        return Trace(tr.func, tr.args, tr.kwargs, new_called, shorten(res))
    end
    tr2 = trav(trace, norm(top_value))
    # number(x) is to handle TrialEstimate
    return greenred(tr2; map=(value(tr2)==0 ? identity : x->number(x)/number(value(tr2))))
end

number(x::Number) = x

only_exceptions(trace::Trace) = filter(tr->tr.value isa Exception, trace)
macro stacktrace(to_trace, expr)
    esc(:($TraceCalls.only_exceptions(@trace($to_trace, $expr))))
end

macro stacktrace(expr)
    esc(:($TraceCalls.@stacktrace () $expr))
end

################################################################################
# compare_past_trace

struct IsEqual
    a
    b
end

iseql(isd::IsEqual) = iseql(isd.a, isd.b)
iseql(tr::Trace) = iseql(tr.value)
iseql(a::Nullable, b::Nullable) = isequal(a, b)
iseql(a, b) = a == b
function show_val(io::IO, mime::MIME"text/html", isd::IsEqual)
    write(io, """<font color=$(redgreen(iseql(isd)))>""")
    if iseql(isd)
        write(io, "Same: ")
        try_show_val(io, mime, isd.a)
    else
        write(io, "<u>before</u>: ")
        try_show_val(io, mime, isd.a)
        write(io, " <u>vs. now:</u> ")
        try_show_val(io, mime, isd.b)
    end
    write(io, """</font>""")
end
function show_val(io::IO, mime::MIME"text/plain", isd::IsEqual)
    if iseql(isd)
        with_crayon(io, Crayon(foreground=:green)) do
            write(io, "Same: ")
            try_show_val(io, mime, isd.a)
        end
    else
        with_crayon(io, Crayon(foreground=:red)) do
            with_crayon(io, Crayon(underline=true)) do
                write(io, "before")
            end
            write(io, ": ")
            try_show_val(io, mime, isd.a)
            write(io, " ")
            with_crayon(io, Crayon(underline=true)) do
                write(io, "vs. now")
            end
            write(io, ": ")
            try_show_val(io, mime, isd.b)
        end
    end
end

""" `compare_past_trace(old_trace::Trace; filter_out_equal=true))` reruns every function
call in `old_trace`, and shows in red where the new results differ from the old.  If
`filter_out_equal==true`, only show the non-equal results. Override
`TraceCalls.iseql(a, b)` to implement custom comparisons. """
function compare_past_trace(old_trace::Trace; filter_out_equal=true)
    tr2 = map(sub_tr->IsEqual(sub_tr.value, get_error_or_value(sub_tr)),
              old_trace)
    return filter_out_equal ? filter(!iseql, tr2) : tr2
end

""" `@traceable trace_log(args...; kwargs...) = nothing` is a dummy, always-traced
function you can call to add extra information to your traces. For instance,
`trace_log(:inside_foo; y=2, sqrt_of_x=sqrt(x))` """
@traceable trace_log(args...; kwargs...) = nothing


################################################################################
# groupby

struct Group
    #key    # temporarily taken out until https://github.com/simonster/JLD2.jl/issues/37
            # is resolved
    traces::Vector{Trace}
end
Group(key, traces) = Group(traces)
value(grp::Group) = value(grp[1])

Base.getindex(gr::Group, i) = gr.traces[i]
Base.push!(gr::Group, tr::Trace) = push!(gr.traces, tr)
Base.length(gr::Group) = length(gr.traces)
Base.endof(gr::Group) = length(gr)
(grp::Group)() = grp[1]()
function show_group(io, mime, gr)
    N = length(gr)
    write(io, "$N call", N>1?"s":"", " like ")
    show_call_(io, mime, gr[1])
end
# Necessary to split because otherwise it's ambiguous
Base.show(io::IO, mime::MIME"text/plain", gr::Group) = show_group(io, mime, gr)
function Base.show(io::IO, mime::MIME"text/html", gr::Group)
    write(io, "<pre>")
    show_group(io, mime, gr)
    write(io, "</pre>")
end
Base.show(io::IO, gr::Group) = show_group(io, MIME"text/plain"(), gr)

apply(mac_or_fun, group::Group) =
    # Only apply mac_or_fun to the first trace, which is assumed to be representative
    Group([map(mac_or_fun, group[1]); group[2:end]])

measure(mac_or_fun::Union{Expr, Function}, groups::Vector{Group}) =
    map_groups(mac_or_fun, groups)

map_groups(mac_or_fun, groups::Vector{Group}) =
    Group[apply(return_error_fn(handle_mac(mac_or_fun)), grp) for grp in groups]
# map_groups(mac_or_fun, groups1::Vector{Group}, groups2::Vector{Group}) =
#     Group[apply(return_error_fn(handle_mac(mac_or_fun)), grp1, grp2)
#           for (grp1, grp2) in zip(groups1, groups2)]


if_not_error(fn::Function, group::Group) =
    value(group) isa Exception ? apply(tr->value(group), group) : fn()
if_not_error(fn::Function, group1::Group, group2::Group) =
    (value(group1) isa Exception || value(group2) isa Exception ?
     # Return the exception
     apply(tr->value(value(group1) isa Exception ? group1 : group2), group1) :
     fn())

macro delegate_to_group(funs...)
    esc(quote
        $([:($fun(grp::Group, args...; kwargs...) = apply($fun, grp, args...; kwargs...))
           for fun in funs]...)
        end)
end
@delegate_to_group is_inferred

redgreen(grp::Group) = Group([redgreen(grp[1]); grp[2:end]])

function groupby(by::Function, trace::Trace)
    di = OrderedDict{Any, Group}()
    for tr in collect(trace)
        key = by(tr)
        group = get!(()->Group(key, []), di, key)
        push!(group, prune(tr))
    end
    collect(values(di))
end

################################################################################
# BenchmarkTools

const judgement_colors = Dict(:invariant=>:black, :regression=>:red, :improvement=>:green)

@require BenchmarkTools begin
    using BenchmarkTools: @benchmark, Trial, TrialEstimate, prettytime, prettymemory
    using BenchmarkTools: memory, ratio, judge, Benchmark, TrialEstimate, Trial, params
    using BenchmarkTools: loadparams!, run, TrialJudgement

    export benchmark
    TraceCalls.show_val(io::IO, mime, t::Trial) =
        # The default `show` method only shows the time. 
        print(io, "Trial(", prettytime(time(t)), ", ",
              prettymemory(memory(t)), ")")
    TraceCalls.show_val(io::IO, mime, t::TrialEstimate) =
        print(io, "TrialEstimate(", prettytime(time(t)), ", ",
              prettymemory(memory(t)), ")")

    """ `benchmark(tr::Trace; warmup=true)` calls `BenchmarkTools.@benchmark` on `tr`. If
    `tr` is already a benchmark, then we reuse that benchmark's parameters, so the new
    results are more comparable and faster-computed. """
    function benchmark(tr::Trace; warmup=true)
        if value(tr) isa Union{Benchmark, Trial, TrialEstimate}
            benchmarkable = apply_macro(:@benchmarkable, tr)
            loadparams!(benchmarkable, params(value(tr)))
            if warmup;
                # See https://discourse.julialang.org/t/benchmarktools-theory-and-practice/5728/2
                BenchmarkTools.warmup(benchmarkable) end
            run(benchmarkable)
        else
            apply_macro(:@benchmark, tr)
        end
    end
    number(x::TrialEstimate) = time(x)
    # That's type-piracy. Remove it once https://github.com/JuliaCI/BenchmarkTools.jl/pull/73
    # gets decided. Could also be fixed by creating our own "divide" function.
    Base.:/(a::TrialEstimate, b::TrialEstimate) = ratio(a, b)

    BenchmarkTools.ratio(group1::Group, group2::Group) =
        if_not_error(()->apply(tr->ratio(value(tr), value(group2)), group1),
                     group1, group2)
    add_color(judge::TrialJudgement) = FontColor(judgement_colors[time(judge)], judge)
    BenchmarkTools.judge(new_group::Group, old_group::Group) =
        if_not_error(()->apply(tr->add_color(judge(value(tr), value(old_group))), new_group),
                     new_group, old_group)
end

################################################################################
# TraceGroupBenchmark

struct GroupBenchmark
    estimator::Function
    groups::Vector{Group}
end
Base.getindex(gb::GroupBenchmark, ind) = gb.groups[ind]
Base.length(gb::GroupBenchmark) = length(gb.groups)
Base.endof(gb::GroupBenchmark) = length(gb)
Base.sort(gb::GroupBenchmark; kwargs...) =
    GroupBenchmark(gb.estimator, sort(gb.groups; kwargs...))

function show_gb(io::IO, mime, gb::GroupBenchmark)
    write(io, "Benchmark\n")
    for group in gb.groups
        write(io, " ")
        show(io, mime, group)
        write(io, "\n")
    end
end
Base.show(io::IO, mime::MIME"text/html", gb::GroupBenchmark) = show_gb(io, mime, gb)
Base.show(io::IO, mime::MIME"text/plain", gb::GroupBenchmark) = show_gb(io, mime, gb)
Base.time(grp::Group) = value(grp) isa Exception ? 10000 : length(grp) * time(value(grp))
signature(grp::Group) = signature(grp[1])

@require BenchmarkTools begin
    using BenchmarkTools: run

    trace_benchmark(trace::Trace; estimator=median) =
        sort(run(GroupBenchmark(estimator, groupby(signature, map(tr->:not_run, trace)))),
             by=time, rev=true)
    trace_benchmark(code::Function, to_trace; kwargs...) =
        trace_benchmark(tracing(code, to_trace); kwargs...)
    trace_benchmark(file_to_include::String, to_trace; kwargs...) =
        define_benchmark(()->include(file_to_include), to_trace; kwargs...)
    BenchmarkTools.run(gb::GroupBenchmark) =
        GroupBenchmark(gb.estimator, map_groups(gb.estimator∘benchmark, gb.groups))
    BenchmarkTools.ratio(gb1::GroupBenchmark, gb2::GroupBenchmark) =
        GroupBenchmark(gb1.estimator, map(ratio, gb1.groups, gb2.groups))
    function BenchmarkTools.judge(new_gb::GroupBenchmark, old_gb::GroupBenchmark;
                                  sort=true)
        @assert(map(signature, new_gb.groups) == map(signature, old_gb.groups),
                "The two GroupBenchmarks are not in the same order. The new benchmark should be computed by calling `run(old_benchmark)`")
        gb = GroupBenchmark(new_gb.estimator, map(judge, new_gb.groups, old_gb.groups))
        by(grp) = (value(grp) isa Exception ? -1000000 : -(time(ratio(value(grp)))-1))
        return sort ? Base.sort(gb; by=by) : gb
    end
end

################################################################################

"""    compilation_times(to_trace, trace::Trace; warmed_up_precompile=true)

Compute the time to compile each distinct call signature in `trace`, then aggregate
it by method to return a report of how much total time is spent compiling each function
called by `trace`.
"""
function compilation_times(to_trace, trace::Trace; warmed_up_precompile=true)
    # My understanding is that `precompile(f)` triggers compilation (or at least,
    # inference) on some of the functions that `f` calls. Thus, we iterate
    # `collect(trace)[end:-1:1]` so that the leaves are precompiled first.
    signatures = OrderedSet(signature(tr) for tr in collect(trace)[end:-1:1])
    precompile(signature) = Base.precompile(signature[1], signature[2:end])
    # Recommended, since otherwise compilation times are reported much higher.
    # My guess is that `precompile(f, type_tuple)` spends a lot of time generating
    # code for `type_tuple`. Not 100% sure, though.
    if warmed_up_precompile; foreach(precompile, signatures) end
    @trace to_trace nothing   # reset Julia's method table for these functions
    dict = Dict{Any, Float64}()
    for signature in signatures
        method = which(signature[1], signature[2:end])
        dict[method] = get(dict, method, 0) + @elapsed(precompile(signature))
    end
    return OrderedDict(sort(collect(dict), by=last, rev=true)...)
end

""" `@compilation_times to_trace expr` (similar to `@trace to_trace expr`) executes
`expr` while tracing the modules/functions/files in `to_trace`, then returns an estimate
of how much time it takes to compile each of the method specializations called within
`expr`. """
macro compilation_times(to_trace, expr)
    @gensym ev_to_trace trace
    esc(quote
        $ev_to_trace = $to_trace
        $trace = $TraceCalls.@trace($ev_to_trace, $expr)
        $TraceCalls.compilation_times($ev_to_trace, $trace)
        end)
end

end # module
