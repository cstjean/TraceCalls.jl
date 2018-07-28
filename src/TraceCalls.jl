__precompile__()
module TraceCalls

using Requires
using MacroTools
using MacroTools: combinedef, combinearg, longdef1
using Base.Test: @inferred
using DataStructures: OrderedDict, OrderedSet
using Memoize
using Base: url
using Base.Core: MethodInstance
using Crayons: Crayon, inv
import Base: +, -

export @traceable, @trace, Trace, prune, FontColor, Bold,
    is_inferred, map_is_inferred, redgreen, greenred,
    compare_past_trace, filter_func, apply_macro, @stacktrace, measure, tree_size,
    is_mutating, REPR, filter_cutting, NoTraceable, trace_log, filter_lineage,
    bottom, top, highlight, @show_val_only_type, objects_in, signature, group_by,
    map_groups, trace_benchmark, @compilation_times, specializations, is_root,
    function_is

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
Base.maximum(tr::Trace) = maximum(sub.value for sub in collect(tr))
Base.round(tr::Trace, n::Int) = map(sub->round(sub.value, n), tr)
Base.signif(tr::Trace, n::Int) = map(sub->signif(sub.value, n), tr)
Base.normalize(tr::Trace, div=value(tr)) = map(apply_to_value_fn(x->x/div), tr)
Base.sort(tr::Trace; rev=false, by=tr->value(tr)) =
    Trace(tr, sort(tr.called; rev=rev, by=by))

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
            
function_is(funs::Function...) = tr->tr.func in funs

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
    top.delta==0 ? tr[args...] : deepest_call(tr)[Top(top.delta-1), args...]


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

################################################################################

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

function collect_!(trace_list, tr::Trace)
    push!(trace_list, tr)
    for sub in tr.called
        collect_!(trace_list, sub)
    end
end

""" `collect(tr::Trace)` returns a vector of all `Trace` objects within `tr`. """
function Base.collect(tr::Trace)
    trace_list = Trace[]
    collect_!(trace_list, tr)
    trace_list
end

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

number(x::Number) = x


################################################################################
# group_by

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
is_root(grp::Group) = is_root(grp[1])
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
    # Hmmm, not a great definition?
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
is_inferred(grp::Group) = is_inferred(grp[1])

redgreen(grp::Group) = Group([redgreen(grp[1]); grp[2:end]])

function group_by(by::Function, trace::Trace)
    di = OrderedDict{Any, Group}()
    for tr in collect(trace)
        key = by(tr)
        group = get!(()->Group(key, []), di, key)
        push!(group, prune(tr))
    end
    collect(values(di))
end


################################################################################


include("scrub_stderr.jl")
include("code_update.jl")
include("tracing.jl")
include("show.jl")
include("specializations.jl")
include("profile.jl")
include("debug.jl")

""" `@traceable trace_log(args...; kwargs...) = nothing` is a dummy, always-traced
function you can call to add extra information to your traces. For instance,
`trace_log(:inside_foo; y=2, sqrt_of_x=sqrt(x))` """
@traceable trace_log(args...; kwargs...) = nothing

end # module
