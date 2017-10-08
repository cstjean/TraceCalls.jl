const current_called =
    fill(Trace[]) # the current vector onto which to tack the next function calls

# TODO: consider @memoize Dict{Tuple{Expr}, Any}
# But I need to make sure that Revise doesn't modify the Expr
splitdef_memo(def::Expr) = MacroTools.splitdef(def)

is_traceable(def) =
    (is_generated_function_definition(def) ?
     is_traceable(generated2normal(def)) :
     (is_function_definition(def) && (di=splitdef_memo(def); !is_call_definition(di)) &&
      !is_fancy_constructor_definition(di)))

""" `@traceable_loose(expr)` is like `traceable(expr)`, but doesn't error on non-function
definitions (it's a helper for `@traceable begin ... end`) """
macro traceable_loose(expr)
    is_traceable(expr) ? esc(:($TraceCalls.@traceable $expr)) : esc(expr)
end

global gensym_counter = 0  # necessary because of JuliaLang#23809
my_gensym() = Symbol(:__tc_gensym_, (global gensym_counter+=1;))

""" Turns `::Int=5` (and `_`) into `some_gensym::Int=5` """
function handle_missing_arg(arg)
    arg_name, arg_type, is_splat, default = splitarg(arg)
    combinearg((arg_name === nothing || arg_name == :_) ? my_gensym() : arg_name,
               arg_type, is_splat, default)
end

""" Overload `TraceCalls.store(x)` for specific types to change how `TraceCalls` stores
its arguments. For example, `TraceCalls.store(x::Vector) = copy(x)`. See also ?REPR. """
store(x) = x

arg_name(arg) = splitarg(arg)[1]
is_splat(arg) = splitarg(arg)[3]
arg_name_splat(arg) = is_splat(arg) ? Expr(:..., arg_name(arg)) : arg_name(arg)
function typed_arg(arg)
    name, arg_type = splitarg(arg)
    if is_splat(arg) arg_type = Any end
    return :($name::$arg_type)
end

function tracing_fun(splitdef_di::Dict, do_body_name::Symbol)::Dict
    # Returns a function definition that stores its arguments as a Trace and passes
    # them to do_body_name
    di = splitdef_di
    di[:args] = map(handle_missing_arg, di[:args])
    di[:kwargs] = map(handle_missing_arg, di[:kwargs])
    fname = di[:name]

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
            $res = $do_body_name($(map(arg_name_splat, di[:args])...);
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
    updated_fun_di
end

function do_body_fun(di)
    fname = di[:name]
    do_body_di = copy(di)
    do_body_di[:name] = fname = isa(fname, Expr) ? gensym() : gensym(fname)
    combinedef(do_body_di), fname
end    

function tracing_code_function(fdef::Expr)
    di = splitdef_memo(fdef)
    do_body_fdef, do_body = do_body_fun(di)
    quote
        @inline $do_body_fdef
        $(combinedef(tracing_fun(di, do_body)))
    end
end

function tracing_code_generated(fdef::Expr)
    di = splitdef_memo(generated2normal(fdef))
    do_body_fdef, do_body = do_body_fun(di)
    tfun_di = tracing_fun(di, do_body)
    tfun_di[:body] = Expr(:quote, tfun_di[:body])
    quote
        $(normal2generated(do_body_fdef))
        $(normal2generated(combinedef(tfun_di)))
    end
end

#"""  Takes a function definition, and returns a tracing version of it. """
tracing_code(fdef::Expr) =
    (is_generated_function_definition(fdef) ?
     tracing_code_generated(fdef) :
     tracing_code_function(fdef))

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
    di = splitdef_memo(fdef)
    signature = (module_name(mod),         # loose approximation of the real signature
                 :($(di[:name])($([splitarg(arg)[2] for arg in di[:args]]...))))
    
    traceable_definitions[signature] =
          RevertibleCodeUpdate(tracing_code,
                               CodeUpdate(ModDict(mod=>OrderedSet([MakeRelocatableExpr(fdef)]))))

    return esc(fdef)
end

#@memoize Dict{Tuple{Expr}, Any} strip_docstring_memo(expr0) = strip_docstring(expr0)
# There might be an issue with the memo if we decide to macroexpand the code, since the
# macroexpansion depends on the environment. It's probably a negligible issue.
@memoize Dict{Tuple{Expr, Bool}, Any} function traceable_update_handle_expr(expr0::Expr, trace_generated_functions)
    # maybe strip_docstring is not necessary anymore - September '17
    expr = strip_docstring(expr0)
    (!is_traceable(expr) || (!trace_generated_functions &&
                             is_generated_function_definition(expr))
     ? nothing : tracing_code(expr))
end
traceable_update_handle_expr(::Any, ::Any) = nothing
clear_handle_expr_memo!() =
    empty!(eval(TraceCalls, Symbol("##traceable_update_handle_expr_memoized_cache")))

""" `traceable_update(obj)::RevertibleCodeUpdate` produces the traceable code for the
given object. """
function traceable_update end

custom_when_missing(x) = warn(x)
custom_when_missing(fail::UpdateInteractiveFailure) =
    warn("Cannot trace $(fail.fn); use `@traceable` to trace methods defined interactively. See ?@traceable and the user manual for details.")
traceable_update(obj::Union{Module, String}, trace_generated_functions) =
    update_code_revertible(x->traceable_update_handle_expr(x, trace_generated_functions),
                           obj)
traceable_update(f::Union{Function, Type}, trace_generated_functions) =
    update_code_revertible(x->traceable_update_handle_expr(x, trace_generated_functions),
                           f, when_missing=custom_when_missing)

traceable_update(tup::Tuple, trace_generated_functions) =
    merge(map(x->traceable_update(x, trace_generated_functions), tup)...)
traceable_update(tup::Tuple{}, ::Bool) = EmptyRevertibleCodeUpdate()

""" The `RevertibleCodeUpdate` for the code from the `@traceable` macros. """
traceable_macro_update() = merge(EmptyRevertibleCodeUpdate(),
                                 values(traceable_definitions)...)


""" `@trace (foo, NoTraceable()) ...` will only trace `foo`; not the `@traceable`
functions. """
struct NoTraceable end
traceable_update(::NoTraceable, ::Any) = EmptyRevertibleCodeUpdate()
""" `traceable_update_code(x)` returns the code of the update. For debugging. """
traceable_update_code(x, trace_generated_functions) =
    map(to_expr, collect(reduce((s1,s2)->OrderedSet([collect(s1); collect(s2)]),
                                collect(values(traceable_update(x, trace_generated_functions).apply.md)))))
traceable_update_revert_code(x) =
    map(to_expr, collect(reduce(merge, collect(values(traceable_update(x, trace_generated_functions).revert.md)))))


function with_tracing_definitions(body::Function, obj, trace_generated_functions)
    if obj isa Tuple && NoTraceable() in obj
        upd = traceable_update(obj, trace_generated_functions)
    else
        upd = merge(traceable_update(obj, trace_generated_functions),
                    traceable_macro_update())
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

struct Root
    fun::Function
end
Base.show(io::IO, root::Root) = write(io, "Root$(root.fun)")
(r::Root)() = r.fun()

is_root(::Root) = true
is_root(::Any) = false
is_root(tr::Trace) = is_root(tr.func)

""" `recording_trace(fun::Function)` sets up a fresh Trace recording state, then executes
`fun` and returns the resulting Trace object. """
function recording_trace(fun::Function)
    current_called[] = called = Trace[]
    res = get_error_or_value(fun)
    top_trace = Trace(Root(fun), (), (), called, res)
    current_called[] = Trace[] # don't hang on to that memory unnecessarily
    top_trace
end
recording_trace(tr::Trace) = recording_trace(()->tr())  # could be handled more gracefully

function Base.trace(body::Union{Function, Trace}, to_trace=();
                    generated_functions::Bool=true)
    with_tracing_definitions(to_trace, generated_functions) do
        # To debug, just use `with_tracing_definitions()` interactively
        recording_trace(body)
    end
end

macro trace(expr)
    :($TraceCalls.trace(()->$(esc(expr))))
end

macro trace(to_trace, expr)
    :($TraceCalls.trace(()->$(esc(expr)), $(esc(to_trace))))
end
