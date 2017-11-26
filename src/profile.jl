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
        sort(run(GroupBenchmark(estimator, group_by(signature, map(tr->:not_run, trace)))),
             by=time, rev=true)
    trace_benchmark(code::Function, to_trace; kwargs...) =
        trace_benchmark(trace(code, to_trace); kwargs...)
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

"""
    compilation_times(to_trace, trace::Trace; warm_up=true,
                      comp_fun=Base.precompile)

Compute the time to compile each distinct call signature in `trace`, then aggregate
it by method to return a report of how much total time is spent compiling each function
called by `trace`.
"""
function compilation_times(to_trace, trace::Trace; warm_up=true,
                           comp_fun=Base.precompile)
    # My understanding is that `precompile(f)` triggers compilation (or at least,
    # inference) on some of the functions that `f` calls. Thus, we iterate
    # `collect(trace)[end:-1:1]` so that the leaves are precompiled first.
    # It would potentially be more exact if I used:
    # https://github.com/JuliaLang/julia/blob/903644385b91ed8d95e5e3a5716c089dd1f1b08a/base/reflection.jl#L710-L714
    signatures = OrderedSet(signature(tr) for tr in collect(trace)[end:-1:1])
    do_comp(signature) = comp_fun(signature[1], signature[2:end])
    # Recommended, since otherwise compilation times are reported much higher.
    # My guess is that `do_comp(f, type_tuple)` spends a lot of time generating
    # code for `type_tuple`. Not 100% sure, though.
    if warm_up; foreach(do_comp, signatures) end
    @trace to_trace nothing   # reset Julia's method table for these functions
    dict = Dict{Any, Float64}()
    for signature in signatures
        method = which(signature[1], signature[2:end])
        dict[method] = get(dict, method, 0) + @elapsed(do_comp(signature))
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

