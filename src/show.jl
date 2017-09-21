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

################################################################################

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

################################################################################

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
