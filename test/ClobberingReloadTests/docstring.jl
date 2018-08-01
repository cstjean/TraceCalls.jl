""" aasd"""
function fooo(x)
    # In 0.6, this could just be a quoted expression instead of a separate file, but
    # in 0.5, `quote` doesn't handle docstrings properly.
    x+2
end

