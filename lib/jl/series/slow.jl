"""
slow implementations of common functions :)
"""

function shift_1!(arr, len)
    p = arr[1]
    for n in 1:len
        arr[n] = arr[n + 1]
    end
    return p
end

function init_window(arr, window, default)
    len = length(arr)
    res = typeof(arr)(undef, len)
    res[1:window] .= default
    return len, res
end

function rolling_norm_simple(arr, window, default=NaN)
    len, out = init_window(arr, window, default)
    wm1 = window - 1
    c = 1
    for n in window:len - 3
        mn = mx = arr[c]
        for i in c + 1:c + wm1
            v = arr[i]
            if isnan(v)
                continue
            end
            if v > mx || isnan(mx)
                mx = v
            elseif v < mn || isnan(mn)
                mn = v
            end
        end
        out[n] = (mx != mn) ? (arr[n] - mn) / (mx - mn) : 0
        c += 1
    end
    return out
end

function rolling_norm_slow(arr, window, default=NaN)
    len, out = init_window(arr, window, default)
    v = view(arr, 1:window)
    out[window] = (arr[window] - minimum(v)) / (maximum(v) - minimum(v))
    wm1 = window - 1
    for n in window + 1:len
        v = view(arr, n - wm1:n)
        out[n] = (arr[n] - minimum(v)) / (maximum(v) - minimum(v))
    end
    return out
end

@inline function normalize(max, min, x)
    return (x - min) / (max - min)
end

function rolling_norm(arr, window, default=NaN)
    len, out = init_window(arr, window, default)
    r_window = arr[1:window]
    s_idx = sortperm(r_window)
    s_window = r_window[s_idx]
    min = s_window[1]
    max = s_window[end]
    out[window] = normalize(min, max, arr[window])
    wm1 = window - 1

    for n in window + 1:len
        v = arr[n]
        p = shift_1!(r_window, wm1)
        r_window[end] = v
        ip = searchsortedlast(s_window, p)
        if ip == 0
            ip = 1
        end
        # find location of new value
        i = searchsortedlast(s_window, v)
        if i == 0
            i = 1
        end
        if i == ip
            s_window[i] = v
        else
            # move elements in between
            if i < ip
                # upwards
                for s in ip:-1:i + 1
                    s_window[s] = s_window[s - 1]
                end
                if s_window[i + 1] < v
                    s_window[i] = s_window[i + 1]
                    s_window[i + 1] = v
                else
                    s_window[i] = v
                end
            else
                # downwards
                for s in ip + 1:i
                    s_window[s - 1] = s_window[s]
                end
                s_window[i] = v
            end
        end
        out[n] = (v - s_window[1]) / (s_window[end] - s_window[1])
    end
    return out
end

function rolling_max(arr, window, default=NaN)
    len, out = init_window(arr, window, default)
    out[window] = mx = maximum(view(arr, 1:window))
    wp1 = window + 1
    wm1 = window - 1
    @inbounds for n in window + 1:len
        if arr[n] >= mx
            out[n] = mx = arr[n]
        else
            i = n - wm1
            mx = arr[n - wm1]
            for w in i:i + wm1
                if arr[w] > mx
                    mx = arr[w]
                end
            end
            out[n] = mx
        end
    end
    return out
end
