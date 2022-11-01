# Some function from tsfresh python library ...

using IterTools; const global it = IterTools
using Statistics
using Base
using SplitApplyCombine

AN = Array{<: Number}


function _get_length_sequences_where(arr::AN)
    if (length(arr) == 0)
        return 0
    else
        res = [length(i) for i in it.groupby(x -> x[1], arr) if i[1] == 1]
        return (length(res) > 0) ? res : [0]
    end
end

function abs_energy(arr::AN)
    return arr .* arr
end

function absolute_sum_of_changes(arr::AN)
    return sum(abs.(diff(arr)))
end

function longest_strike_below_mean(arr::AN)
    return maximum(size(arr) .> 0) ? _get_length_sequences_where(arr .< mean(arr)) : 0
end

function longest_strike_above_mean(arr::AN)
    return maximum(size(arr) .> 0) ? _get_length_sequences_where(arr .> mean(arr)) : 0
end

function count_above_mean(arr::AN)
    return size(arr[ifelse.(arr .> mean(arr))])
end

function count_below_mean(arr::AN)
    return size(arr[ifelse.(arr .< mean(arr))])
end

function last_location_of_maximum(arr::AN)
    # NOTE: julia indexes base 1, argmax returns an index
    return (length(arr) > 0) ? 1.0 - (argmax(view(arr, length(arr):-1:1)) - 1) / length(arr) : NaN
end

function first_location_of_maximum(arr::AN)
    return (length(arr) > 0) ? (argmax(arr) - 1) / length(arr) : NaN
end

function last_location_of_minimum(arr::AN)
    return (length(arr) > 0) ? 1.0 - (argmin(view(arr, length(arr):-1:1)) - 1) / length(arr) : NaN
end

function first_location_of_minimum(arr::AN)
    return (length(arr) > 0) ? (argmin(arr) - 1) / length(arr) : NaN
end

function percentage_of_reoccurring_values_to_all_values(arr::AN)
    if length(arr) == 0
        return NaN
    end
    recur = 0
    n = 0
    for v in groupcount(arr)
        recur += v > 1
        n += 1
    end
    return recur / n
end

function percentage_of_reoccurring_datapoints_to_all_datapoints(arr::AN)
    if length(arr) == 0
        return NaN
    end
    # groupcount returns a dict, and iteration defaults over values (Dictionaries.jl)
    recur = 0
    for v in groupcount(arr)
        if v > 1
            recur += v
        end
    end
    return recur / length(arr)
end

function sum_of_reoccurring_values(arr::AN)
    recur = 0
    g = groupcount(arr)
    for (k, v) in zip(keys(g), g)
        if v > 1
            recur += k
        end
    end
    return recur
end


function sum_of_reoccurring_values(arr::AN)
    recur = 0
    g = groupcount(arr)
    for (k, v) in zip(keys(g), g)
        if v > 1
            recur += k
        end
    end
    return recur
end


function sum_of_reoccurring_data_points(arr::AN)
    recur = 0
    g = groupcount(arr)
    for (k, v) in zip(keys(g), g)
        if v > 1
            recur += k * v
        end
    end
    return recur
end


function ratio_value_number_to_time_series_length(arr::AN)
    return (length(arr) == 0 ) ? NaN : length(Set(arr)) / length(arr)
end
