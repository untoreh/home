module CramersV
using FreqTables
using ..utils
using ..transformations

# REF: HypothesisTests.jl (ChisqTest)
function chisq(x; lambda=1.0)
    """ Returns the chi square, and the overall sum of a matrix """
    nrows, ncols = size(x)
    @assert nrows > 1 && ncols > 1

    rowsums = sum(x, dims=2)
    colsums = sum(x, dims=1)
    n = sum(rowsums)

    xhat = rowsums * colsums / n

    stat = 0
    if lambda == 0
        for i in 1:length(x)
            stat += x[i] * (log(x[i]) - log(xhat[i]))
        end
        stat *= 2
    elseif lambda == -1
        for i in 1:length(x)
            stat += xhat[i] * (log(xhat[i]) - log(x[i]))
        end
        stat *= 2
    else
        for i in 1:length(x)
            stat += x[i] * ((x[i] / xhat[i])^lambda - 1)
        end
        stat *= 2 / (lambda * (lambda + 1))
    end
    return stat, n
end

function _get_chi(x, y, digits, factorize)
    if factorize
        # factorize
        x = x |> unit_range |> v -> round.(v; digits=digits)
        y = y |> unit_range |> v -> round.(v; digits=digits)
    end
    # cross table
    cmat = freqtable(x, y)

    chi2, n = chisq(cmat)
    phi2 = chi2 / n
    r, k = size(cmat)
    return chi2, phi2, n, r, k
end

function _correct(phi2, n, r, k)
    phi2_corr = max(0, phi2 - (k - 1) * (r - 1) / (n - 1))
    r_corr = r - (r - 1)^2 / (n - 1)
    k_corr = k - (k - 1)^2 / (n - 1)
    return phi2_corr, r_corr, k_corr
end

function cramv(x, y; digits=1, factorize=true)
    """ Cramer's V coefficient with correction """
    chi2, phi2, n, r, k = _get_chi(x, y, digits, factorize)

    phi2, r, k = _correct(phi2, n, r, k)

    √(phi2 /
      min((k - 1), (r - 1)))
end

# REF: https://github.com/MavericksDS/pycorr/blob/master/pycorrcat/pycorrcat.py
function tschu(x, y; digits=1, factorize=true)
    """ Tschuprow's_T coefficient with correction """
    chi2, phi2, n, r, k = _get_chi(x, y, digits, factorize)

    phi2, r, k = _correct(phi2, n, r, k)
    √(phi2 /
      √((r - 1) * (k - 1)))
end

end
