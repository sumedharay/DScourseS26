using CSV, DataFrames, GLM, HTTP, CategoricalArrays

function main()
    # load Stata auto dataset (i.e. `sysuse auto` in Stata)
    url = "https://tyleransom.github.io/teaching/MetricsLabs/auto.csv"
    auto = CSV.read(HTTP.get(url).body,DataFrame)
    
    # set `rep78` variable to be categorical
    auto.rep78 = categorical(auto.rep78)
    
    # run basic regression (`reg price mpg foreign headroom i.rep78` in Stata)
    @show lm(@formula(price ~ mpg + foreign + headroom + rep78), auto)
    return nothing
    
end 

main()