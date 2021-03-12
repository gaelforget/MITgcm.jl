### A Pluto.jl notebook ###
# v0.12.21

using Markdown
using InteractiveUtils

# ╔═╡ b96456b4-8267-11eb-043a-07fc1ee69d34
begin
	using JLD2, FileIO, DataFrames, GLMakie
    AbstractPlotting.inline!(false) #may need true for VS code & jupyter?
	true
end

# ╔═╡ ae030b46-826a-11eb-093e-810bd32a356d
begin
	I=load("HS94.jld2")["I"] #one can generate HS94.jld2 using examples/HS94_particles.jl
	rec_by_t=groupby(I.record, :t)
	true
end

# ╔═╡ 3a2102ba-8269-11eb-14a7-af914ab4fb16
begin
	time = Node(1)
    lo = @lift(rec_by_t[$time].lon)
    la = @lift(rec_by_t[$time].lat)

    fi1,ax1 = scatter(lo, la, markersize = 4.0, color=:red, strokewidth = 0.0)
	xlims!(ax1,(-180.,180.))
	ylims!(ax1,(-90.,90.))

	fi1
end

# ╔═╡ 3a71aab6-826a-11eb-3858-51651dd387dc
begin
	record(fi1, "HS94_Makie.mp4", 1:length(rec_by_t); framerate = 20) do t
        time[] = t
    end
	save("HS94_Makie.png",fi1)
end

# ╔═╡ Cell order:
# ╠═b96456b4-8267-11eb-043a-07fc1ee69d34
# ╠═ae030b46-826a-11eb-093e-810bd32a356d
# ╠═3a2102ba-8269-11eb-14a7-af914ab4fb16
# ╠═3a71aab6-826a-11eb-3858-51651dd387dc
