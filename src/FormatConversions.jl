
"""
    cube2compact(x::Array)

Reshape from e.g. size (192, 32, 5) in cube format to (32, 192, 5) in compact format.
"""
function cube2compact(x::Array)
  s=size(x)
  k=1 .+Tuple(1:length(s))
  n=gcd(s[1],s[2])
  reshape(permutedims(reshape(x,(n,6,n,s[3:end]...)),(1,3,2,k[3:end]...)),(n,n*6,s[3:end]...))
end

"""
    compact2cube(x::Array)

Reshape from e.g. size (32, 192, 5) in cube format to (192, 32, 5) in compact format.
"""
function compact2cube(x::Array)
  s=size(x)
  k=1 .+Tuple(1:length(s))
  n=gcd(s[1],s[2])
  reshape(permutedims(reshape(x,(n,n,6,s[3:end]...)),(1,3,2,k[3:end]...)),(n*6,n,s[3:end]...))
end

"""
MatrixInterp(in::Array{T,N},MTRX,siz) where {T,N}

Interpolate `in` using `MTRX` to grid of size `siz`.
"""
function MatrixInterp(in::Array{T,N},MTRX::SparseMatrixCSC,siz) where {T,N}
#input
l=size(in,1)*size(in,2);
m=size(in,3);
tmp1=reshape(in,l,m)
tmp0=Float64.(.!(isnan.(tmp1)))
tmp1[isnan.(tmp1)].=0.
siz=siz[1],siz[2],m
#matrix product
tmp0=MTRX*tmp0
tmp1=MTRX*tmp1
tmp1=tmp1./tmp0
#this may be redundant:
tmp1[tmp0 .== 0.] .= NaN
#output
out=reshape(tmp1,siz)
m==1 ? out=dropdims(out,dims=3) : nothing
return out
end

## convert2array method:

"""
    convert2array(fld::MeshArray)

Convert MeshArray to Array (or vice versa otherwise)
"""
function convert2array(fld::MeshArray)

if fld.grid.class=="LatLonCap";
    tmp1=cat(fld.f[1],fld.f[2],rotr90(fld.f[4]),rotr90(fld.f[5]);dims=1);
    tmp2=cat(rotl90(fld.f[3]),NaN*fld.f[3],NaN*fld.f[3],NaN*fld.f[3];dims=1);
    arr=cat(tmp1,tmp2;dims=2);
elseif fld.grid.class=="CubeSphere";
    tmp1=cat(fld.f[1],fld.f[2],rotr90(fld.f[4]),rotr90(fld.f[5]);dims=1);
    tmp2=cat(rotl90(fld.f[3]),NaN*fld.f[3],NaN*fld.f[3],NaN*fld.f[3];dims=1);
    tmp0=cat(NaN*fld.f[3],NaN*fld.f[3],NaN*fld.f[3],rotr90(fld.f[6]);dims=1);
    arr=cat(tmp0,tmp1,tmp2;dims=2);
elseif fld.grid.class=="ll";
  arr=fld.f[1];
else;
  error("unknown grTopo case");
end;

return arr;

end

## convert2array ##

function convert2array(fld::Array{T,N},grid::gcmgrid) where {T,N}

grTopo=grid.class
nFaces=grid.nFaces
facesSize=grid.fSize

v1=Array{Array{T,N},1}(undef,nFaces);
N>2 ? error("N>2 case not implemented yet") : nothing

if grTopo=="LatLonCap";
    (n1,n2)=facesSize[1];
    v1[1]=fld[1:n1,1:n2];
    v1[2]=fld[n1+1:n1*2,1:n2];
    v1[3]=rotr90(fld[1:n1,n2+1:n2+n1]);
    v1[4]=rotl90(fld[n1*2+1:n1*3,1:n2]);
    v1[5]=rotl90(fld[n1*3+1:n1*4,1:n2]);
elseif grTopo=="CubeSphere";
    (n1,n2)=facesSize[1];
    v1[1]=fld[1:n1,n1+1:n1+n2];
    v1[2]=fld[n1+1:2*n1,n1+1:n1+n2];
    v1[3]=rotr90(fld[1:n1,n1+n2+1:n2+n1*2]);
    v1[4]=rotl90(fld[n1*2+1:n1*3,n1+1:n1+n2]);
    v1[5]=rotl90(fld[n1*3+1:n1*4,n1+1:n1+n2]);
    v1[6]=rotl90(fld[n1*3+1:n1*4,1:n1]);
elseif grTopo=="ll";
    v1[1]=fld;
else;
  error("unknown grTopo case");
end;

MeshArray(grid,v1);

end

## convert2gcmfaces method:

"""
    convert2gcmfaces(fld::MeshArray)

Convert mitgcm output to MeshArray (or vice versa otherwise)
"""
function convert2gcmfaces(fld::MeshArray)

    grTopo=fld.grid.class
    nFaces=fld.grid.nFaces
    (n1,n2)=fld.grid.ioSize
    facesSize=fld.grid.fSize

    aa=0;bb=0;
    for iFace=1:nFaces;
        aa=aa+prod(facesSize[iFace]);
        bb=bb+prod(size(fld.f[iFace]));
    end;
    n3=Int64(bb/aa);
    v11=NaN*zeros(aa,n3);

    i0=0; i1=0;
    for iFace=1:nFaces;
        i0=i1+1;
        nn=facesSize[iFace][1];
        mm=facesSize[iFace][2];
        i1=i1+nn*mm;
        v11[i0:i1,:]=reshape(fld.f[iFace],(nn*mm,n3));
    end;
    v1=reshape(v11,(n1,n2,n3));
    n3==1 ? v1=dropdims(v1,dims=3) : nothing

    return v1;

end

## convert2gcmfaces ##

function convert2gcmfaces(fld::Array,grid::gcmgrid)

grTopo=grid.class
nFaces=grid.nFaces
(n1,n2)=grid.ioSize
facesSize=grid.fSize

n3=Int64(prod(size(fld))/n1/n2);

v0=reshape(fld,(n1*n2,n3));
v1=Array{Array{eltype(fld),ndims(fld)}}(undef,nFaces);
i0=0; i1=0;
for iFace=1:nFaces
  i0=i1+1;
  nn=facesSize[iFace][1]; mm=facesSize[iFace][2];
  i1=i1+nn*mm;
  if n3>1;
    v1[iFace]=reshape(v0[i0:i1,:],(nn,mm,n3));
  else;
    v1[iFace]=reshape(v0[i0:i1,:],(nn,mm));
  end;
end

MeshArray(grid,v1);

end
