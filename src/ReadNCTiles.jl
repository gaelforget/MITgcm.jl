

"""
    read_nctiles(fileName,fldName,mygrid)

Read model output from Netcdf / NCTiles file and convert to MeshArray instance.
```
mygrid=GridSpec("LatLonCap")
fileName="nctiles_grid/GRID"
Depth=read_nctiles(fileName,"Depth",mygrid)
hFacC=read_nctiles(fileName,"hFacC",mygrid)
```
"""
function read_nctiles(fileName::String,fldName::String,mygrid::gcmgrid)

if (mygrid.class!="LatLonCap")||(mygrid.ioSize!=[90 1170])
  error("non-llc90 cases not implemented yet");
end;

fileIn=@sprintf("%s.%04d.nc",fileName,1);
x = ncread(fileIn,fldName);
ndims=length(size(x));

#initialize f
if ndims==2;
  f0=Array{Float64}(undef,90,0);
  f00=Array{Float64}(undef,0,90);
elseif ndims==3;
  f0=Array{Float64}(undef,90,0,size(x,3));
  f00=Array{Float64}(undef,0,90,size(x,3));
elseif ndims==4;
  f0=Array{Float64}(undef,90,0,size(x,3),size(x,4));
  f00=Array{Float64}(undef,0,90,size(x,3),size(x,4));
end;
f=[f0,f0,f0,f00,f00];

#fill in f
for ff=1:13;
  #read one tile
  fileIn=@sprintf("%s.%04d.nc",fileName,ff);
  x = ncread(fileIn,fldName);
  #combine tiles
  if ff<=3;
    f[1]=cat(f[1],x;dims=2);
  elseif ff<=6;
    f[2]=cat(f[2],x;dims=2);
  elseif ff==7;
    f[3]=x;
  elseif ff<=10;
    f[4]=cat(f[4],x;dims=1);
  elseif ff<=13;
    f[5]=cat(f[5],x;dims=1);
  end;

end;

fld=MeshArray(mygrid,f)
return fld

end

"""
    findtiles(ni::Int,nj::Int,mygrid::gcmgrid)
    findtiles(ni::Int,nj::Int,grid::String="llc90",GridParentDir="./")

Return a `MeshArray` map of tile indices, `mytiles["tileNo"]`, for tile
size `ni,nj` and extract grid variables accordingly.
"""
function findtiles(ni::Int,nj::Int,mygrid::gcmgrid)
    mytiles = Dict()

    GridVariables=GridLoad(mygrid)

    mytiles["nFaces"]=mygrid.nFaces;
    mytiles["ioSize"]=mygrid.ioSize;

    XC=GridVariables["XC"];
    YC=GridVariables["YC"];
    XC11=similar(XC); YC11=similar(XC);
    XCNINJ=similar(XC); YCNINJ=similar(XC);
    iTile=similar(XC); jTile=similar(XC); tileNo=similar(XC);
    tileCount=0;
    for iF=1:XC11.grid.nFaces
        face_XC=XC.f[iF]; face_YC=YC.f[iF];
        for jj=Int.(1:size(face_XC,2)/nj);
            for ii=Int.(1:size(face_XC,1)/ni);
                tileCount=tileCount+1;
                tmp_i=(1:ni).+ni*(ii-1)
                tmp_j=(1:nj).+nj*(jj-1)
                tmp_XC=face_XC[tmp_i,tmp_j]
                tmp_YC=face_YC[tmp_i,tmp_j]
                XC11.f[iF][tmp_i,tmp_j].=tmp_XC[1,1]
                YC11.f[iF][tmp_i,tmp_j].=tmp_YC[1,1]
                XCNINJ.f[iF][tmp_i,tmp_j].=tmp_XC[end,end]
                YCNINJ.f[iF][tmp_i,tmp_j].=tmp_YC[end,end]
                iTile.f[iF][tmp_i,tmp_j]=collect(1:ni)*ones(Int,1,nj)
                jTile.f[iF][tmp_i,tmp_j]=ones(Int,ni,1)*collect(1:nj)'
                tileNo.f[iF][tmp_i,tmp_j]=tileCount*ones(Int,ni,nj)
            end
        end
    end

    mytiles["tileNo"] = tileNo;
    mytiles["XC"] = XC;
    mytiles["YC"] = YC;
    mytiles["XC11"] = XC11;
    mytiles["YC11"] = YC11;
    mytiles["XCNINJ"] = XCNINJ;
    mytiles["YCNINJ"] = YCNINJ;
    mytiles["iTile"] = iTile;
    mytiles["jTile"] = jTile;

    return mytiles

end

findtiles(ni::Int,nj::Int,GridName::String="llc90",GridParentDir="./") = findtiles(ni,nj,GridSpec(GridName,GridParentDir))
