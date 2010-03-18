for i=1:42
    filename = sprintf('d:/temp/krueckau/A%04d.2d',i);
    [points, arcs, elements] = loadBce2d(filename,'VA');
    handle = plotBce2d(points(:,[1 2 7]), arcs, elements);
    faces = get(handle,'Faces');
    vertices = get(handle,'Vertices');
    saveMeshAsVtk(sprintf('d:/temp/krueckau/A%04d',i),vertices,faces);
end