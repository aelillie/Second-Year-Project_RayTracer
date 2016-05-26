open TracerTestSuite

let renderAll toScreen =
  Shapes.render toScreen
  AffineTransformations.render toScreen
  ImplicitSurfaces.render toScreen
  Meshes.render toScreen
  Texture.render toScreen
  Light.render toScreen
  CSG.render toScreen


[<EntryPoint>]
let main argv =
    Util.init();
    renderAll false;
    Util.finalize();
    0 // return an integer exit code