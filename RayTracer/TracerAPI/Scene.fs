module Scene

open Camera
open BaseShape
open Light

type Scene =
  | S of shape list * light list * ambientLight * camera * int

let mkScene shapes lights ambientLight camera reflection = S(shapes, lights, ambientLight, camera, reflection)

// recursively casts rays to determine the color a given ray should register
let rec castRay ray scene numReflections =
    let intersects = scene.shapes
                     |> List.collect (fun x -> intersectShape x ray)
                     |> List.filter  (fun x -> fst x > Epsilon)
    match intersects with
    | [] -> Background
    | _  -> let (time, intersection) = List.minBy (fun x -> (fst x))  intersects
            let colorAtIntersection = colorAt intersection scene
            let reflectDir = (ray.direction - 2.0 * norm ((Vector3D.DotProduct(ray.direction, intersection.normal)) * intersection.normal))
            let newRay = { origin = intersection.point; direction = reflectDir }
            match time with
            | _ when time > FadeDistance -> Background
            | _ -> match numReflections with
                   | _ when numReflections < MaxReflections ->
                          ((colorAtIntersection * (1.0-intersection.material.reflectivity)) +
                            ((castRay newRay scene (numReflections+1)) * intersection.material.reflectivity)) *
                            ((FadeDistance-time)/FadeDistance)
                   | _ -> (colorAtIntersection * (1.0-intersection.material.reflectivity))

    // Make ourselves a canvas
    let width = 640
    let height = 480

    // Vertical and horizontal field of view
    let hfov = System.Math.PI/3.2
    let vfov = hfov * float(height)/float(width)

    // Pixel width and height
    let pw = 2.0 * System.Math.Tan(float(hfov/2.0))/float(width)
    let ph = 2.0 * System.Math.Tan(float(vfov/2.0))/float(height)

    // Setting up the UI components
    let mainForm = new Form(Width = width, Height = height, Text = "FRay")
    let box = new PictureBox(BackColor = Color.White, Dock = DockStyle.Fill, SizeMode = PictureBoxSizeMode.CenterImage)
    mainForm.Controls.Add(box)
    let bmp = new Bitmap(width,height)

    // set up the coordinate system
    let n = norm (scene.camera.position - scene.camera.lookAt)
    let u = norm (scene.camera.lookUp * n)
    let v = norm (n * u)
    let vpc = scene.camera.position - n

    // render the scene
    let image = Array.Parallel.init width  (fun x ->
                Array.init height (fun y ->
                    let rayPoint = vpc + float(x-width/2)*pw*u + float(y-height/2)*ph*v
                    let rayDir = norm (rayPoint - scene.camera.position)
                    let ray = { origin = scene.camera.position; direction = rayDir }
                    let color = castRay ray scene 0
                    let (a,r,g,b) = argbFromColor(color)
                    Color.FromArgb(a,r,g,b)))


    // build the bitmap from the rendered scene
    for x in 0..(image.Length-1) do
        for y in 0..(image.[x].Length-1) do
            bmp.SetPixel(x,y,image.[x].[y])

    // save the bitmap to disk.
    bmp.Save("FRayOutput.jpg");
