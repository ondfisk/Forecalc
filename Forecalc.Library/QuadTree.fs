namespace Forecalc.Library

module QuadTree =

    let private logw = 4
    let private w = 1 <<< logw
    let private mw = ~~~((~~~0) <<< logw)
    let private logh = 5
    let private h = 1 <<< logh
    let private mh = ~~~((~~~0) <<< logh)

    let private maxw = (w <<< (3 * logw)) - 1
    let private maxh = (h <<< (3 * logh)) - 1 

    let private validate (c, r) =
        if c < 0 || c > maxw then
            failwithf "c must be between 0 and %i" maxw
        if r < 0 || r > maxh then
            failwithf "r must be between 0 and %i" maxh

    type quadtree<'a> internal() =
        let item = Array2D.zeroCreate<'a option [,] [,] [,]> w h
        with 
            member internal this.Item 
                with get() = item

    let create<'a> = quadtree<'a>()

    let get (c, r) (quadtree : quadtree<'a>) =
        validate (c, r)
        let l3 = quadtree.Item.[(c >>> (3 * logw)) &&& mw, (r >>> (3 * logh)) &&& mh]
        if l3 = null then 
            None 
        else
            let l2 = l3.[(c >>> (2 * logw)) &&& mw, (r >>> (2 * logh)) &&& mh]
            if l2 = null then
                None
            else
                let l1 = l2.[(c >>> logw) &&& mw, (r >>> logh) &&& mh]
                if l1 = null then
                    None
                else
                    l1.[c &&& mw, r &&& mh]
        
    let set (c, r) value (quadtree : quadtree<'a>) =
        validate (c, r)
        let l3() = quadtree.Item.[(c >>> (3 * logw)) &&& mw, (r >>> (3 * logh)) &&& mh]
        if l3() = null then
            quadtree.Item.[(c >>> (3 * logw)) &&& mw, (r >>> (3 * logh)) &&& mh] <- Array2D.zeroCreate<'a option [,] [,]> w h
        let l2() = l3().[(c >>> (2 * logw)) &&& mw, (r >>> (2 * logh)) &&& mh]
        if l2() = null then
            l3().[(c >>> (2 * logw)) &&& mw, (r >>> (2 * logh)) &&& mh] <- Array2D.zeroCreate<'a option [,]> w h
        let l1() = l2().[(c >>> logw) &&& mw, (r >>> logh) &&& mh]
        if l1() = null then
            l2().[(c >>> logw) &&& mw, (r >>> logh) &&& mh] <- Array2D.zeroCreate<'a option> w h
        l1().[c &&& mw, r &&& mh] <- Some(value)

    let iter f (quadtree : 'a option [,] [,] [,] [,]) =
        quadtree |> Array2D.iter (fun l3 ->
            if l3 <> null then
                l3 |> Array2D.iter (fun l2 ->
                    if l2 <> null then
                        l2 |> Array2D.iter (fun l1 ->
                            if l1 <> null then
                                l1 |> Array2D.iter (fun l0 ->
                                    match l0 with
                                        | Some(value) -> f value
                                        | None -> ()))))

    // skulle den hedde map eller apply eller??? 
    let mapInPlace f (quadtree : quadtree<'a>) =
        quadtree.Item |> Array2D.iteri (fun c3 r3 l3 ->
            if l3 <> null then
                l3 |> Array2D.iteri (fun c2 r2 l2 ->
                    if l2 <> null then
                        l2 |> Array2D.iteri (fun c1 r1 l1 ->
                            if l1 <> null then
                                l1 |> Array2D.iteri (fun c0 r0 l0 ->
                                    match l0 with
                                        | Some(value) -> quadtree.Item.[c3, r3].[c3, r2].[c1, r1].[c0, r0] <- Some(f value)
                                        | None -> ()))))

    let toSeq (quadtree : quadtree<'a>) =
        seq {
            for c in [ 0 .. mw ] do
                for r in [ 0 .. mh ] do
                    let l = quadtree.Item.[c, r]
                    if l <> null then
                        for c in [ 0 .. mw ] do
                            for r in [ 0 .. mh ] do
                                let l = l.[c, r]
                                if l <> null then
                                    for c in [ 0 .. mw ] do
                                        for r in [ 0 .. mh ] do
                                            let l = l.[c, r]
                                            if l <> null then
                                                for c in [ 0 .. mw ] do
                                                    for r in [ 0 .. mh ] do
                                                        let l = l.[c, r]
                                                        match l with
                                                            | Some(value) -> yield value
                                                            | None -> ()
        }