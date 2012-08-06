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
        let array = Array2D.zeroCreate<'a option [,] option [,] option [,] option> w h
        with 
            member internal this.Array 
                with get() = array
            member this.Item
                with get(c, r) =
                        validate (c, r)
                        let l3 = array.[(c >>> (3 * logw)) &&& mw, (r >>> (3 * logh)) &&& mh]
                        match l3 with
                            | None -> None
                            | Some(value) ->
                                let l2 = value.[(c >>> (2 * logw)) &&& mw, (r >>> (2 * logh)) &&& mh]
                                match l2 with
                                    | None -> None
                                    | Some(value) ->
                                        let l1 = value.[(c >>> logw) &&& mw, (r >>> logh) &&& mh]
                                        match l1 with
                                            | None -> None
                                            | Some (value) -> value.[c &&& mw, r &&& mh]

                and set(c, r) value =
                        validate (c, r)
                        let l3() = array.[(c >>> (3 * logw)) &&& mw, (r >>> (3 * logh)) &&& mh]
                        if l3().IsNone then
                            array.[(c >>> (3 * logw)) &&& mw, (r >>> (3 * logh)) &&& mh] <- Some(Array2D.zeroCreate<'a option [,] option [,] option> w h)
                        let l2() = l3().Value.[(c >>> (2 * logw)) &&& mw, (r >>> (2 * logh)) &&& mh]
                        if l2().IsNone then
                            l3().Value.[(c >>> (2 * logw)) &&& mw, (r >>> (2 * logh)) &&& mh] <- Some(Array2D.zeroCreate<'a option [,] option> w h)
                        let l1() = l2().Value.[(c >>> logw) &&& mw, (r >>> logh) &&& mh]
                        if l1().IsNone then
                            l2().Value.[(c >>> logw) &&& mw, (r >>> logh) &&& mh] <- Some(Array2D.zeroCreate<'a option> w h)
                        l1().Value.[c &&& mw, r &&& mh] <- value

    let create<'a> = quadtree<'a>()

    let get (c, r) (quadtree : quadtree<'a>) =
        quadtree.[c, r]
        
    let set (c, r) value (quadtree : quadtree<'a>) =
        quadtree.[c, r] <- value

    let iter f (quadtree : quadtree<'a>) =
        quadtree.Array |> Array2D.iter (fun l3 ->
            if l3.IsSome then
                l3.Value |> Array2D.iter (fun l2 ->
                    if l2.IsSome then
                        l2.Value |> Array2D.iter (fun l1 ->
                            if l1.IsSome then
                                l1.Value |> Array2D.iter (fun l0 ->
                                    match l0 with
                                        | Some(value) -> f value
                                        | None -> ()))))

    // skulle den hedde map eller apply eller??? 
    let mapInPlace f (quadtree : quadtree<'a>) =
        quadtree.Array |> Array2D.iteri (fun c3 r3 l3 ->
            if l3.IsSome then
                l3.Value |> Array2D.iteri (fun c2 r2 l2 ->
                    if l2.IsSome then
                        l2.Value |> Array2D.iteri (fun c1 r1 l1 ->
                            if l1.IsSome then
                                l1.Value |> Array2D.iteri (fun c0 r0 l0 ->
                                    match l0 with
                                        | Some(value) -> quadtree.Array.[c3, r3].Value.[c3, r2].Value.[c1, r1].Value.[c0, r0] <- Some(f value)
                                        | None -> ()))))
    
    // mangler en mapi, en iteri og en filter funktion.
                                        
    let toSeq (quadtree : quadtree<'a>) =
        seq {
            for c in [ 0 .. mw ] do
                for r in [ 0 .. mh ] do
                    let l = quadtree.Array.[c, r]
                    if l.IsSome then
                        for c in [ 0 .. mw ] do
                            for r in [ 0 .. mh ] do
                                let l = l.Value.[c, r]
                                if l.IsSome then
                                    for c in [ 0 .. mw ] do
                                        for r in [ 0 .. mh ] do
                                            let l = l.Value.[c, r]
                                            if l.IsSome then
                                                for c in [ 0 .. mw ] do
                                                    for r in [ 0 .. mh ] do
                                                        let l = l.Value.[c, r]
                                                        match l with
                                                            | Some(value) -> yield value
                                                            | None -> ()
        }