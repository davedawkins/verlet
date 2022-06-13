module App

open Browser.Dom
open Browser.Types

let mutable loggingEnabled = false
let mutable loggingCount = 0

type Stick = {
        P1 : int
        P2 : int
        Length : float
        Visible : bool
        Flexible : bool
    }

type Message =
    | Frame
    | Pinned of int * float * float

let distance x1 y1 x2 y2 =
    System.Math.Sqrt( (x2-x1) * (x2-x1) + (y2-y1) * (y2-y1) )

type Vector2( x : float, y : float ) =
    member __.X = x
    member __.Y = y
    member __.LengthSq =
        (x * x) + (y * y)
    member __.Length =
        System.Math.Sqrt((x * x) + (y * y))
    member __.Normalize() =
        let len = __.Length
        Vector2( x / len, y / len )
    member __.Dot( other : Vector2 ) =
        x * other.X + y * other.Y
    static member (+) (a : Vector2, b: Vector2) =
        Vector2( a.X + b.X, a.Y + b.Y )
    static member (-) (a : Vector2, b: Vector2) =
        Vector2( a.X - b.X, a.Y - b.Y )
    static member (*) (a : Vector2, n : float) =
        Vector2( a.X * n, a.Y * n )
    static member (~-) (a : Vector2 ) =
        Vector2( -a.X, -a.Y )

type Particle = {
        Id : int
        PrevXPos : float
        PrevYPos : float
        XPos : float
        YPos : float
        Fixed : bool
        Visible : bool
        Gravity: bool
        Vertical : bool
        Color : string
    }
    with
        member __.Contains( x : float, y : float ) =
            distance x y (__.XPos) (__.YPos) <= 4

        member __.Velocity =
            Vector2( __.XPos - __.PrevXPos, __.YPos - __.PrevYPos)

        member __.Speed =
            __.Velocity.Length

        member __.Energy =
            let v = __.Speed
            0.5 * v * v

        static member Create(
            id : int,
            xpos : float,
            ypos : float,
            speedx : float option,
            speedy : float option,
            isFixed : bool option,
            isVisible : bool option,
            gravity : bool option,
            vertical : bool option,
            color : string option
            ) =
            {
                Id = id
                PrevXPos = xpos - (speedx |> Option.defaultValue 0.0)
                PrevYPos = ypos - (speedy |> Option.defaultValue 0.0)
                XPos = xpos
                YPos = ypos
                Fixed = isFixed |> Option.defaultValue false
                Visible = isVisible |> Option.defaultValue true
                Gravity = gravity |> Option.defaultValue true
                Vertical = vertical |> Option.defaultValue false
                Color = color |> Option.defaultValue "black"
            }

type IdFactory() =
    let mutable _nextId = 0
    do
        console.log("New factory")
    member _.Next() =
        let n = _nextId
        _nextId <- n + 1
        n
    member _.PeekNext() = _nextId

let private _ParticleIds = IdFactory()
let private _ShapeIds = IdFactory()

type Shape =
    {
        Id : int
        Particles : Map<int,Particle>
        Sticks : Stick list
    }
    with
    static member ParticleIds = _ParticleIds
    static member ShapeIds = _ShapeIds
    static member Empty =
        {
            Id = Shape.ShapeIds.Next()
            Particles = Map.empty
            Sticks = []
        }
    member __.AddParticle( p : Particle ) =
        { __ with
            Particles = __.Particles.Add( p.Id, p )
        }
    member __.AddParticle( xpos : float, ypos : float, ?vx : float, ?vy : float, ?isFixed : bool, ?isVisible : bool, ?gravity : bool, ?vertical : bool, ?color : string ) =
        __.AddParticle(Particle.Create(Shape.ParticleIds.Next(), xpos, ypos, vx, vy, isFixed, isVisible, gravity, vertical, color))
    member __.AddStick( id1 : int, id2 : int, len : float, isVisible : bool, isFlexible : bool ) =
        { __ with
            Sticks = __.Sticks @ [ { P1=id1; P2=id2; Length = len; Visible = isVisible; Flexible = isFlexible } ]
        }
    member __.AddStick( id1 : int, id2 : int, ?isVisible : bool, ?isFlexible : bool ) =
        let p1,p2 = __.Particles[id1], __.Particles[id2]
        let len = distance (p1.XPos) (p1.YPos) (p2.XPos) (p2.YPos)
        __.AddStick( id1, id2, len, isVisible |> Option.defaultValue true, isFlexible |> Option.defaultValue false )
    member __.GetParticle (id:int) =
        __.Particles[id]

type Model =
    {
        Pinned : int * float * float
        Shapes : Shape list
    }
    member __.FindShapeWithParticle (id : int) =
        __.Shapes |> List.tryFind (fun s -> s.Particles.ContainsKey(id))
    member __.ReplaceShape (s : Shape) =
        { __ with
            Shapes = __.Shapes |> List.map (fun x -> if x.Id = s.Id then s else x ) }
    member __.AllParticles =
        __.Shapes |> List.collect (fun s -> s.Particles.Values |> Seq.toList)

    member __.Energy =
        __.AllParticles |> List.sumBy (fun p -> p.Energy)

let rnd (min:float) (max:float) =
    min + (max - min) * Fable.Core.JS.Math.random()

let rndColor() =
    $"hsla({rnd 0.0 360.0} 50% 50% / 1.0)"

let oddNeg (n) =
    (2 * (n % 2)) - 1

let makeParticles (n:int) =
    let rp (s:Shape) (i : int) =
        let x = rnd 10.0 300.00 //300.0 - 50.0 * (float i)
        let y = rnd 10.0 200.0 //100.0 + (float i * 4.0)
        let vx = rnd -2.0 8.0 // float (oddNeg i) * -8.0// rnd 1.0 4.0
        let vy = rnd -2.0 8.0 // 0.0 // rnd -1.0 1.0
        s.AddParticle( x, y, vx, vy, false, true, false, false, rndColor() )

    let shape = Shape.Empty
    [1..n] |> List.fold rp shape

let makeBox (posx : float) (posy:float) (size : float) (vx : float) (vy : float) =
    let shape = Shape.Empty

    let firstId = Shape.ParticleIds.PeekNext()

    let x1, y1 = posx - size/2.0, posy - size/2.0
    let x2, y2 = posx + size/2.0, posy + size/2.0

    shape
        .AddParticle( x1, y1,  vx, vy, false, false )
        .AddParticle( x2, y1,  vx, vy, false, false )
        .AddParticle( x2, y2,  vx, vy, false, false )
        .AddParticle( x1, y2,  vx, vy, false, false )
        .AddStick( firstId + 0, firstId + 1 )
        .AddStick( firstId + 1, firstId + 2 )
        .AddStick( firstId + 2, firstId + 3 )
        .AddStick( firstId + 3, firstId + 0 )
        .AddStick( firstId + 0, firstId + 2, false )
        .AddStick( firstId + 1, firstId + 3, false )

type ChainFixing =
    | NoFixings
    | FixEndsOnly
    | FixAll

let addChain (x1:float) (y1:float) (x2:float) (y2:float) (len:float) (numLinks:int) (fixing : ChainFixing) (shape : Shape) =
    let id0 = Shape.ParticleIds.PeekNext()
    let dx = (x2 - x1) / float numLinks
    let dy = (y2 - y1) / float numLinks
    let stickLen = len / float numLinks

    let addLink (s : Shape) n =
        let isFixed =
            match fixing with
            | NoFixings -> false
            | FixAll -> true
            | FixEndsOnly -> n = numLinks
        s.AddParticle(
            x1 + (float n) * dx, y1 + (float n) * dy,
            0.0, 0.0,
            isFixed, true, true, false
        ).AddStick( id0 + n-1, id0 + n, stickLen, true, true )

    let addLinks (shape : Shape) =
        [1..numLinks]
        |> List.fold addLink shape

    let fixFirst = fixing <> NoFixings

    shape
        .AddParticle( x1, y1, 0.0, 0.0, fixFirst, true, true, false )
        |> addLinks

let makeChain (x1:float) (y1:float) (x2:float) (y2:float) (len:float) (numLinks:int) =
    Shape.Empty |> addChain x1 y1 x2 y2 len numLinks FixEndsOnly

let makeNet (x1:float) (y1:float) (x2:float) (y2:float) (len:float) (numLinks:int) (height : float) (numHLinks : int) =
    let dh = height / float numHLinks

    let addVert (shape : Shape) n =
        shape.AddStick(
            n + Shape.ParticleIds.PeekNext() - (numLinks+1),
            n + Shape.ParticleIds.PeekNext() - (numLinks+1) * 2,
            dh,
            true, true )

    let addVerts shape =
        [0..numLinks]
        |> List.fold addVert shape

    let addChainRow (shape : Shape) (n : int) =
        shape
        |> addChain x1 (y1 + float n * dh) x2 (y2 + float n * dh) len numLinks NoFixings
        |> addVerts

    [1..numHLinks-1]
    |> List.fold addChainRow (Shape.Empty |> addChain x1 y1 x2 y2 len numLinks FixEndsOnly)

let init (cvs : HTMLCanvasElement) () =

    cvs.height <- cvs.clientHeight
    cvs.width <- cvs.clientWidth

    {
        Pinned = -1,0,0
        Shapes = [
            makeBox 100.0 100.00 50.0 8.0 0.0
            makeBox 200.0 100.00 50.0 0.0 -4.0
            makeParticles 32
            //makeChain 50.0 10.0 350.0 10.0 300 8
            //makeNet 5.0 10.0 195.0 10.0 250 16 100 4
        ]
    }

let initEventHandlers (cvs : HTMLCanvasElement) (model : unit -> Model) (dispatch: Message -> unit) =
    let loggingB = document.querySelector("#logging") :?> HTMLButtonElement

    loggingB.onclick <- fun e ->
        loggingEnabled <- not loggingEnabled

    cvs.onmousedown <- fun e ->
        let p =
            model().Shapes
            |> List.map (fun s -> s.Particles.Values |> List.ofSeq |> List.map (fun x -> s,x))
            |> List.concat
            |> List.tryFind (fun (_,p:Particle) -> p.Contains(e.offsetX,e.offsetY))
        match p with
        | None ->
            ()
        | Some (_,x) ->
            Pinned (x.Id,e.offsetX,e.offsetY) |> dispatch

    cvs.onmousemove <- fun e ->
        let m = model()
        let (pinnedId,_,_) = m.Pinned
        if (pinnedId >= 0) then
            Pinned (pinnedId,e.offsetX,e.offsetY) |> dispatch

    cvs.onmouseup <- fun e ->
        dispatch (Pinned (-1,0.0,0.0))

let view (en : HTMLDivElement) (cvs : HTMLCanvasElement) ( m : Model ) =
    let ctx = cvs.getContext_2d()
    let pinnedId,_,_ = m.Pinned

    ctx.clearRect(0.0, 0.0, cvs.width, cvs.height)

    ctx.lineWidth <- 1.0
    ctx.fillStyle <- Fable.Core.U3.Case1 "black"
    ctx.strokeStyle <- Fable.Core.U3.Case1 "black"

    for shape in m.Shapes do
        for p in shape.Particles.Values |> Seq.filter (fun p -> p.Visible) do
            ctx.beginPath()
            ctx.fillStyle <-
                if pinnedId = p.Id then
                    Fable.Core.U3.Case1 "red"
                else
                    Fable.Core.U3.Case1 p.Color
            ctx.arc(p.XPos, p.YPos, 4.0, 0.0, System.Math.PI * 2.0)
            ctx.fill()

    for shape in m.Shapes do
        ctx.beginPath()
        for s in shape.Sticks |> List.filter (fun s -> s.Visible) do
            let p1, p2 =
                shape.Particles[s.P1],
                shape.Particles[s.P2]
            ctx.moveTo( p1.XPos, p1.YPos )
            ctx.lineTo( p2.XPos, p2.YPos )
        ctx.stroke()

    let totalE = m.Energy
    en.innerText <- string totalE

let updateFrame (cvs : HTMLCanvasElement) ( m : Model ) =
    let w = cvs.width
    let h = cvs.height
    let friction = 1.0 //0.999
    let gravity = 0.5
    let bounce = 1.0 // 0.999

    let updatePosition (p : Particle) =
        let vx = friction * (p.XPos - p.PrevXPos)
        let vy = friction * (p.YPos - p.PrevYPos)

        { p with
            PrevXPos = p.XPos
            PrevYPos = p.YPos
            XPos = if (p.Vertical) then p.XPos else p.XPos + vx
            YPos = p.YPos + vy + (if p.Gravity then gravity else 0)
            }

    let updateParticle p =
        if p.Fixed then
            p.Id, p
        else
            p.Id, updatePosition p

    let bounceParticle p =
        let mutable px = p.PrevXPos
        let mutable py = p.PrevYPos

        let mutable x = p.XPos
        let mutable y = p.YPos

        let vx = (x - px) * bounce
        let vy = (y - py) * bounce

        let mutable bounced = false

        if x < 0 then
            x <- 0
            px <- x + vx
            py <- y - vy
            bounced <- true

        if y < 0 then
            y <- 0
            py <- y + vy
            px <- x - vx
            bounced <- true

        if x > w then
            x <- w
            px <- w + vx
            py <- y - vy
            bounced <- true

        if y > h then
            y <- h
            py <- h + vy
            px <- x - vx
            bounced <- true

        p.Id,
        if not bounced then p else
            { p with
                PrevXPos = if p.Vertical then p.PrevXPos else px
                PrevYPos = py
                XPos = if p.Vertical then p.PrevXPos else x
                YPos = y
            }

    let updateStick (particles : Map<int,Particle>) (stick : Stick) =
        let p1 = particles[stick.P1]
        let p2 = particles[stick.P2]
        let dx = p2.XPos - p1.XPos
        let dy = p2.YPos - p1.YPos
        let d = System.Math.Sqrt( dx*dx + dy*dy )

        let e = (d - stick.Length) / d / 2.0

        let p1' =
            if p1.Fixed || (stick.Flexible && e < 0) then p1 else
            {
                p1 with
                    XPos = if (p1.Vertical) then p1.XPos else p1.XPos + (dx * e)
                    YPos = p1.YPos + (dy * e)
            }

        let p2' =
            if p2.Fixed || (stick.Flexible && e < 0) then p2 else
            {
                p2 with
                    XPos = if (p2.Vertical) then p2.XPos else p2.XPos - (dx * e)
                    YPos = p2.YPos - (dy * e)
            }

        particles
            .Add( p1.Id, p1')
            .Add( p2.Id, p2')

    let updateParticles (particles : Map<int,Particle>) =
        particles.Values
        |> Seq.map updateParticle
        |> Map.ofSeq

    let bounceParticles (particles : Map<int,Particle>) =
        particles.Values
        |> Seq.map bounceParticle
        |> Map.ofSeq

    let updateSticks sticks particles =
        sticks |> List.fold updateStick particles

    // [ 1; 2; 3 ]
    // [ 1,2; 1,3; 2,3 ]
    let comb(list : 'T list) : ('T*'T) list =
        let rec comb' results list =
            match list with
            | [] -> results
            | x::xs -> (xs |> List.map (fun x' -> x,x')) @ comb' results xs
        comb' [] list

    //  (p1) ---->  <---- (p2)
    //   (n) <----
    let collideP1 (e: float) (p1:Particle) (p2:Particle) =
        let dx = p2.XPos - p1.XPos
        let dy = p2.YPos - p1.YPos

        let c = Vector2( p1.XPos + (dx * e), p1.YPos + (dy * e) )

        // Reflection normal for p1
        let n = - (Vector2(dx,dy).Normalize()) // From p2 to p1

        let v2 = Vector2( p2.XPos - p2.PrevXPos, p2.YPos - p2.PrevYPos )
        let v2_speed_contrib = v2.Dot(n);

        let v1 = Vector2( p1.XPos - p1.PrevXPos, p1.YPos - p1.PrevYPos )
        let v1_speed_contrib = v1.Dot(n);

        // Cnrrent position
        let prev =
                let v2n = n * v2_speed_contrib
                let v1n = n * v1_speed_contrib
                let v1' = v1 + v2n - v1n
                c - v1' // prev

        if loggingEnabled then
            //loggingCount <- loggingCount + 1
            if loggingCount > 1 then
                loggingEnabled <- false

            console.log("-----")
            console.log("dx", dx)
            console.log("dy", dy)
            console.log("n", n)
            console.log("v2", v2)
            console.log("v2_speed_contrib", v2_speed_contrib)
            console.log("c",c)
            console.log("prev",prev)
        {
            p1 with
                XPos = c.X
                YPos = c.Y
                PrevXPos = prev.X
                PrevYPos = prev.Y
            }

    let collidePair (particles : Map<int,Particle>) ((p1,p2) : Particle * Particle) =
        let dx = p2.XPos - p1.XPos
        let dy = p2.YPos - p1.YPos
        let d = System.Math.Sqrt( dx*dx + dy*dy )
        let len = 8.0
        let e = (d - len) / d / 2.0

        if d > len then
            particles
        else

            let p1' = collideP1 e p1 p2
            let p2' = collideP1 e p2 p1

            particles
                .Add( p1.Id, p1')
                .Add( p2.Id, p2')

    let collideParticles (particles : Map<int,Particle>) =
        let pairs = particles.Values |> Seq.toList |> comb

        pairs |> List.fold collidePair particles

    let applyConstraints (n : int) sticks particles =
        let applyC p =
            p
            |> updateSticks sticks
            |> bounceParticles
            |> collideParticles

        [1..n] |> List.fold (fun p _ -> applyC p) particles

    let updateShape shape =
        { shape with
            Particles =
                shape.Particles
                |> updateParticles
                |> applyConstraints 50 (shape.Sticks)
        }

    {  m with
        Shapes = m.Shapes |> List.map updateShape
    }

let pin (model:Model) =
    let (id,x,y) = model.Pinned
    match (model.FindShapeWithParticle(id)) with
    | None -> model
    | Some s ->
        let p = s.GetParticle id
        let p' =
            { p with
                PrevXPos = p.XPos
                PrevYPos = p.YPos
                XPos = if p.Vertical then p.XPos else x
                YPos = y
                }
        let s' = s.AddParticle p'
        model.ReplaceShape s'

let update cvs (msg : Message) (model : Model) : Model =
    match msg with
    | Frame ->
        updateFrame cvs (pin model)
    | Pinned (id,x,y) ->
        { model with Pinned= (id,x,y) }

let runGameLoop<'Model,'Msg> (init : unit -> 'Model) (update: 'Msg -> 'Model -> 'Model) (view : 'Model -> unit) (frame : 'Msg)=

    let rafu f =
        window.requestAnimationFrame (fun _ -> f()) |> ignore

    let  mutable model = init()

    let rec main() =
        model <- update frame model
        view model
        rafu main

    let dispatch msg =
        model <- update msg model

    (fun () -> model), dispatch, (fun () -> rafu main)

let run cvs =
    let energy = document.querySelector("#energy") :?> HTMLDivElement
    let model, dispatch, run = runGameLoop (init cvs) (update cvs) (view energy cvs) Frame
    initEventHandlers cvs model dispatch
    run()


run (document.querySelector("canvas") :?> HTMLCanvasElement)