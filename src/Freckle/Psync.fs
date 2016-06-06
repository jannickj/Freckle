namespace Freckle

type ThreadMax = int

type ThreadsUsed = int

type PsyncRes<'a> = ThreadsUsed * Async< 'a>

type Psync<'a> =  ThreadMax -> ThreadsUsed * Async< 'a>

module Psync =
    open System.Threading.Tasks
    
    let pure' (x : 'a) : Psync<'a> = fun s -> (1, async.Return x)

    let map (f : 'a -> 'b) (sa : Psync<'a>) : Psync<'b> = 
        fun c ->
            let (r, ma) = sa c            
            Async.map f ma
            |> tuple r

    let bind (f : 'a -> Psync<'b>) (sa : Psync<'a>) : Psync<'b> =
        fun c ->
            let (r, ma) = sa c            
            async {
                let! a = ma
                let (_, mb) = f a c
                let! b = mb
                return b
            }
            |> tuple r
            
    type Builder() =
        member inline this.Return(x : 'a) = pure' x
        member inline this.ReturnFrom(x) = x
        member inline this.Bind(ma, f) = bind f ma
        member inline this.Zero () = pure' ()

    let psync = Builder()
    
    let many (lpa : List<Psync<'a>>) : Psync<List<'a>> =
        fun c ->
            let res = List.map ((|>) c) lpa
            let ids = [1 .. List.length res]
            let resIds = List.zip ids res
            let psaRest = List.map (fun (id,(tc,ma)) -> (tc, Async.map (tuple id) ma) ) resIds
            let sorted = List.sortBy (fun (tc,_) -> tc) <| psaRest

            let awaitResource tc ma  =  
                let folder (inUse, running, completed) =
                    async {
                        let inUse' = inUse + tc  
                        if inUse' <= c || List.length running = 0
                        then let! ta = Async.StartChildAsTask ma
                             return Async.Completed (inUse', (tc, ta) :: running, completed)
                        else let! _ = Task.WhenAny (List.map snd running) |> Async.AwaitTask
                             let (running, res) = List.partition (fun (_, (t : Task<_>)) -> t.Status = TaskStatus.RanToCompletion) running
                             let (tc, completed') = List.unzip res

                             return Async.Continue (inUse - List.sum tc, running, completed' @ completed)

                    }
                Async.recursion folder

            let folder (rem, threads) =
                async {
                    match rem with
                    | (tc, ma) :: rest ->
                        let! threads' = awaitResource tc ma threads
                        return Async.Continue (rest, threads')
                    | [] -> return Async.Stop
                }
            async {
                let! (_, (_, running, completed)) = Async.recursion folder (sorted, (0, [], []))
                let! all = Task.WhenAll (List.map snd running @ completed) |> Async.AwaitTask
                return all |> List.ofArray |> List.sortBy fst |> List.map snd
            }
            |> tuple (min (List.map fst res |> List.sum) c)

    let two ma mb : Psync<'a * 'b> =
        psync {
            let! [a; b] = many [map box ma; map box mb]
            return (unbox a, unbox b)
        }
    
    let ap (mf : Psync<'a -> 'b>) (ma : Psync<'a>) : Psync<'b> =
        psync {
            let! (f, a) = two mf ma
            return f a
        }

    let three ma mb mc : Psync<'a * 'b * 'c> =
        psync {
            let! [a; b; c] = many [map box ma; map box mb; map box mc]
            return (unbox a, unbox b, unbox c)
        }

    let four ma mb mc md : Psync<'a * 'b * 'c * 'd> =
        psync {
            let! [a; b; c; d] = many [map box ma; map box mb; map box mc; map box md]
            return (unbox a, unbox b, unbox c, unbox d)
        }

    let five ma mb mc md me : Psync<'a * 'b * 'c * 'd * 'e> =
        psync {
            let! [a; b; c; d; e] = many [map box ma; map box mb; map box mc; map box md; map box me]
            return (unbox a, unbox b, unbox c, unbox d, unbox e)
        }

    let six ma mb mc md me mf : Psync<'a * 'b * 'c * 'd * 'e * 'f> =
        psync {
            let! [a; b; c; d; e; f] = many [map box ma; map box mb; map box mc; map box md; map box me; map box mf]
            return (unbox a, unbox b, unbox c, unbox d, unbox e, unbox f)
        }