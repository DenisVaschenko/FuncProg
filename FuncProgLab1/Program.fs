
namespace CityProgram
open System.Collections.Generic
open System

module Domain =
    [<Measure>] type uah
    
    type Point = {X: int; Y: int}
    type RouteType =
        | Walking
        | Bus of price: float<uah>
        | Metro of line: int
    let defaultMetroPrice = 40.0<uah>
    type Id = string
    type Route = {Length: float; Type: RouteType}
    type Place = {Id: Id; Name: string; Location: Point; Neighbours : (Id * Route) list}
    type defaultMap<'T> = Map<Id, 'T>
    type CityMap = {Name: string; Places: defaultMap<Place> }
module GraphOperations=
    open Domain
    let createCity name = { Name = name; Places = Map.empty}
    let generateId () =
        System.Guid.NewGuid().ToString()
    let createPlace location name =
        {Id = generateId (); Name = name; Location = location; Neighbours = []}
    let addPlace city place =
        {city with Places = city.Places.Add(place.Id, place)}
    let findPlaceByName city name=
        city.Places.Values |> Seq.tryFind (fun place -> place.Name = name)
    let findPlaceById city id =
        city.Places[id]
    let findDistance fromPlace toPlace =
        let dx = toPlace.Location.X - fromPlace.Location.X
        let dy = toPlace.Location.Y - fromPlace.Location.Y
        Math.Sqrt(float (dx * dx + dy * dy))
    let connectPlaces routeType length fromPlace toPlace city=
        let minDistance = findDistance fromPlace toPlace
        match minDistance with
        | d when d > length -> 
            failwithf "The minimal possible distance between %s and %s is more than the specified length." fromPlace.Name toPlace.Name
        | _ ->
            let fromPlace = city.Places[fromPlace.Id]
            {city with Places = city.Places.Add(fromPlace.Id, {fromPlace with Neighbours = (toPlace.Id, {Length = length; Type = routeType}) :: fromPlace.Neighbours})}
    let swapFunc f x y res=
        res |> f x y |> f y x
    let connect2Directions routeType length place1 place2 city = swapFunc (connectPlaces routeType length) place1 place2 city

    let getRouteInfo route=
        let price =
            match route.Type with
            | Walking -> 0.0<uah>
            | Bus price -> price
            | Metro line -> defaultMetroPrice
        route.Length, price
    let getNeighbours place =
        place.Neighbours |> List.map(fun (neighbour, route) -> neighbour)
    let findPath city fromPlace toPlace =
        let queue = PriorityQueue<(Id * Route) list * float, float>()
        let seen = HashSet<Id>()
        let destDistance = findDistance toPlace
        queue.Enqueue(([fromPlace.Id, {Length = 0.0; Type = Walking}], 0.0), destDistance fromPlace)
        let rec loop () =
            match queue.Count with 
                | 0 -> None
                | _ ->
                    let path, currentLength = queue.Dequeue()
                    let currentPlace = city.Places[path.Head |> fst]
                    match currentPlace with
                        | place when place.Id = toPlace.Id -> List.rev path |> Some
                        | place when seen.Contains(place.Id) -> loop ()
                        | _ ->
                            seen.Add(currentPlace.Id) |> ignore
                            currentPlace.Neighbours
                            |> Seq.filter(fun (neighbour, route) -> not (seen.Contains(neighbour)))
                            |> Seq.iter(fun (neighbourId, route) ->
                                queue.Enqueue(((neighbourId, route) :: path, currentLength + route.Length), 
                                currentLength + route.Length + destDistance city.Places[neighbourId])
                            )
                            loop () 
        loop ()
module KyivExample =
    open Domain
    open GraphOperations
    let generateKyivCity () =
        let maidan = createPlace { X = 0; Y = 0 } "Майдан Незалежності"
        let khreschatyk = createPlace { X = 1; Y = 0 } "Хрещатик"
        let arsenalna = createPlace { X = 3; Y = -1 } "Арсенальна"
        let palaceSportu = createPlace { X = 1; Y = -1 } "Палац Спорту"
        let university = createPlace { X = -1; Y = 1 } "Університет"
        createCity "Kyiv"
        |> addPlace <| university
        |> addPlace <| palaceSportu
        |> addPlace <| arsenalna
        |> addPlace <| khreschatyk
        |> addPlace <| maidan
        |> connect2Directions (Metro 1) 1.0 maidan khreschatyk
        |> connect2Directions (Bus 12.0<uah>) 4.5 palaceSportu university
        |> connect2Directions (Metro 1) 2.5 khreschatyk arsenalna
        |> connect2Directions (Walking) 1.3 khreschatyk palaceSportu
    
    let printCity city =
        printfn $"Місто: {city.Name}"
        for place in city.Places.Values do
            printfn $"\nМісце: {place.Name} (ID: {place.Id})"
            printfn $"Координати: ({place.Location.X}, {place.Location.Y})"
            if List.isEmpty place.Neighbours then
                printfn "  Немає сусідів."
            else
                printfn "  Сусіди:"
                for (neighbourId, route) in place.Neighbours do
                    let routeTypeStr =
                        match route.Type with
                        | Walking -> "Пішки"
                        | Bus price -> $"Автобус ({price} грн)"
                        | Metro line -> $"Метро (лінія {line})"
                    printfn $"    → {city.Places[neighbourId].Name} | Довжина: {route.Length:F1} | Тип: {routeTypeStr}"
    let countPrice (path : (Id * Route) list) =
       let rec inner (lastEl : (Id * Route) list) price =
          let currPrice = lastEl.Head |> snd |> getRouteInfo |> snd
          match lastEl with
            | el when el.Tail = [] -> price + currPrice
            | _ -> inner lastEl.Tail (price + currPrice)
       inner path 0.0<uah>

    [<EntryPoint>]
    let main argv =
        let mainCity =  generateKyivCity ()
        printCity mainCity
        let findPlaceInKyiv = findPlaceByName mainCity
        let maidan = (findPlaceInKyiv "Майдан Незалежності")
        let university = (findPlaceInKyiv "Університет")
        match maidan,university with
        | Some maidan, Some university ->
            let path = findPath mainCity maidan university
            match path with
            | None -> printfn "Шлях не знайдено."
            | Some path ->
                let price = countPrice path
                printfn $"Шлях {maidan.Name} - {university.Name} з ціною: {price} грн."
                for placeId, route in path do
                    printfn $"Назва: {mainCity.Places[placeId].Name} Тип: {route.Type}  Довжина: {route.Length}"
        | _ -> printfn "Не вдалося знайти місця в місті."
        0
