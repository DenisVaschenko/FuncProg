
namespace CityFsLibrary
open System.Collections.Generic
open System

module Domain =
    [<Measure>] type uah
    let generateId () =
        System.Guid.NewGuid().ToString()
    type Point = {X: int; Y: int}
    type ILocatable =
        abstract member Coordinates : Point
    type RouteType =
        | Walking
        | Bus of price: float<uah>
        | Metro of line: int
    let defaultMetroPrice = 40.0<uah>
    type Id = string
    type Route = {Length: float; Type: RouteType}
    type Place = {Id: Id; Name: string; Location: Point; Neighbours : (Id * Route) list}
        with interface ILocatable with
                member this.Coordinates = this.Location

    type defaultMap<'T> = Map<Id, 'T>
    
    type CityMap = {Places: defaultMap<Place>; NumOfRoutes: int}
    type City(name: string, cityMap: CityMap) =
        let mutable CityMap = cityMap
        let findDistance (fromPlace : ILocatable) (toPlace : ILocatable) =
            let dx = toPlace.Coordinates.X - fromPlace.Coordinates.X
            let dy = toPlace.Coordinates.Y - fromPlace.Coordinates.Y
            Math.Sqrt(float (dx * dx + dy * dy))
        member _.Id = generateId ()
        
        
        member _.Name = name
        new(name: string) =
            City(name, {Places = Map.empty; NumOfRoutes = 0})
        member _.addPlace (place: Place) =
            CityMap <- {CityMap with Places = CityMap.Places.Add(place.Id, place)}
        member _.findPlaceById (id: Id) =
            CityMap.Places[id]
        member _.findPlaceByName (name: string) =
            CityMap.Places.Values |> Seq.tryFind(fun place -> place.Name = name)
        member _.getPlaces () =
            CityMap.Places.Values
        member this.ConnectPlaces (routeType: RouteType) (length: float) (fromPlace: Place) (toPlace: Place) =
            let minDistance = findDistance (fromPlace:>ILocatable) (toPlace:>ILocatable)
            match minDistance with
            | d when d > length -> 
                failwithf "The minimal possible distance between %s and %s is more than the specified length." fromPlace.Name toPlace.Name
            | _ ->
                let fromPlace = CityMap.Places[fromPlace.Id]
                let toPlace = CityMap.Places[toPlace.Id]
                let newFromPlace = {fromPlace with Neighbours = (toPlace.Id, {Length = length; Type = routeType}) :: fromPlace.Neighbours}
                let newToPlace = {toPlace with Neighbours = (fromPlace.Id, {Length = length; Type = routeType}) :: toPlace.Neighbours}
                CityMap <- {CityMap with Places = CityMap.Places.Add(fromPlace.Id, newFromPlace).Add(toPlace.Id, newToPlace)}
        member this.connect2Directions routeType length place1 place2 =
            this.ConnectPlaces routeType length place1 place2 
            this.ConnectPlaces routeType length place2 place1
        member this.printCity =
            printfn $"Місто: {this.Name}"
            for place in CityMap.Places.Values do
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
                        printfn $"    → {CityMap.Places[neighbourId].Name} | Довжина: {route.Length:F1} | Тип: {routeTypeStr}"
        member this.findPath fromPlace toPlace =
            let queue = PriorityQueue<(Id * Route) list * float, float>()
            let seen = HashSet<Id>()
            let destDistance = findDistance toPlace
            queue.Enqueue(([fromPlace.Id, {Length = 0.0; Type = Walking}], 0.0), destDistance fromPlace)
            let rec loop () =
                match queue.Count with 
                    | 0 -> None
                    | _ ->
                        let path, currentLength = queue.Dequeue()
                        let currentPlace =path.Head |> fst |> this.findPlaceById 
                        match currentPlace with
                            | place when place.Id = toPlace.Id -> List.rev path |> Some
                            | place when seen.Contains(place.Id) -> loop ()
                            | _ ->
                                seen.Add(currentPlace.Id) |> ignore
                                currentPlace.Neighbours
                                |> Seq.filter(fun (neighbour, route) -> not (seen.Contains(neighbour)))
                                |> Seq.iter(fun (neighbourId, route) ->
                                    queue.Enqueue(((neighbourId, route) :: path, currentLength + route.Length), 
                                    currentLength + route.Length + (neighbourId |> this.findPlaceById |> destDistance) )
                                )
                                loop () 
            loop ()

module GraphOperations=
    open Domain
    let createPlace location name =
        {Id = generateId (); Name = name; Location = location; Neighbours = []}
        
    let getRouteInfo route=
        let price =
            match route.Type with
            | Walking -> 0.0<uah>
            | Bus price -> price
            | Metro line -> defaultMetroPrice
        route.Length, price
    let getNeighbours place =
        place.Neighbours |> List.map(fun (neighbour, route) -> neighbour)
    
module KyivExample =
    open Domain
    open GraphOperations
    let generateKyivCity () =
        let maidan = createPlace { X = 0; Y = 0 } "Майдан Незалежності"
        let khreschatyk = createPlace { X = 1; Y = 0 } "Хрещатик"
        let arsenalna = createPlace { X = 3; Y = -1 } "Арсенальна"
        let palaceSportu = createPlace { X = 1; Y = -1 } "Палац Спорту"
        let university = createPlace { X = -1; Y = 1 } "Університет"
        let city = City "Kyiv"
        city.addPlace university
        city.addPlace palaceSportu
        city.addPlace arsenalna
        city.addPlace khreschatyk
        city.addPlace maidan
        city.connect2Directions (Metro 1) 1.0 maidan khreschatyk
        city.connect2Directions (Bus 12.0<uah>) 4.5 palaceSportu university
        city.connect2Directions (Metro 1) 2.5 khreschatyk arsenalna
        city.connect2Directions (Walking) 1.3 khreschatyk palaceSportu
        city
    
    
    let countPrice (path : (Id * Route) list) =
       let rec inner (lastEl : (Id * Route) list) price =
          let currPrice = lastEl.Head |> snd |> getRouteInfo |> snd
          match lastEl with
            | el when el.Tail = [] -> price + currPrice
            | _ -> inner lastEl.Tail (price + currPrice)
       inner path 0.0<uah>

    [<EntryPoint>]
    let main argv =
        0
