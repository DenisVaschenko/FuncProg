
namespace CityFsLibrary
open System.Collections.Generic
open System
open System.Text.Json.Serialization

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
    type Route = {PlaceId : Id; Length: float; Type: RouteType}
    type Place = {Id: Id; Name: string; Location: Point; Neighbours : Route list}
        with interface ILocatable with
                member this.Coordinates = this.Location

    type defaultMap<'T> = Map<Id, 'T>
    let findDistance (fromPlace : ILocatable) (toPlace : ILocatable) =
            let dx = toPlace.Coordinates.X - fromPlace.Coordinates.X
            let dy = toPlace.Coordinates.Y - fromPlace.Coordinates.Y
            Math.Sqrt(float (dx * dx + dy * dy))
    type CityMap = {Name: string; Places: defaultMap<Place>; NumOfRoutes: int}
    type City(cityMap: CityMap) =
        let mutable CityMap = cityMap
        member _.Id = generateId ()
        member _.getCityMap ()= {Name = CityMap.Name; Places = CityMap.Places; NumOfRoutes = CityMap.NumOfRoutes}
        new(name: string) =
            City({Name = name; Places = Map.empty; NumOfRoutes = 0})
        member _.addPlace (place: Place) =
            CityMap <- {CityMap with Places = CityMap.Places.Add(place.Id, place)}
        member _.findPlaceById (id: Id) =
            CityMap.Places.TryFind(id)
        member _.findPlaceByName (name: string) =
            CityMap.Places.Values |> Seq.tryFind(fun place -> place.Name = name)
        member _.getPlaces () =
            printfn "%A" CityMap.Places.Values
            CityMap.Places.Values
        member this.ConnectPlaces (routeType: RouteType) (length: float) (fromPlace: Place) (toPlace: Place) =
            let minDistance = findDistance (fromPlace:>ILocatable) (toPlace:>ILocatable)
            match minDistance with
            | d when d > length -> 
                failwithf "The minimal possible distance between %s and %s is more than the specified length." fromPlace.Name toPlace.Name
            | _ ->
                let fromPlace = CityMap.Places[fromPlace.Id]
                let toPlace = CityMap.Places[toPlace.Id]
                let newFromPlace = {fromPlace with Neighbours = {PlaceId = toPlace.Id; Length = length; Type = routeType} :: fromPlace.Neighbours}
                let newToPlace = {toPlace with Neighbours = {PlaceId = fromPlace.Id;Length = length; Type = routeType} :: toPlace.Neighbours}
                CityMap <- {CityMap with Places = CityMap.Places.Add(fromPlace.Id, newFromPlace).Add(toPlace.Id, newToPlace); NumOfRoutes = CityMap.NumOfRoutes + 1}
        member this.printCity =
            printfn $"Місто: {CityMap.Name}"
            for place in CityMap.Places.Values do
                printfn $"\nМісце: {place.Name} (ID: {place.Id})"
                printfn $"Координати: ({place.Location.X}, {place.Location.Y})"
                if List.isEmpty place.Neighbours then
                    printfn "  Немає сусідів."
                else
                    printfn "  Сусіди:"
                    for route in place.Neighbours do
                        let routeTypeStr =
                            match route.Type with
                            | Walking -> "Пішки"
                            | Bus price -> $"Автобус ({price} грн)"
                            | Metro line -> $"Метро (лінія {line})"
                        printfn $"    → {CityMap.Places[route.PlaceId].Name} | Довжина: {route.Length:F1} | Тип: {routeTypeStr}"
        member this.findPath fromPlace toPlace =
            let queue = PriorityQueue<(Id * Route) list * float, float>()
            let seen = HashSet<Id>()
            let destDistance = findDistance toPlace
            queue.Enqueue(([fromPlace.Id, {PlaceId = fromPlace.Id; Length = 0.0; Type = Walking}], 0.0), destDistance fromPlace)
            let rec loop () =
                match queue.Count with 
                    | 0 -> None
                    | _ ->
                        let path, currentLength = queue.Dequeue()
                        let currentPlace =path.Head |> fst |> this.findPlaceById
                        match currentPlace with
                            | None -> 
                                failwithf "Place with ID %s not found in the city." (path.Head |> fst)
                            | Some place when place.Id = toPlace.Id -> List.rev path |> Some
                            | Some place when seen.Contains(place.Id) -> loop ()
                            | Some place ->
                                seen.Add(place.Id) |> ignore
                                place.Neighbours
                                |> Seq.filter(fun route -> not (seen.Contains(route.PlaceId)))
                                |> Seq.iter(fun route ->
                                    match route.PlaceId |> this.findPlaceById with
                                        | None -> failwithf "Place with ID %s not found in the city." route.PlaceId
                                        | Some newPlace ->
                                            queue.Enqueue(((route.PlaceId, route) :: path, currentLength + route.Length), 
                                            currentLength + route.Length + (newPlace |> destDistance) )
                                    
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
        place.Neighbours |> List.map(fun route -> route.PlaceId)
    let countPrice (path : (Id * Route) list) =
       let rec inner (lastEl : (Id * Route) list) price =
          let currPrice = lastEl.Head |> snd |> getRouteInfo |> snd
          match lastEl with
            | el when el.Tail = [] -> price + currPrice
            | _ -> inner lastEl.Tail (price + currPrice)
       inner path 0.0<uah>
    
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
        city.ConnectPlaces (Metro 1) 1.0 maidan khreschatyk
        city.ConnectPlaces (Bus 12.0<uah>) 4.5 palaceSportu university
        city.ConnectPlaces (Metro 1) 2.5 khreschatyk arsenalna
        city.ConnectPlaces (Walking) 1.3 khreschatyk palaceSportu
        city
    
    
 module SaveCity =
    open KyivExample
    open Domain
    open System.IO
    open System.Text.Json
    open FSharp.Data
    type Config = JsonProvider<"config.json">
    let filePath = Config.Load("config.json").CityFilePath
    type CityData = {Name : string; Places : Place list}
    let saveCity (city: City) =
        let options = JsonSerializerOptions(PropertyNamingPolicy = JsonNamingPolicy.CamelCase, WriteIndented = true)
        options.Converters.Add(JsonFSharpConverter())
        use stream = new FileStream(filePath, FileMode.Create, FileAccess.Write)
        let cityMap = city.getCityMap()
        JsonSerializer.Serialize(stream, {Name = cityMap.Name; Places = cityMap.Places.Values |> Seq.toList }, options)
    let getSavedCity () =
        let options = JsonSerializerOptions(PropertyNamingPolicy = JsonNamingPolicy.CamelCase, WriteIndented = true)
        options.Converters.Add(JsonFSharpConverter())
        use stream = new FileStream(filePath, FileMode.Open, FileAccess.Read)
        JsonSerializer.Deserialize<CityData>(stream, options)
        |> fun data -> 
            let city = City(data.Name)
            data.Places |> List.iter (fun place -> city.addPlace place)
            city
    [<EntryPoint>]
    let main argv =
        let kCity = generateKyivCity ()
        printf "%A" (kCity.findPlaceByName("Арсенальна").Value)
        //saveCity (generateKyivCity ())
        0
