namespace CityFsLibrary
open FSharp.Data
   
module CityAnalyzer = 
    type resultOfAnalyzing = {
        TotalPlaces: int
        TotalRoutes: int
        AverageRouteLength: float option
        LongestRoute: (string * string * float) option
        ShortestRoute: (string * string * float) option
    }
    type Config = JsonProvider<"config.json">
    type CityData = JsonProvider<"data/cityExample.json">
    let Analyze () = task {
        let filePath = Config.Load("config.json").CityFilePath
        let! cityData = CityData.AsyncLoad(filePath)
        let totalPlaces = cityData.Places |> Seq.length
        let totalRoutes = 
            cityData.Places |>
            Seq.sumBy (fun place -> place.Neighbours |> Seq.length)
        let averageRouteLength = 
            match totalRoutes with
            | 0 -> None
            | _ -> 
                let totalLength = 
                    cityData.Places |>
                    Seq.sumBy (fun place -> place.Neighbours |> Seq.sumBy (fun route -> route.Length |> float))
                Some (totalLength / float totalRoutes)
        let nameMap = 
            cityData.Places |>
            Seq.map (fun place -> place.Id, place.Name) |>
            Map.ofSeq
        let longestRoute =
            match totalRoutes with
            |0 -> None
            | _ -> 
                cityData.Places |>
                Seq.collect (fun place -> 
                    place.Neighbours |>
                    Seq.map (fun route -> (place.Name, nameMap[route.PlaceId], route.Length |> float))) |>
                Seq.maxBy (fun (_,_,length) -> length) |> Some
            
        let shortestRoute =
            match totalRoutes with
            |0 -> None
            | _ ->
                cityData.Places |>
                Seq.collect (fun place -> 
                    place.Neighbours |>
                    Seq.map (fun route -> (place.Name, nameMap[route.PlaceId], route.Length |> float))) |>
                Seq.minBy (fun (_,_,length) -> length) |> Some
        return {TotalPlaces = totalPlaces; TotalRoutes = totalRoutes; AverageRouteLength = averageRouteLength; LongestRoute = longestRoute; ShortestRoute = shortestRoute }
    }

