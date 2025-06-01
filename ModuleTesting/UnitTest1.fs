module ModuleTesting

open NUnit.Framework
open CityFsLibrary.Domain
open CityFsLibrary.GraphOperations
open FsUnit
open FsCheck.NUnit
open System
open FsCheck

[<TestFixture>]
type ``City tests`` () =
    let createTestCity () =
        let place1 = createPlace { X = 0; Y = 0 } "A"
        let place2 = createPlace { X = 3; Y = 4 } "B"
        let place3 = createPlace { X = 2; Y = 1 } "C"
        let place4 = createPlace { X = 3; Y = 4 } "D"
        let city = City("TestCity")
        city.addPlace place1
        city.addPlace place2
        city.addPlace place3
        city.addPlace place4
        city.ConnectPlaces Walking 5.0 place1 place2
        city.ConnectPlaces Walking 6.0 place1 place4
        city.ConnectPlaces Walking 4.0 place2 place3
        city, place1, place2

    [<Test>]
    member _.``Can add and find a place by ID`` () =
        //Assert.Fail()
        let city, place1, _ = createTestCity()
        city.findPlaceById(place1.Id).IsSome |> should be True
        city.findPlaceById(place1.Id).Value.Name |> should equal place1.Name

    [<Test>]
    member _.``Can find place by name`` () =
        let city, place1, _ = createTestCity()
        let result = city.findPlaceByName("A")
        result.IsSome |> should be True
        result.Value.Id |> should equal place1.Id

    [<Test>]
    member _.``Connected places are neighbours of each other`` () =
        let city, place1, place2 = createTestCity()
        let updatedPlace1 = city.findPlaceById(place1.Id)
        let updatedPlace2 = city.findPlaceById(place2.Id)
        updatedPlace1.IsSome |> should be True
        updatedPlace2.IsSome |> should be True
        updatedPlace1.Value.Neighbours |> List.exists (fun r -> r.PlaceId = place2.Id) |> should be True
        updatedPlace2.Value.Neighbours |> List.exists (fun r -> r.PlaceId = place1.Id) |> should be True

    [<Test>]
    member _.``Creating route calculates distance constraint`` () =
        let place1 = createPlace { X = 0; Y = 0 } "A"
        let place2 = createPlace { X = 0; Y = 10 } "B"
        let city = City("DistanceCheck")
        city.addPlace place1
        city.addPlace place2
        (fun () -> city.ConnectPlaces Walking 5.0 place1 place2 |> ignore)
        |> should throw typeof<System.Exception>

    [<TestCase(1, 1, 4, 5, 5.0)>]
    [<TestCase(1, 2, 4, 6, 5.0)>]
    member _.``Distance between points is calculated correctly`` (x1: int, y1: int, x2: int, y2: int, expected: float) =
        let p1 = { Id = "1"; Name = "Place1"; Location = { X = x1; Y = y1 }; Neighbours = [] }
        let p2 = { Id = "1"; Name = "Place1"; Location = { X = x2; Y = y2 }; Neighbours = [] }
        let distance1 = findDistance p1 p2
        let distance2 = findDistance p2 p1
        distance1 |> should equal distance2
        distance1 |> should (equalWithin 0.0001) expected

    [<Test>]
    member _.``getNeighbours returns correct neighbours`` () =
        let city, place1, place2 = createTestCity()
        let updatedPlace = city.findPlaceById(place1.Id)
        updatedPlace.IsSome |> should be True
        getNeighbours updatedPlace.Value |> should contain place2.Id

    [<Test>]
    member _.``getRouteInfo returns correct price`` () =
        let route = { PlaceId = "1"; Length = 10.0; Type = Bus 12.0<uah> }
        let _, price = getRouteInfo route
        price |> should equal 12.0<uah>

    [<Test>]
    member _.``countPrice calculates full path price`` () =
        let path = [
            ("1", { PlaceId = "1"; Length = 1.0; Type = Bus 5.0<uah> })
            ("2", { PlaceId = "2"; Length = 1.0; Type = Metro 1 })
            ("3", { PlaceId = "3"; Length = 1.0; Type = Walking })
        ]
        let price = countPrice path
        price |> should equal (5.0<uah> + defaultMetroPrice + 0.0<uah>)

    [<Test>]
    member _.``City.findPath returns correct route`` () =
        let city,_,_ = createTestCity ()
        match city.findPlaceByName("B"), city.findPlaceByName("C") with
        | Some fromPlace, Some toPlace ->
            let pathOption = city.findPath fromPlace toPlace

            pathOption.IsSome |> should be True
            let path = pathOption.Value

            // Перший елемент — цільовий вузол
            path.Head |> fst |> should equal fromPlace.Id

            // Перевірка усіх кроків маршруту
            path |> Seq.iter (fun (id, route) ->
                let placeOpt = city.findPlaceById(id)
                placeOpt.IsSome |> should be True
                let place = placeOpt.Value

                route.PlaceId |> should equal place.Id
                place.Name |> should not' (equal "")
                route.Length |> should be (greaterThanOrEqualTo 0.0)
            )
    
        | _ ->
            Assert.Fail("Either 'B' or 'C' was not found in the generated city.")
    [<Property>]
    member _.``Randomly added and connected places allow for path search`` (size : PositiveInt) =
        let size = max 3 (min 1000 size.Get) // Щонайменше 3 точки
        TestContext.WriteLine(size)
        let city = City("GeneratedCity")
        let random = Random()
        // Крок 1: генерація і додавання місць
        let places =
            [ for i in 0 .. size - 1 ->
                createPlace { X = random.Next(-100, 100); Y = random.Next(-100, 100) } $"P{i}" ]
        places |> List.iter city.addPlace

        // Крок 2: генерація з'єднань
        for _ in 1 .. size * 2 do
            let a = places[random.Next(size)]
            let b = places[random.Next(size)]
            if a.Id <> b.Id then
                let distance = findDistance a b
                let dist = distance * (1.0 + random.NextDouble())
                try city.ConnectPlaces (Bus 5.0<uah>) dist a b with _ -> ()

        // Крок 3: вибір 2 випадкових місць
        let fromPlace = places[random.Next(size)]
        let toPlace = places[random.Next(size)]

        // Крок 4: перевірка findPath
        let pathOption = city.findPath fromPlace toPlace

        match pathOption with
        | None -> true // шлях може не існувати — допустимо
        | Some path ->
            // Кожна точка в шляху має існувати і відповідати ID
            path
            |> List.forall (fun (id, route) ->
                match city.findPlaceById(id) with
                | Some place -> route.PlaceId = place.Id && place.Name <> "" && route.Length >= 0.0
                | None -> false
            )