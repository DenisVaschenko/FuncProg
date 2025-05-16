using CityFsLibrary;
using Microsoft.AspNetCore.Mvc;
using static CityFsLibrary.Domain;

namespace CityApi.Controllers
{
    [ApiController]
    public class CityController : ControllerBase
    {
        private readonly City _city;

        public CityController(City city)
        {
            _city = city;
        }
        [HttpGet("places")]
        public ActionResult<IEnumerable<string>> GetPlaces()
        {
            return _city.getPlaces()
                .Select(p => p.Name)
                .ToList();
        }
        [HttpGet("findpath")]
        public ActionResult<IEnumerable<string>> FindPath(string from, string to)
        {
            var fromPlace = _city.findPlaceByName(from);
            var toPlace = _city.findPlaceByName(to);

            if (fromPlace == null || toPlace == null)
                return NotFound("Place not found");

            var pathOption = _city.findPath(fromPlace.Value, toPlace.Value);

            if (pathOption == null)
                return NotFound("Path not found");

            var path = pathOption.Value;


            var result = new List<string>();
            foreach (var (placeId, route) in path)
            {
                var place = _city.findPlaceById(placeId);
                result.Add(place.Name + " length: " +route.Length);
            }

            return Ok(result);
        }
        [HttpPost("place")]
        public ActionResult CreatePlace([FromBody] CreatePlaceRequest request)
        {
            if (string.IsNullOrWhiteSpace(request.Name))
                return BadRequest("Name is required");

            var location = new Point(request.X, request.Y);
            var newPlace = GraphOperations.createPlace(location, request.Name);
            _city.addPlace(newPlace);

            return Ok(new { newPlace.Id, newPlace.Name });
        }
        [HttpPost("connect")]
        public ActionResult ConnectPlaces([FromBody] ConnectPlacesRequest request)
        {
            var fromPlace = _city.findPlaceByName(request.FromPlaceName).Value;
            var toPlace = _city.findPlaceByName(request.ToPlaceName).Value;

            if (fromPlace == null || toPlace == null)
                return NotFound("One or both places not found");

            RouteType routeType = request.RouteType.ToLower() switch
            {
                "walking" => RouteType.Walking,
                "bus" => RouteType.NewBus(request.Price),
                "metro" => RouteType.NewMetro(request.MetroLine),
                _ => RouteType.Walking
            };

            try
            {
                _city.ConnectPlaces(routeType, request.Length, fromPlace, toPlace);
                return Ok("Places connected successfully");
            }
            catch (System.Exception ex)
            {
                return BadRequest(ex.Message);
            }
        }
        public class CreatePlaceRequest
        {
            public string Name { get; set; } = default!;
            public int X { get; set; }
            public int Y { get; set; }
        }
        public class ConnectPlacesRequest
        {
            public string FromPlaceName { get; set; } = default!;
            public string ToPlaceName { get; set; } = default!;
            public string RouteType { get; set; } = "walking";
            public double Length { get; set; }
            public double Price { get; set; } = 0.0;          
            public int MetroLine { get; set; } = 0;           
        }
    }
}
