using static CityFsLibrary.Domain;
using static CityFsLibrary.KyivExample;
var builder = WebApplication.CreateBuilder(args);

// Add services to the container.

builder.Services.AddControllers();
// Learn more about configuring Swagger/OpenAPI at https://aka.ms/aspnetcore/swashbuckle
builder.Services.AddEndpointsApiExplorer();
builder.Services.AddSwaggerGen();
builder.WebHost.ConfigureKestrel(options =>
{
    options.ListenLocalhost(5204, listenOption =>
    {
        listenOption.UseHttps("E:\\GitHub\\WebLab3\\WebLab3\\localhost.p12", "changeit");
    });
});
builder.Services.AddSingleton(generateKyivCity());
var app = builder.Build();

// Configure the HTTP request pipeline.
if (app.Environment.IsDevelopment())
{
    app.UseSwagger();
    app.UseSwaggerUI();
}

app.UseHttpsRedirection();

app.UseAuthorization();

app.MapControllers();

app.Run();
