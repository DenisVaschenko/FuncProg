using static CityFsLibrary.SaveCity;
var builder = WebApplication.CreateBuilder(args);

// Add services to the container.

builder.Services.AddControllers();
builder.Services.AddEndpointsApiExplorer();
builder.Services.AddSwaggerGen();
builder.WebHost.ConfigureKestrel(options =>
{
    options.ListenLocalhost(5204, listenOption =>
    {
        listenOption.UseHttps("E:\\GitHub\\WebLab3\\WebLab3\\localhost.p12", "changeit");
    });
});

builder.Services.AddSingleton(getSavedCity());
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
