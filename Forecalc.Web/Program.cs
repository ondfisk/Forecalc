using Forecalc.Library;
using Forecalc.Web.Models;
using Microsoft.FSharp.Collections;

var builder = WebApplication.CreateBuilder(args);

// Add services to the container.
builder.Services.AddRazorPages();
builder.Services.AddSingleton(builder => MapModule.Add("Sheet1", QT4.create<CellContent>(), MapModule.Empty<string, QT4<CellContent>>()));
builder.Services.AddSingleton<ForecalcService>();

var app = builder.Build();

// Configure the HTTP request pipeline.
if (!app.Environment.IsDevelopment())
{
    app.UseExceptionHandler("/Error");
    // The default HSTS value is 30 days. You may want to change this for production scenarios, see https://aka.ms/aspnetcore-hsts.
    app.UseHsts();
}

app.UseHttpsRedirection();
app.UseStaticFiles();

app.UseRouting();

app.UseAuthorization();

app.MapPut("/forecalc", (ForecalcService functions, CellUpdateDTO cell) => functions.Put(cell.Row, cell.Col, cell.Expr));
app.MapDelete("/forecalc", (ForecalcService functions) => functions.Reset());

app.MapRazorPages();

app.Run();
