# Forecalc

**Forecalc** is a core spreadsheet implementation in F# built for research purposes at the IT University of Copenhagen.

## Goals of **Forecalc**

* Learn large-scale functional programming using an LR parser, pattern matching, and asynchronous parallel calculations.
* Compare different data representations for large spreadsheets.
* Compare object-oriented and functional programming styles in a substantial piece of software, including testability, maintainability, and code volume.

### 2023-11-18 Update

* Upgraded to .NET 8.
* Web App rewritten in Razor Pages.

### 2018-03-13 Update

* Solution and projects upgraded for latest version of F#.

## Build and Run

Forecalc is a cross platform .NET 8 application:

```bash
dotnet restore
dotnet build
dotnet test
dotnet run --project src/Forecalc.Web
```
