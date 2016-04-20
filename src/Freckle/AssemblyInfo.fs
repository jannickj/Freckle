namespace System
open System.Reflection

[<assembly: AssemblyTitleAttribute("Freckle")>]
[<assembly: AssemblyProductAttribute("Freckle")>]
[<assembly: AssemblyDescriptionAttribute("A Functional Reactive Programming Library for FSharp")>]
[<assembly: AssemblyVersionAttribute("0.0.1")>]
[<assembly: AssemblyFileVersionAttribute("0.0.1")>]
do ()

module internal AssemblyVersionInformation =
    let [<Literal>] Version = "0.0.1"
