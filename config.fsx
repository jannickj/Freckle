// --------------------------------------------------------------------------------------
// FAKE Config
// --------------------------------------------------------------------------------------

// --------------------------------------------------------------------------------------
// START TODO: Provide project-specific details below
// --------------------------------------------------------------------------------------

// Information about the project are used
//  - for version and project name in generated AssemblyInfo file
//  - by the generated NuGet package
//  - to run tests and to publish documentation on GitHub gh-pages
//  - for documentation, you also need to edit info in "docs/tools/generate.fsx"

// The name of the project
// (used by attributes in AssemblyInfo, name of a NuGet package and directory in 'src')
let project = "Freckle"

// Short summary of the project
// (used as description in AssemblyInfo and as a short summary for NuGet package)
let summary = "A Functional Reactive Programming Library for FSharp"

// Longer description of the project
// (used as a description for NuGet package; line breaks are automatically cleaned up)
let description = "Functional Reactive Programming library Handle"

// List of author names (for NuGet package)
let authors = [ "Jannick Johnsen" ]

// Tags for your project (for NuGet package)
let tags = "FRP reactive ui event behavior functional freckle"

// File system information
let solutionFile  = "Freckle.sln"

// Pattern specifying assemblies to be tested using NUnit
let testAssemblies = "tests/**/bin/Release/*Tests*.dll"

// Git configuration (used for publishing documentation in gh-pages branch)
// The profile where the project is posted
let gitOwner = "jannickj"

// The name of the project on GitHub
let gitName = "Freckle"

let nugetUrl = "https://nuget.org/" 
let nugetFeed = "api/v2/package" 
