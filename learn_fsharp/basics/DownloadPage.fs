(*
    https://fsharpforfunandprofit.com/posts/fvsc-download/ (Scott W.)
    Downloading a web page in F#

    use - only used with classes that implement IDisposable
        - meaning, dispose when it goes out of scope (ctx manager)

    Conceptually, 
    fetchUrl: [request -> response -> stream -> reader] -> callback: [read] 
*)

// .NET namespace to be visible
open System
open System.Net
open System.IO

// fetch contents of a web page
let fetchUrl callback url = 
    let req = WebRequest.Create(Uri(url))  // explicitly tell that it's Uri
    use resp = req.GetResponse()
    use stream = resp.GetResponseStream()
    use reader = new IO.StreamReader(stream)
    callback reader url  // callback processes the stream


// testing with a web page
let myCallback (reader:IO.StreamReader) url = 
    let html = reader.ReadToEnd()
    let html1000 = html.Substring(0, 1000)
    printfn "Downloaded %s. First 1000 is %s" url html1000
    html


// build function with callback backed in
// testing
let fetchUrl2 = fetchUrl myCallback  // partial application
let bbc = fetchUrl2 "http://bizovi.github.io"
let google = fetchUrl myCallback "http://google.com"

let sites = ["http://www.bing.com";
             "http://www.bing.com";
             "http://www.bing.com";]
sites |> List.map fetchUrl2
