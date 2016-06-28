//
////////////////////////////////////////

#I "..\packages\FsLab.0.3.19"

#load "FsLab.fsx"

open FSharp.Data
open FSharp.Data.CsvExtensions
open FSharp.Charting    
open RDotNet
open RProvider
open RProvider.qgraph

///////////////////////////////////////////////////////////


[<Literal>]
let work_path = @"D:\DataStorage\Metabolites\"

open System.IO
open FSharp.Data

open RDotNet
open RProvider
open RProvider.igraph

let dir = DirectoryInfo(work_path)
let all_cards = [for f in dir.EnumerateFiles() -> f.FullName]

//type Database = XmlProvider<"D:\DataStorage\Metabolites\HMDB02111.xml"> /// PATHWAY BUG
type Database = XmlProvider<"D:\DataStorage\Metabolites\HMDB00001.xml">

#r "System.Xml.Linq.dll"
open System.Xml.Linq

///////////////////////////////////////////

let fetch_metabolite (name:string) = 
    async {
                   let! aux = Database.AsyncLoad (name)
                   return aux
    }

let results =
    [for m in all_cards -> fetch_metabolite (m) ]              
    |> Async.Parallel |> Async.RunSynchronously

// Net relating metabolite with diseases

results

let aa =
    [for m in results -> m.Name, m.BiofluidLocations |> Seq.length  ]
    |> Seq.filter ( fun (_, contagem) -> contagem = 0)
    |> Seq.toArray








[for m in results -> m.ProteinAssociations |> Seq.map (fun p -> p.Name ) ]

[for m in results -> m.ProteinAssociations |> Seq.map (fun p -> p.Name) ]
|> Seq.collect id
|> Seq.length

[for m in results -> m.ProteinAssociations |> Seq.map (fun p -> p.GeneName) ]   
|> Seq.collect id  


[for m in results -> m.Name, m.Synonyms]

let res1 = 
    [for m in results ->
                m.Diseases
                |> Seq.map (fun x -> m.Name, x.Name,m.ProteinAssociations )

    ] |> Seq.collect id 
    
     
let show_disease = 
    res1 
    |> Seq.filter (fun (m, d,_) -> d = "Brain tumors" || d = "CNS tumors")
    |> Seq.iter (fun (m, d,p ) ->
                        printfn "%s\t *%s* => " m d
                        p |> Seq.iter (fun p1 -> printfn "\t\t%s\t[%s]" p1.Name p1.GeneName)
                        printfn ""  )

// genes
let genes = 
    [for m in results ->
                m.ProteinAssociations |> Seq.map (fun p -> m.Name, p.GeneName )
    ] |> Seq.collect id

genes |> Seq.length

genes |> Seq.distinctBy (fun (_, g ) -> g ) |> Seq.length

open FSharp.Data.HttpRequestHeaders
open FSharp.Data.JsonExtensions

let get_chromosome (protein_name : string) =
    let tmp_url = @"http://rest.genenames.org/fetch/symbol/" + protein_name
    let req = Http.RequestString( tmp_url , headers = [Accept HttpContentTypes.Json] )
    let res = JsonValue.Parse( req )
    res?response?docs.[0]?location.AsString()

    //res?response?docs.[0]?name.AsString()


get_chromosome ("AADAT")




