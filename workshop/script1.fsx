////////////////////////////////////////
//
// EXPLORING DISEASE INFO -!
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
let file_to_open = @"D:\DataStorage\diseases\all_gene_disease_associations.csv"

type GeneNet = CsvProvider<file_to_open>

let dataset = GeneNet.GetSample()

dataset.Headers

let pair_disease_gene = 
    [for d in dataset.Rows -> d.DiseaseName, d.GeneName]

let new_set = 
    pair_disease_gene 
    |> List.groupBy (fun (disease, _) -> disease )
    |> List.map (fun (disease, st ) -> disease, st |> List.map (fun (name,gene) -> gene))
    |> dict

new_set.Keys |> Seq.length

let disease_10 = 
    [for k in new_set -> k.Key, k.Value ]
    |> List.take 10

let links_from = 
    [for (disease,gene) in disease_10 -> gene
                                         |> List.map (fun _ -> disease) ]
    |> Seq.collect id

let links_to = [for (_,gene) in disease_10 -> gene
                                                    |> List.map (fun g -> g) ] |> Seq.collect id
let weights = [for (disease,gene) in disease_10 -> gene |> Seq.map (fun x -> 1.0)] |> Seq.collect id

let rf = ["from", box ( links_from )
          "to", box ( links_to )
          "weight", box ( weights) ] |> namedParams |> R.data_frame

let graph1 = ["input", box (rf )
              "directed", box false
              "layout", box "spring"  
              "curveAll", box true
              "posCol", box "darkgreen"
              "negCol", box "darkred"
              "border", box false
              "curve", box 0.25
              "vsize", box 5.0 ] |> namedParams 

let r3 = R.qgraph( graph1 )


////// HMDB
/// 

[<Literal>]
let work_path = @"D:\DataStorage\Metabolites\"

open System.IO
open FSharp.Data

open RDotNet
open RProvider
open RProvider.igraph

let dir = DirectoryInfo(work_path)
let all_cards = [for f in dir.EnumerateFiles() -> f.FullName]

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

let diseases = 
    results 
    |> Seq.map (fun m -> m.Diseases )
    |> Seq.collect id

[for d in diseases -> d.Name]
|> Seq.distinct
|> Seq.iter (fun s -> printfn "%s " s)

let res1 = 
    [for m in results ->
                m.Diseases
                |> Seq.map (fun x -> m.Name, x.Name,m.ProteinAssociations )

    ] |> Seq.collect id

// show diseases
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

genes
|> Seq.countBy (fun (_,g) -> g )
|> Seq.maxBy (fun (_,i) -> i )

let metabolites =
    results 
    |> Seq.map (fun m -> let protein =  m.ProteinAssociations
                                        |> Seq.map (fun p -> p.Name )
                                        |> Seq.toList
                                        



                         (m.Name, m.Description) )  |> dict

printfn "%s" metabolites.["Ascorbate"] 

metabolites.Keys |> Seq.iter (fun x -> printfn "%s" x)

open FSharp.Data.HttpRequestHeaders
open FSharp.Data.JsonExtensions

let get_chromosome (protein_name : string) =
    let tmp_url = @"http://rest.genenames.org/fetch/symbol/" + protein_name
    let req = Http.RequestString( tmp_url , headers = [Accept HttpContentTypes.Json] )
    let res = JsonValue.Parse( req )
    res?response?docs.[0]?location.AsString()


get_chromosome ("MOGAT2")



















let diseases =
         results
         |> Seq.map (fun x -> x.Diseases 
                              |> Seq.map (fun x -> x.Name))
         |> Seq.collect id
         |> Seq.groupBy (fun x -> x) |> dict

[for d in diseases -> d.Key, (d.Value |> Seq.length) ] |> Seq.maxBy (fun (_,b) -> b)

let my_list = [for d in diseases -> d.Key + " : " + ( (d.Value) |> Seq.length).ToString()  ]

open System.Windows.Forms
open System.Drawing

let test (alist : string list)=
    let form = new Form (TopMost = true,
                         Visible = true,
                         Width   = 400,
                         Height  = 400 )

    form.Text <- " -- ... " 

    let listbox = new ListBox()
    listbox.Size <- form.Size 
    listbox.BackColor <- Color.LightGray
    [for l in alist -> listbox.Items.Add( l) ] |> ignore

    form.Controls.Add( listbox )
    form.Show()

test(my_list )










let new_set = 
    results |> Seq.filter (fun x ->
                             let flag = x.Diseases 
                                        |> Seq.map (fun y -> y.Name)
                                        |> Seq.contains "Alzheimer's disease"
                             flag )
            |> Seq.map id


let links2 = 
    new_set
    |> Seq.map (fun x -> [for d in x.Diseases -> x.Name, d.Name] ) 
    |> Seq.collect id

let rand = System.Random(100)

let rand1() = rand.NextDouble()

let len = links2 |> Seq.length

let rf = 
    ["from", box ( links2 |> Seq.map (fun x -> fst x))
     "to", box ( links2 |> Seq.map (fun x -> snd x))
     "weight", box ( [0..len-1] |> Seq.map (fun _ -> rand1()    ))
    ] |> namedParams |> R.data_frame


let graph1 = ["input", box rf
              "directed", box false
              "layout", box "spring"  
              "curveAll", box true
              "border", box false
              "curve", box 0.25
              "vsize", box 4.0
              "title", box "?" ] |> namedParams

open RProvider.qgraph

R.qgraph ( graph1)

let edge_list = links |> Seq.collect ( fun (a,b) -> [|a;b|]) |> Seq.toArray

let export_edge =
    links 
    |> Seq.map (fun (a,b) -> sprintf "%s\t%s\t%s" a b "undirected") |> Seq.toList

let export2 = "Source\tTarget\tType" :: export_edge


File.WriteAllLines(__SOURCE_DIRECTORY__ + @"\to_net.csv", export2 )



open RProvider.graphics

let ed = ["edges", box edge_list 
          "directed", box false] |>namedParams |> R.graph
R.plot ed









open RProvider.d3Network

let m = R.d3ForceNetwork(links)










let avg_mw =
    results 
    |> Seq.map (fun x -> try
                             x.AverageMolecularWeight


                         with
                         | ex -> -999M
                        
    ) 

let biofluid = 
    results |> Seq.map ( fun x -> x.BiofluidLocations ) |> Seq.collect id


let diseases = 
    results |> Seq.map (fun x -> x.Diseases) |> Seq.collect id

let D = 
    diseases |> Seq.groupBy (fun x -> x.Name ) |> dict


[for d in D -> printfn "%s %A" d.Key d.Value ]

let xml1 = D.["Obesity"]























let D = 
    biofluid |> Seq.groupBy id |> dict

[for d in D -> printfn "%s : %d " d.Key ( Seq.length d.Value) ]










let values = avg_mw |> Seq.map (fun x -> x |> float ) 

values |> Seq.countBy (fun x -> x = -999.)



open FSharp.Charting

Chart.FastLine values






