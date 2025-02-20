module Explora.Graph

open BitcoinRpc
open Browser
open Browser.Types
open Explora.Types
open Fable.Core
open Microsoft.FSharp.Core
open Vis
open Fable.Core.JsInterop

type Graph = {
    network : Network
    data : Data
    scriptPubKeys:  ResizeArray<string * IdType>
}

//[<Import("*", "vis")>]
[<Global>]
let vis : IExports = jsNative

[<Literal>]
let pkhColor = "#fea1c9"
[<Literal>]
let wkhColor = "#36d38c"
[<Literal>]
let trColor = "#24d442"
[<Literal>]
let nullDataColor = "#f3661e"
[<Literal>]
let unknownWitnessColor = "#bd7fbf"
[<Literal>]
let shColor = "#fefe5e"
[<Literal>]
let multiColor = "#5d87fd"
[<Literal>]
let unknown = "#ff0000"

let addNode (node: Node) (data : Data) =
    if Option.isSome node.id then
        let id = Option.get node.id
        if Option.isNone (data.nodes.get(id)) then
            data.nodes.add(!^node) |> ignore

let addEdge (edge: Edge) (data : Data) =
    data.edges.add(!^edge) |> ignore

let addAddress address txoNodeId graph =
    graph.scriptPubKeys.Add (address, txoNodeId)
    
let createNode id title metadata =
    let getShapeByLocktime locktime =
        match locktime with
        | 0 -> "dot"
        | t when t < 500_000_000 -> "square"
        | _ -> "hexagon"

    let shape =
        match metadata with
        | U3.Case3 meta -> getShapeByLocktime meta.tx.locktime
        | _ -> "dot"

    jsOptions<GraphNode>(fun n -> 
       n.id <- Some id
       n.title <- Some title
       n.metadata <- metadata
       n.shape <- Some shape)
  
let createEdge from ``to`` (value: float option) (arrows: string option) (scriptType: string) =
   let mutable dashes =
       match scriptType with
       | "multisig" ->  U2.Case2 (ResizeArray([15.0; 15.0; 5.0]))
       | "scripthash" ->   U2.Case2 (ResizeArray([40.0; 5.0; 5; 5]))
       | "witness_v0_scripthash" ->   U2.Case2 (ResizeArray([30.0; 15.0; 5.0; 15.0]))
       | "nulldata" ->   U2.Case2 (ResizeArray([10.0; 10.0; 10; 10]))
       | _ -> U2.Case1 false

    //      scripthash, multisig, nulldata, witness_v0_scripthash, witness_v0_keyhash, witness_v1_taproot, witness_unknown 
   let color =
       match scriptType with
       | "pubkeyhash" -> pkhColor
       | "witness_v0_keyhash" -> wkhColor
       | "witness_v1_taproot" -> trColor
       | "nulldata" -> nullDataColor
       | "witness_unknown" -> unknownWitnessColor
       | "witness_v0_scripthash"
       | "scripthash" -> shColor 
       | "multisig" -> multiColor
       | _ -> unknown
       
   jsOptions<Edge>(fun e ->
       e.from <- from
       e.``to`` <- ``to``
       e.value <- value
       e.arrows <- arrows |> Option.map U2.Case1
       e.dashes <- Some dashes
       e.color <- Some !^color)

let formatAmount value = $"{value} BTC"

let createTitle (item : U3<Transaction, Input, Output>) =
    let row (label, value) = $"<tr><td>{label}:</td><td>{value}</td></tr>"
    let fields =
        match item with
        | U3.Case1 tx -> [
            "Hash", tx.txid
            "Version", string tx.version
            "Fee", formatAmount tx.fee
            "Vsize", string tx.vsize
            "Locktime", string tx.locktime
            "BlockHash", tx.blockhash
            "Inputs", string tx.vin.Length
            "Outputs", string tx.vout.Length ]
        | U3.Case2 input -> [
            "PrevTx", input.txid
            "PrevIx", string input.vout
            "Address", input.prevOut.scriptPubKey.address |> Option.defaultValue "--"
            "Amount", formatAmount input.prevOut.value
            "ScriptType", input.prevOut.scriptPubKey.scriptType ]
        | U3.Case3 output ->[
            "Address", output.scriptPubKey.address |> Option.defaultValue "--"
            "Amount", formatAmount output.value
            "ScriptType", output.scriptPubKey.scriptType ]
    let rows = fields |> List.map row |> String.concat ""
    let div = document.createElement("div")
    div.innerHTML <- $"<table><tbody>{rows}</tbody></table>"
    div
    
let createInputNode (input: Input) =
    let nodeId = $"{input.txid}-%d{input.vout}"
    let node = createNode !^nodeId !^(createTitle !^input) !^{ input = input }
    node
    
let createOutputNode (output: Output) txid index =
    let nodeId = $"{txid}-%d{index}"
    let metadata = {
        txid = txid
        index = index
        output = output
    }
    let node = createNode !^nodeId !^(createTitle !^output) !^metadata
    node
    
let createTxNode (tx: Transaction) =
    createNode !^tx.txid !^(createTitle !^tx) !^{ tx = tx }
   
// Network visualization
let addTransactionToGraph (tx: Transaction) (graph: Graph) =
    // Add central transaction node
    let node = createTxNode tx
    addNode node graph.data
    
    // Add inputs
    for input in tx.vin do
        let node = createInputNode input
        addNode node graph.data
        let edge = createEdge node.id (Some !^tx.txid) (Some input.prevOut.value) (Some "to") input.prevOut.scriptPubKey.scriptType
        addEdge edge graph.data

    // Add outputs
    for index, output in Array.indexed tx.vout do 
        let node = createOutputNode output tx.txid index
        addNode node graph.data
        let edge = createEdge (Some !^tx.txid) node.id (Some output.value) (Some "to") output.scriptPubKey.scriptType
        addEdge edge graph.data
        
        output.scriptPubKey.address |> Option.iter( fun addr -> addAddress addr node.id.Value graph)

let createGraph (container : HTMLElement) =
    let nodes = vis.DataSet.Create<Node>()
    let edges = vis.DataSet.Create<Edge>()
    let networkData = jsOptions<Data>(
        fun data ->
            data.nodes <- nodes
            data.edges <- edges
    ) 
    
    let nodeOptions = jsOptions<NodeOptions>(fun n -> n.shape <- Some "dot")
    let edgeOptions = jsOptions<EdgeOptions>(fun e ->
        e.arrows <- Some !^{|
          from = None
          ``to`` = None
          middle = Some (!^(jsOptions<ArrowHead>(fun a -> a.scaleFactor <- Some 0.1; a.enabled <- Some true)))
           |}
        e.smooth <- Some !^true)
    
    let options = jsOptions<Options>(
        fun opts ->
            opts.interaction <- Some {| hover = true |}
            opts.nodes <- Some nodeOptions
            opts.edges <- Some edgeOptions
            opts.physics <- Some {|
                barnesHut = {|
                    gravitationalConstant = -5000
                    centralGravity= 0.12
                    springLength= 150
                |}
                stabilization = {|
                    iterations = 55
                    fit = false
                |}
            |}
        )
    
    // Create the network
    let network = vis.Network.Create(container, networkData, options)
    {
        network = network
        data = networkData
        scriptPubKeys = ResizeArray<string * IdType>()
    }
