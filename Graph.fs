module Explora.Graph

open BitcoinRpc
open Browser
open Browser.Types
open Explora.Types
open Fable.Core
open Microsoft.FSharp.Core
open Vis
open Fable.Core.JsInterop

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
[<Literal>]
let markedColor = "#ff5722"
[<Literal>]
let ignoredColor ="#333333"

type Graph = {
    network: Network
    data: Data
}

[<Global>]
let vis : IExports = jsNative

let formatAmount value = $"{value} BTC"

let createTitle (item : U2<TxMetadata, OutputMetadata>) =
    let row (label, value) = $"<tr><td>{label}:</td><td>{value}</td></tr>"
    let fields =
        match item with
        | U2.Case1 meta -> [
            "Hash", meta.tx.txid
            "Version", string meta.tx.version
            "Value", meta.tx.vin |> Seq.sumBy (_.prevOut.value) |> formatAmount
            "Vsize", string meta.tx.vsize
            "Locktime", string meta.tx.locktime
            "Fee", formatAmount meta.tx.fee
            "Confirmations", string meta.tx.confirmations
            "BlockHash", meta.tx.blockhash
            "BlockTime", meta.tx.blocktime.ToString()
            "Inputs", string meta.tx.vin.Length
            "Outputs", string meta.tx.vout.Length ]
        | U2.Case2 meta ->[
            "Address", meta.output.scriptPubKey.address |> Option.defaultValue "--"
            "Amount", formatAmount meta.output.value
            "ScriptType", meta.output.scriptPubKey.scriptType 
            "PrvTx", meta.txid
            "PrevIx", string meta.index ]
    let rows = fields |> List.map row |> String.concat ""
    let div = document.createElement("div")
    div.innerHTML <- $"<table><tbody>{rows}</tbody></table>"
    div
   
let createTxNode (tx: ConfirmedTransaction) =
    {
        Id = tx.txid
        Title = createTitle !^{ tx = tx }
        Metadata = !^{ tx = tx}
        Selected = false
        Highlighted = false
        Marked = false
        Kind = NodeModelKind.Tx
    }
    
let createTxoNode (txo: OutputMetadata) =
    {
        Id = $"{txo.txid}-{txo.index}"
        Title = createTitle !^txo
        Selected = false
        Highlighted = false
        Marked = false
        Metadata = !^txo
        Kind = NodeModelKind.Txo
    }
   
let createEdge (id: string) (from: NodeId) (_to: NodeId) (txo: OutputMetadata)=
    {
       Id = id 
       From = from
       To = _to
       Value = txo.output.value
       Title = createTitle !^txo
       Highlighted = false
       Marked = false
       Selected = false
       OutputData = txo
    }
    
let deselectAll graphModel =
    { graphModel with
        Nodes = graphModel.Nodes |> List.map (fun n -> {n with Selected = false; Highlighted = false})
        Edges = graphModel.Edges |> List.map (fun n -> {n with Selected = false; Highlighted = false})
    }
    
let selectNode (node: NodeModel) graphModel =
    let graphModel = deselectAll graphModel  
    let nodeToUpdate = {node with Selected = true; Highlighted = true}
    let untouchedNodes  = graphModel.Nodes |> List.filter (fun n -> n.Id <> nodeToUpdate.Id)
    let edgesToUpdate = graphModel |> GraphModel.getEdgesConnectedTo nodeToUpdate.Id |> List.map (fun edge -> {edge with Highlighted = true })
    let untouchedEdges = graphModel.Edges |> List.filter (fun e -> List.exists (fun e2 -> e2.Id = e.Id) edgesToUpdate |> not)
    { graphModel with
        Nodes = nodeToUpdate :: untouchedNodes
        Edges = edgesToUpdate @ untouchedEdges
    }
    
// unused
let selectEdge (edge: EdgeModel) graphModel =
    let graphModel = deselectAll graphModel  
    let edgeToUpdate = {edge with Selected = true; Highlighted = true}
    let untouched  = graphModel.Edges |> List.filter (fun n -> n.Id <> edgeToUpdate.Id)
    { graphModel with
        Edges = edgeToUpdate :: untouched
    }
    
let addTransactionToGraph (tx: ConfirmedTransaction) (graph: GraphModel) =
    let node = createTxNode tx
    
    let edgesFromPreviousNodes =
        tx.vin
        |> Array.map (fun input -> 
            graph
            |> GraphModel.getNode input.txid
            |> Option.map (fun prevTxNode ->
                let from = prevTxNode.Id
                let _to  = tx.txid
                let id = $"{from}-{_to}-{input.vout}"
                createEdge id from _to {output = input.prevOut; txid = input.txid; index = input.vout }))
       |> Array.choose id
       |> List.ofArray
       
    let edgesToNextNodes =
        tx.vout
        |> List.ofArray
        |> List.mapi (fun i output ->
            graph
            |> GraphModel.getSpenderNode tx.txid i
            |> Option.map(fun nextTxNode ->
                let from = tx.txid
                let _to  = nextTxNode.Id
                let id = $"{from}-{i}-{_to}"
                createEdge id from _to {output = output; txid = tx.txid; index = i}))
        |> List.choose id
       
    { graph with
        Nodes = node :: graph.Nodes 
        Edges = List.concat [graph.Edges; edgesFromPreviousNodes; edgesToNextNodes]
    }
    
    //selectNode node newGraph
   
let addTxosToGraph (tx: ConfirmedTransaction) (graph: GraphModel) =
    let createdTxo = tx.vout |> Array.mapi (fun i x -> { txid = tx.txid; index = i; output = x })
    let txos =
        createdTxo
        |> Array.map createTxoNode
        |> List.ofArray
       
    let edgesFromTx =
        createdTxo
        |> Array.map (fun txo ->
            graph
            |> GraphModel.getNode txo.txid
            |> Option.map(fun txNode ->
                let from = txNode.Id
                let _to  = $"{txo.txid}-{txo.index}"
                let id = $"{from}-{_to}"
                createEdge id from _to txo))
        |> Array.choose id
        |> List.ofArray
    
    { graph with
        Nodes = graph.Nodes @ txos
        Edges = List.concat [graph.Edges; edgesFromTx]
    }

let createGraph (container : HTMLElement) =
    let nodes = vis.DataSet.Create<Node>()
    let edges = vis.DataSet.Create<Edge>()
    let networkData = jsOptions<Data>(
        fun data ->
            data.nodes <- nodes
            data.edges <- edges
    ) 
    let font = jsOptions<Font>(fun f ->
        f.face <- Some "Roboto"
        f.size <- Some (float 8)
        f.align <- Some "bottom"
        f.color <- Some "#aaaaaa"
        f.strokeWidth <- Some (float 0))
   
    let edgeLabelScaling = jsOptions<OptionsScaling>(fun s ->
            s.label <- Some !^(jsOptions<OptionsScalingLabel>( fun l ->  l.enabled <-  Some false)))
    
    let nodeOptions = jsOptions<NodeOptions>(fun n -> n.shape <- Some "dot")
    let edgeOptions = jsOptions<EdgeOptions>(fun e ->
        e.arrows <- Some !^"middle"
        e.font <- Some !^font
        e.scaling <- Some edgeLabelScaling
        e.smooth <- Some !^true)
    
    let options = jsOptions<Options>(
        fun opts ->
            opts.interaction <- Some {|
                                        hover = true
                                        selectConnectedEdges = false
                                        |}
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
    }

// ---------------------------------------------------
let updateNode (graph: Graph) (nodeModel: NodeModel) =
    let node =
        graph.data.nodes.get(U2.Case1 nodeModel.Id)
        |> Option.defaultWith (fun ()-> jsOptions<Node>(fun o -> o.id <- Some !^nodeModel.Id ))
    
    let getShapeByLocktime locktime =
        match locktime with
        | 0 -> "dot"
        | t when t < 500_000_000 -> "square"
        | _ -> "hexagon"

    let shape =
        match nodeModel.Metadata with
        | U2.Case1 meta -> "diamond"
        | U2.Case2 meta -> getShapeByLocktime meta.tx.locktime

    let color =
        match nodeModel.Marked, nodeModel.Highlighted with
        | true, _ -> Some !^markedColor
        | _, true -> Some !^"#10cfb5"
        | _, false -> Some !^"#1984fc"
        
    let value =
        match nodeModel.Metadata with
        | U2.Case1 outputMetadata -> outputMetadata.output.value
        | U2.Case2 txMetadata -> txMetadata.tx.vin |> Array.sumBy (_.prevOut.value)
        
    node.title <- Some !^nodeModel.Title
    node.shape <- Some shape
    node.value <- Some value
    node.color <- color 
    node
    
let updateEdge (graph: Graph) (anySelected: bool) (reusedAddresses: string list) (edgeModel: EdgeModel) =
    let edge =
        graph.data.edges.get(U2.Case1 edgeModel.Id)
        |> Option.defaultWith (fun ()-> jsOptions<Edge>(fun o -> o.id <- Some !^edgeModel.Id ))
    
    let mutable dashes =
       match edgeModel.OutputData.output.scriptPubKey.scriptType with
       | "multisig" ->  U2.Case2 (ResizeArray([15.0; 15.0; 5.0]))
       | "scripthash" ->   U2.Case2 (ResizeArray([40.0; 5.0; 5; 5]))
       | "witness_v0_scripthash" ->   U2.Case2 (ResizeArray([30.0; 15.0; 5.0; 15.0]))
       | "nulldata" ->   U2.Case2 (ResizeArray([10.0; 10.0; 10; 10]))
       | _ -> U2.Case1 false

    let color =
       match edgeModel.Marked, (edgeModel.Highlighted || not anySelected), edgeModel.OutputData.output.scriptPubKey.scriptType with
       | true, _, _ -> markedColor
       | _, true, "pubkeyhash" -> pkhColor
       | _, true, "witness_v0_keyhash" -> wkhColor
       | _, true, "witness_v1_taproot" -> trColor
       | _, true, "nulldata" -> nullDataColor
       | _, true, "witness_unknown" -> unknownWitnessColor
       | _, true, "witness_v0_scripthash"
       | _, true, "scripthash" -> shColor 
       | _, true, "multisig" -> multiColor
       | _, true, _ -> unknown
       | _, false, _ -> ignoredColor
    
    let label =
       edgeModel.OutputData.output.scriptPubKey.address
       |> Option.bind (fun addr ->
            match List.contains addr reusedAddresses with
            | true -> Some "reused"
            | false -> if edgeModel.Highlighted then Some (string edgeModel.OutputData.index) else None )
      
    edge.from <- Some !^edgeModel.From
    edge.``to`` <- Some !^edgeModel.To
    edge.title <- Some !^edgeModel.Title
    edge.value <- Some edgeModel.OutputData.output.value
    edge.arrows <- Some !^"to"
    edge.dashes <- Some dashes
    edge.color <- Some !^color
    edge.arrows <- Some !^"middle"
    edge.label <- label
    edge

let getUnexistentNodes graph graphModel =
    let nodeIds = graphModel.Nodes |> List.map (fun x -> !^x.Id)
    let allNodeIds = graph.data.nodes.getIds()
    allNodeIds |> Seq.except nodeIds
    
let getUnexistentEdges graph graphModel =
    let edgesIds = graphModel.Edges |> List.map (fun x -> !^x.Id)
    let allEdgeIds = graph.data.edges.getIds()
    allEdgeIds |> Seq.except edgesIds
    
let update graph graphModel =
    let updatedNodes = ResizeArray ( graphModel.Nodes |> List.map(updateNode graph)  )
    let nodesToRemove = ResizeArray (getUnexistentNodes graph graphModel)
    let isAnySelected = graphModel.Nodes |> List.exists (_.Selected)
    let reusedAddrs = GraphModel.getAddressReused graphModel
    let updatedEdges = ResizeArray ( graphModel.Edges |> List.map(updateEdge graph isAnySelected reusedAddrs)  )
    let edgesToRemove = ResizeArray (getUnexistentEdges graph graphModel)
    graph.data.nodes.remove (!^nodesToRemove) |> ignore
    graph.data.edges.remove (!^edgesToRemove) |> ignore
    graph.data.nodes.update (!^updatedNodes) |> ignore
    graph.data.edges.update (!^updatedEdges) |> ignore

let toggleMarkSelected graphModel =
    let nodesToUpdate, restNodes = graphModel.Nodes |> List.partition _.Selected
    let updatedNodes = nodesToUpdate |> List.map (fun n -> { n with Marked = not n.Marked }) 
    let edgesToUpdate, restEdges = graphModel.Edges |> List.partition _.Selected
    let updatedEdges = edgesToUpdate |> List.map (fun e -> { e with Marked = not e.Marked }) 
    
    { graphModel with
        Nodes = updatedNodes @ restNodes
        Edges = updatedEdges @ restEdges
    }