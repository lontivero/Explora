module Index

open System.Collections.Generic
open BitcoinRpc
open Explora
open Explora.Api
open Explora.MempoolSpace
open Explora.Types
open Fable.Core
open Fable.Core.JsInterop
open Microsoft.FSharp.Core
open Browser
open Browser.Types
open Vis
open Graph

let requestTransaction (txId: string) (graphModel: GraphModel) =
    promise {
        let txNode = GraphModel.getNode txId graphModel
        if txNode.IsSome then
            return (Ok graphModel)
        else
            let! txResult = getTransaction txId
            return
                txResult
                |> Result.bind(function
                    | ConfirmedTransaction tx ->Ok(addTransactionToGraph tx graphModel)
                    | UnconfirmedTransaction _ -> Error "The requested transaction is still unconfirmed")
    } 
    
let requestSpenderTransaction (txId: string) (index: int) (graphModel: GraphModel) =
    promise {
        let! spenderResult = Api.getSpenderTransaction txId index
        match spenderResult with
        | Ok (SpentConfirmed spender) -> return! requestTransaction spender.txid graphModel
        | Ok (SpentUnconfirmed spender) -> return (Error "The utxo is unconfirmed and the data providers doesn't give us info about unconfirmed elements")
        | Ok SpendingStatus.Unspent -> return (Error "The txo is unspent")
        | Error msg -> return (Error msg)
    }

let shortenString (s: string) (maxLenght: int)=
     match s.Length with
     | n when n <= maxLenght -> s
     | _ -> $"{s.[0..(maxLenght/2)-1]}..{s.[s.Length-(maxLenght/2)-1..]}"

let formatAddress (addr: string option) =
    addr |> Option.defaultValue "--"

let formatBTC (value: float) =
    value.ToString("N8").TrimEnd('0').TrimEnd('.') + " ₿"

let createAddressTableRow address amount id =
    $"""<tr data-address="{id}" class="clickable-row">
        <td class="address">{formatAddress address}</td>
        <td class="amount">{formatAmount amount}</td>
    </tr>"""

let createDetailTableRow detail value =
    $"""<tr>
        <td class="detail-label">{detail}</td>
        <td class="detail-value">{value}</td>
    </tr>"""
    
let createTableSection (title: string) (rows: string) =
    $"""
    <thead>
        <tr>
            <th colspan="2">{title}</th>
        </tr>
    </thead>
    <tbody>
        {rows}
    </tbody>
    """
    
let createTableForTransaction (tx: ConfirmedTransaction) =
    let inputsHtml =
        tx.vin
        |> Seq.map (fun inp -> createAddressTableRow inp.prevOut.scriptPubKey.address inp.prevOut.value inp.txid)
        |> String.concat ""

    let outputsHtml =
        tx.vout
        |> Seq.mapi (fun i out -> createAddressTableRow out.scriptPubKey.address out.value $"{tx.txid}-{i}")
        |> String.concat ""

    let detailsHtml =
        [
            "Hash", $"<a href=https://mempool.space/tx/{tx.txid}>{tx.txid}</a>"
            "Version", string tx.version
            "Volume", tx.vin |> Seq.sumBy (_.prevOut.value) |> formatAmount
            "Fee", formatAmount tx.fee
            "BlockHash",  $"<a href=https://mempool.space/block/{tx.blockhash}>{tx.blockhash}</a>"
            "BlockTime", string tx.blocktime
            "Vsize", string tx.vsize
            "Locktime", string tx.locktime
        ] 
        |> Seq.map (fun (d, v) -> createDetailTableRow d v)
        |> String.concat ""
        
        
    $"""
    <table class="tx-details">
        {createTableSection $"Details" detailsHtml}
    </table>
    <table class="tx-details">
        {createTableSection $"Inputs ({tx.vin.Length})" inputsHtml}
        {createTableSection $"Outputs ({tx.vout.Length})" outputsHtml}
    </table>
    """

// Initialize application
let init() =
    
    // Create graph container
    let container = document.getElementById("mynetwork")
    let txDetails = document.getElementById("tx-details")
    // Create HTML structure
    let graph = createGraph container
    let graphModelHistory = Stack<GraphModel>()
    graphModelHistory.Push { Nodes = []; Edges = [] }
    
    let getCurrentGraphModel () =
        graphModelHistory.Peek ()
        
    let updateGraphModel (newGraphModelResult : Result<GraphModel,string>) =
        match newGraphModelResult with
        | Ok newGraphModel ->
            graphModelHistory.Push newGraphModel
            update graph newGraphModel
        | Error msg -> window.alert msg
        
    let addTxsAndTxosToGraph (tx: ConfirmedTransaction) =
        promise {
            let mutable graphModel = getCurrentGraphModel()
            for inp in tx.vin do
                let! txResult = requestTransaction inp.txid graphModel
                graphModel <-
                    match txResult with
                    | Ok newGraphModel ->
                        update graph newGraphModel
                        newGraphModel
                    | _ -> graphModel
            updateGraphModel (Ok graphModel)
        } |> Promise.start
         
        
    let requestTransaction txid graphModel =
        promise {
            let! newGraphModelResult = requestTransaction txid graphModel
            updateGraphModel(newGraphModelResult)
        } |> Promise.start
        
    let requestSpenderTransaction (txId: string) (index: int) (graphModel: GraphModel) =
        promise {
            let! newGraphModelResult = requestSpenderTransaction txId index graphModel
            updateGraphModel(newGraphModelResult)
        } |> Promise.start
        
        
    let setupRowClickHandlers (graph : Graph) =
       document
           .querySelectorAll(".clickable-row")
           |> JS.Constructors.Array.from 
           |> Seq.cast<HTMLElement>
           |> Seq.iter (fun row ->
               row.addEventListener("click", fun e ->
                   let id = row.getAttribute("data-address")
                   printfn "Clicked row with id: %s" id
                   match id.Split("-") with
                   | [|txid; vout|] -> requestSpenderTransaction txid (int vout) (getCurrentGraphModel())
                   | [|txid|] -> requestTransaction txid (getCurrentGraphModel()) 
                   | _ -> failwith "WFT!!"
               )) 
        
    // Add event listeners
    document.getElementById("search-button").onclick <- fun _ ->
        let txId = (document.getElementById("txId") :?> HTMLInputElement).value
        requestTransaction txId (getCurrentGraphModel())
    container.onkeypress <- fun k ->
        match k.key with
        | "m" -> updateGraphModel (Ok (toggleMarkSelected (getCurrentGraphModel())))
        | "z" ->
            if graphModelHistory.Count > 0 then
                graphModelHistory.Pop() |> ignore
                update graph (getCurrentGraphModel())
        | _ -> ()
    graph.network.on(NetworkEvents.HoverNode, fun ps -> ())
    graph.network.on(NetworkEvents.BlurNode, fun ps -> ())
    graph.network.on(NetworkEvents.DragStart, fun ps ->
        graph.network.stopSimulation()
        graph.network?physics?options?enabled <- false
        )
    graph.network.on(NetworkEvents.DragEnd, fun ps ->
        graph.network?physics?options?enabled <- true
        graph.network.startSimulation()
        )
    graph.network.on(NetworkEvents.SelectNode, fun ps ->
        let txid = ps?nodes |> Array.head
        getCurrentGraphModel()
        |> GraphModel.getNode txid
        |> Option.iter(fun node ->
            match node.Metadata with
            | U2.Case2 txMetadata -> 
                txDetails.innerHTML <- createTableForTransaction txMetadata.tx
                if txDetails.classList.contains("hidden") then
                    txDetails.classList.toggle("hidden") |> ignore
                setupRowClickHandlers graph
                updateGraphModel (Ok (selectNode node (getCurrentGraphModel())))
            | _ -> ()) 
        )
    graph.network.on(NetworkEvents.DeselectNode, fun ps ->
        if txDetails.classList.contains("hidden") = false then
            txDetails.classList.toggle("hidden") |> ignore
        updateGraphModel (Ok (deselectAll (getCurrentGraphModel())))
    )
    graph.network.on(NetworkEvents.DoubleClick,
       function
       | Some o ->
           let ps = o :?> {| nodes : string array |}
           if ps.nodes.Length > 0 then
              let graphModel = getCurrentGraphModel()
              let clickedNodeId = ps.nodes[0]
              graphModel
              |> GraphModel.getNode clickedNodeId
              |> Option.iter (fun clickedNodeModel ->
                  match clickedNodeModel.Metadata with
                  | U2.Case2 txMetadata -> addTxsAndTxosToGraph txMetadata.tx
                  | _ -> ())
       | None -> ())

// Start the application
init()
