module Index

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

let requestTransaction (txId: string) (graph: Graph) =
    promise {
        let! txResult = getTransaction txId
        match txResult with
        | Ok tx ->
            if graph.data.nodes.get(U2.Case1 tx.txid).IsNone then
                addTransactionToGraph tx graph
        | Error msg -> window.alert msg
    } |> Promise.start
    
let requestSpenderTransaction (txId: string) (index: int) (graph: Graph) =
    promise {
        let! spenderResult = Api.getSpenderTransaction txId index
        match spenderResult with
        | Ok (SpentConfirmed spender) -> 
            requestTransaction spender.txid graph
        | Ok (SpentUnconfirmed spender) ->
            window.alert "The utxo is unconfirmed and the data providers doesn't give us info about unconfirmed elements"
        | Ok SpendingStatus.Unspent ->
            window.alert "The txo is unspent"
        | Error msg ->
            window.alert msg
    } |> Promise.start
    
let shortenString (s: string) (maxLenght: int)=
     match s.Length with
     | n when n <= maxLenght -> s
     | _ -> $"{s.[0..(maxLenght/2)-1]}..{s.[s.Length-(maxLenght/2)-1..]}"
     
let formatAddress (addr: string option) =
    addr |> Option.defaultValue "--"
                          
let formatBTC (value: float) =
    value.ToString("N8").TrimEnd('0').TrimEnd('.') + " ₿" 

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
    
// Initialize application
let init() =
    
    // Create graph container
    let container = document.getElementById("mynetwork")
    let txDetails = document.getElementById("tx-details")
    // Create HTML structure
    let graph = createGraph container
    // Add event listeners
    document.getElementById("search-button").onclick <- fun _ ->
        let txId = (document.getElementById("txId") :?> HTMLInputElement).value
        requestTransaction txId graph
    graph.network.on(NetworkEvents.HoverNode, fun ps -> ())
    graph.network.on(NetworkEvents.BlurNode, fun ps -> ())
    graph.network.on(NetworkEvents.DragStart, fun ps ->
        graph.network.stopSimulation()
        graph.network?physics?options?enabled <- false
        )
    graph.network.on(NetworkEvents.SelectNode, fun ps ->
        let txid = ps?nodes |> Array.head
        let tx = graph.txs.Find(fun tx -> tx.txid = txid)
        let inputsHtml =
            tx.vin
            |> Seq.map (fun inp -> 
                $"""<tr>
                    <td class="address">{formatAddress inp.prevOut.scriptPubKey.address}</td>
                    <td class="amount">{formatBTC inp.prevOut.value}</td>
                </tr>""")
            |> String.concat ""

        let outputsHtml =
            tx.vout
            |> Seq.map (fun out -> 
                $"""<tr>
                    <td class="address">{formatAddress out.scriptPubKey.address}</td>
                    <td class="amount">{formatBTC out.value}</td>
                </tr>""")
            |> String.concat ""

        txDetails.innerHTML <- 
            $"""
            <table class="tx-details">
                {createTableSection $"Inputs ({tx.vin.Length})" inputsHtml}
                {createTableSection $"Outputs ({tx.vout.Length})" outputsHtml}
            </table>
            """
        txDetails.classList.toggle("hidden") |> ignore 
        )
    graph.network.on(NetworkEvents.DeselectNode, fun ps ->
        txDetails.classList.toggle("hidden") |> ignore
        )
    graph.network.on(NetworkEvents.DragEnd, fun ps ->
        graph.network?physics?options?enabled <- true
        graph.network.startSimulation()
        )
    graph.network.on(NetworkEvents.DoubleClick,
               function
               | Some o ->
                   let ps = o :?> {| nodes : string array |}
                   if ps.nodes.Length > 0 then
                      let clickedNodeId = ps.nodes[0]
                      let clickedNode = graph.data.nodes.get(U2.Case1 clickedNodeId) 
                      match clickedNode with
                      | Some n ->
                          let gn = n :?> GraphNode
                          match gn.metadata with
                          | U3.Case1 inputMetadata -> requestTransaction inputMetadata.input.txid graph
                          | U3.Case2 outputMetadata -> requestSpenderTransaction outputMetadata.txid outputMetadata.index graph
                          | U3.Case3 txMetadata -> ()
                      | None -> ()
               | None -> ())

// Start the application
init()
