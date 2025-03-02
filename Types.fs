module Explora.Types

open BitcoinRpc
open Browser.Types
open Fable.Core

type OutputMetadata = {
    txid: string
    index: int
    output: Output 
}

type TxMetadata = {
    tx: ConfirmedTransaction
}
    
type [<AllowNullLiteral>] GraphNode =
    inherit Vis.Node
    abstract metadata: U2<OutputMetadata, TxMetadata> with get, set

type NodeId = string
type EdgeId = string

type NodeModelKind =
    | Tx
    | Txo
    
type NodeModel = {
    Id: NodeId
    Title: HTMLElement
    Selected: bool
    Marked: bool
    Kind: NodeModelKind
    Metadata: U2<OutputMetadata, TxMetadata>
}

type EdgeModel = {
    Id: EdgeId
    From: NodeId
    To: NodeId
    Value: float
    Title: HTMLElement
    Selected: bool
    Marked: bool
    OutputData: OutputMetadata
}

type GraphModel = {
    Nodes: NodeModel list
    Edges: EdgeModel list
}

module GraphModel =
    let getNode (id: NodeId) (g: GraphModel) =
        g.Nodes |> List.tryFind (fun m -> id = m.Id)
        
    let getEdge (id: EdgeId) (g: GraphModel) =
        g.Edges |> List.tryFind (fun e -> id = e.Id)
        
    let getTransactionNodes (g: GraphModel) =
        g.Nodes |> List.filter (_.Metadata.IsCase2)
   
    let getTransactions (g: GraphModel) =
        g
        |> getTransactionNodes
        |> List.map (_.Metadata)
        |> List.map (function
            | (U2.Case2 m) -> m.tx 
            | _ -> failwith "Not possible.")
        
    let getSpenderNode (txid: string) (index: int) (g: GraphModel) =
        g
        |> getTransactions
        |> List.collect (fun tx -> tx.vin |> Array.map(fun inp -> (inp, tx.txid)) |> List.ofArray)
        |> List.filter (fun (inp, _) -> inp.txid = txid && inp.vout = index)
        |> List.map snd
        |> List.tryExactlyOne
        |> Option.bind (fun txid -> getNode txid g)
        
    let getEdgesConnectedTo (id: NodeId) (g: GraphModel) =
        g.Edges |> List.filter (fun e -> e.To = id || e.From = id)
        
    let getAddressReused (g: GraphModel) =
        g.Edges
        |> List.choose (_.OutputData.output.scriptPubKey.address)
        |> List.groupBy (id)
        |> List.filter (fun (_, es) -> List.length es > 1) 
        |> List.map fst
