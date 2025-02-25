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
    tx: Transaction
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

module NodeModel =
    let get (id: NodeId) (repo: NodeModel list) =
        repo |> List.tryFind (fun m -> id = m.Id)
        
    let getTransactionNodes (repo: NodeModel list) =
        repo |> List.filter (_.Metadata.IsCase2)
   
    let getTransactions (repo: NodeModel list) =
        repo
        |> getTransactionNodes
        |> List.map (_.Metadata)
        |> List.map (function
            | (U2.Case2 m) -> m.tx 
            | _ -> failwith "Not possible.")
        
    let getSpenderNode (txid: string) (index: int) (repo: NodeModel list) =
        repo
        |> getTransactions
        |> List.collect (fun tx -> tx.vin |> Array.map(fun inp -> (inp, tx.txid)) |> List.ofArray)
        |> List.filter (fun (inp, _) -> inp.txid = txid && inp.vout = index)
        |> List.map snd
        |> List.tryExactlyOne
        |> Option.bind (fun txid -> get txid repo)
        
type EdgeModel = {
    Id: EdgeId
    From: NodeId
    To: NodeId
    Value: float
    Title: HTMLElement
    Selected: bool
    Marked: bool
    Output: Output
}

module EdgeModel =
    let get (id: EdgeId) (repo: EdgeModel list) =
        repo |> List.tryFind (fun e -> id = e.Id)
        
type GraphModel = {
    Nodes: NodeModel list
    Edges: EdgeModel list
}