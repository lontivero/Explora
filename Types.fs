module Explora.Types

open BitcoinRpc
open Fable.Core

type InputMetadata = {
    input: Input
}

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
    abstract metadata: U3<InputMetadata, OutputMetadata, TxMetadata> with get, set

