module Explora.Api

open BitcoinRpc

let getTransaction (txId: string)  =
    promise {
        try
            let! response = makeRpcCall "getrawtransaction" [| txId; 3 |]
            return Ok response
        with ex ->
            return Error $"Failed to fetch transaction: %s{ex.Message}"
    }
        
let getSpenderTransaction (txId: string) (index: int) =
    promise {
        try
            let! response = MempoolSpace.getSpenderTransaction txId index
            return Ok response
        with ex ->
            return Error $"Failed to fetch spender tx: %s{ex.Message}"
    }