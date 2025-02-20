module BitcoinRpc

open System
open Microsoft.FSharp.Core
open Thoth.Fetch
open Thoth.Json


type ScriptPubKey = {
    asm : string
    address: string option
    scriptType: string
}

type Output = {
    value: float
    scriptPubKey: ScriptPubKey
}

type Input = {
    txid: string
    vout: int
    prevOut: Output
}

type Transaction = {
    txid: string
    version: int
    blockhash: string
    confirmations: int
    time: DateTimeOffset
    blocktime: DateTimeOffset
    fee: float
    size: int
    vsize: int
    weight: int
    locktime: int 
    vin: Input[]
    vout: Output[]
}


let unixDateTimeDecoder : Decoder<DateTimeOffset> =
    Decode.int64 |> Decode.map (DateTimeOffset.FromUnixTimeSeconds)
    
let scriptPubKeyDecoder =
    Decode.object( fun get -> {
       asm = get.Required.Field "asm" Decode.string
       address = get.Optional.Field "address" Decode.string
       scriptType = get.Required.Field "type" Decode.string
    });
    
let OutputDecoder =
    Decode.object(fun get -> {
        value = get.Required.Field "value" Decode.float
        scriptPubKey = get.Required.Field "scriptPubKey" scriptPubKeyDecoder
    })
    
let InputDecoder =
    Decode.object(fun get -> {
        txid = get.Required.Field "txid" Decode.string
        vout = get.Required.Field "vout" Decode.int
        prevOut = get.Required.Field "prevout" OutputDecoder
    })
    
let TransactionDecoder =
   Decode.object (fun get -> {
        txid = get.Required.Field "txid" Decode.string
        version = get.Required.Field "version" Decode.int
        blockhash = get.Required.Field "blockhash" Decode.string
        confirmations = get.Required.Field "confirmations" Decode.int
        time = get.Required.Field "time" unixDateTimeDecoder
        blocktime = get.Required.Field "blocktime" unixDateTimeDecoder
        fee = get.Required.Field "fee" Decode.float
        size = get.Required.Field "size" Decode.int
        vsize = get.Required.Field "vsize" Decode.int
        weight = get.Required.Field "weight" Decode.int
        locktime = get.Required.Field "locktime" Decode.int
        vin = get.Required.Field "vin" (Decode.array InputDecoder)
        vout = get.Required.Field "vout" (Decode.array OutputDecoder)
   }) 

type RpcResponse<'T> = {
    result: 'T
    error: string option
    id: string
}

let RpcErrorDecoder =
    Decode.field "error" Decode.string |> Decode.andThen (fun e -> Decode.fail e)

let RpcSuccessDecoder =
    Decode.field "result" TransactionDecoder

let RpcResponseDecoder<'T> =
    Decode.oneOf [
        RpcErrorDecoder
        RpcSuccessDecoder
    ]
    

// Bitcoin RPC functions
let rpcUrl = "https://rpc.ankr.com/btc"
//let rpcUser = "bitcoinuser"
//let rpcPassword = "bitcoinpassword"

let makeRpcCall (method: string) (parameters: obj[]) =
    let headers =  [
        Fetch.Types.ContentType "application/json"
        //Fetch.Types.Authorization (sprintf "Basic %s" (window.btoa(sprintf "%s:%s" rpcUser rpcPassword)))
    ]
    
    let request = {|
        jsonrpc = "1.0"
        id = "1"
        method = method
        params = parameters
    |}
    
    promise {
        return! Fetch.post(rpcUrl, request, headers = headers, decoder = RpcResponseDecoder)
    }



