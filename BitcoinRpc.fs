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

type ConfirmedTransaction = {
    txid: string
    version: int
    size: int
    vsize: int
    weight: int
    locktime: int 
    vin: Input[]
    vout: Output[]
    fee: float
    blockhash: string
    confirmations: int
    time: DateTimeOffset
    blocktime: DateTimeOffset
}

type UnconfirmedTransaction = {
    txid: string
    version: int
    size: int
    vsize: int
    weight: int
    locktime: int 
    vout: Output[]
}

type Transaction =
    | ConfirmedTransaction of ConfirmedTransaction
    | UnconfirmedTransaction of UnconfirmedTransaction
    
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
    
let ConfirmedTransactionDecoder : Decoder<ConfirmedTransaction> =
   Decode.object (fun get -> {
        txid = get.Required.Field "txid" Decode.string
        version = get.Required.Field "version" Decode.int
        size = get.Required.Field "size" Decode.int
        vsize = get.Required.Field "vsize" Decode.int
        weight = get.Required.Field "weight" Decode.int
        locktime = get.Required.Field "locktime" Decode.int
        vin = get.Required.Field "vin" (Decode.array InputDecoder)
        vout = get.Required.Field "vout" (Decode.array OutputDecoder)
        blockhash = get.Required.Field "blockhash" Decode.string
        fee = get.Required.Field "fee" Decode.float
        time = get.Required.Field "time" unixDateTimeDecoder
        confirmations = get.Required.Field "confirmations" Decode.int
        blocktime = get.Required.Field "blocktime" unixDateTimeDecoder
   })
   
let UnconfirmedTransactionDecoder : Decoder<UnconfirmedTransaction> =
   Decode.object (fun get -> {
        txid = get.Required.Field "txid" Decode.string
        version = get.Required.Field "version" Decode.int
        size = get.Required.Field "size" Decode.int
        vsize = get.Required.Field "vsize" Decode.int
        weight = get.Required.Field "weight" Decode.int
        locktime = get.Required.Field "locktime" Decode.int
        vout = get.Required.Field "vout" (Decode.array OutputDecoder)
   })

let TransactionDecoder : Decoder<Transaction> =
   Decode.optional "confirmations" Decode.int
   |> Decode.andThen(function
       | Some _ -> ConfirmedTransactionDecoder |> Decode.map ConfirmedTransaction
       | None -> UnconfirmedTransactionDecoder |> Decode.map UnconfirmedTransaction)
   
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



