module Explora.MempoolSpace

open Fable.Core
open Thoth.Fetch
open Thoth.Json

type SpentUnconfirmed = {
    txid: string
    vin: int
}

type SpentConfirmed = {
    txid: string
    vin: int
    blockHeight: int
    blockHash: string
    blockTime: int
}

type SpendingStatus =
    | SpentUnconfirmed of SpentUnconfirmed
    | SpentConfirmed of SpentConfirmed
    | Unspent
    

let SpentUnconfirmedDecoder : Decoder<SpentUnconfirmed> =
    Decode.object(fun get -> {
        txid = get.Required.Field "txid" Decode.string
        vin = get.Required.Field "vin" Decode.int})
    
let SpentConfirmedDecoder : Decoder<SpentConfirmed> =
    Decode.object(fun get -> {
        txid = get.Required.Field "txid" Decode.string
        vin = get.Required.Field "vin" Decode.int
        blockHeight = get.Required.At ["status"; "block_height"] Decode.int
        blockHash = get.Required.At ["status"; "block_hash"] Decode.string
        blockTime = get.Required.At ["status"; "block_time"] Decode.int})
    
let SpendingDecoder : Decoder<SpendingStatus> =
    Decode.at ["status"; "confirmed"] Decode.bool
    |> Decode.andThen( function
            | true -> SpentConfirmedDecoder    |> Decode.map SpentConfirmed
            | false -> SpentUnconfirmedDecoder |> Decode.map SpentUnconfirmed
        )

let SpendingStatusDecoder =
    Decode.field "spent" Decode.bool
    |> Decode.andThen (function
        | true -> SpendingDecoder
        | false -> Decode.succeed Unspent)
    
let getSpenderTransaction (txId: string) (index: int) =
    promise {
        let endpoints =
            [|
            //"https://mempool.cakewallet.com"
            //"https://mempool.lyberry.com"
            "https://mempool.space"
            |]
        
        let rnd = JS.Math.floor(JS.Math.random() * 100.0) 
        let endpoint = endpoints[int rnd % endpoints.Length]
        let! result = Fetch.get($"{endpoint}/api/tx/{txId}/outspend/{index}", decoder = SpendingStatusDecoder)
        return result
    }
