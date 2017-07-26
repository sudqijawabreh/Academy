module DomainPrimitiveTypes 
open Rop
open System.Text.RegularExpressions
type StringError=
|MustNotBeLongerThan of int
type ListIndexError=
|MustbePositive
|MustbeLessThan of int
type IntError=
|NotAnInteger
type UrlError=
|NotAnUrl
module String50=
    type T= String50 of string
    let create( s:string )=
        if s.Length<=50 then
            succeed ( String50 s )
        else
            fail ( MustNotBeLongerThan 50 )
    let value s=match s with String50 x ->x
let parseInt (s:string)=
    try
        s|>int|>succeed
    with
    |_->fail NotAnInteger
module ListIndex=
    type T= ListIndex of int
    let create length index=

        match index with
        |_ when index<0 -> fail MustbePositive
        |_ when index>length -> fail ( MustbeLessThan length )
        |_-> succeed (ListIndex index)

module UrlString=
    let regString="(https?:\/\/(?:www\.|(?!www))[a-zA-Z0-9][a-zA-Z0-9-]+[a-zA-Z0-9]\.[^\s]{2,}|www\.[a-zA-Z0-9][a-zA-Z0-9-]+[a-zA-Z0-9]\.[^\s]{2,}|https?:\/\/(?:www\.|(?!www))[a-zA-Z0-9]\.[^\s]{2,}|www\.[a-zA-Z0-9]\.[^\s]{2,})"
    type T=UrlString of string
    let create (s:string)=
       match  Regex.IsMatch(s,regString) with 
       |true-> succeed (UrlString s)
       |false-> fail NotAnUrl
    let value (UrlString url)=url