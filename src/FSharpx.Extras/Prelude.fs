﻿namespace FSharpx

open System
open System.Globalization
open System.Runtime.ExceptionServices

[<AutoOpen>]
module Prelude =

    /// Transforms a function by flipping the order of its arguments.
    let inline flip f a b = f b a

    /// Transforms a function by flipping the order of its arguments.
    let inline flip3 f a b c = f c a b

    /// Transforms a function by flipping the order of its arguments.
    let inline flip4 f a b c d = f d a b c
    
    /// Transforms an uncurried function to a curried function.
    let inline curry f a b = f(a,b)

    /// Transforms an uncurried function to a curried function.
    let inline uncurry f (a,b) = f a b

    /// Transforms an uncurried function to a triple-curried function.
    let inline  curry3 f a b c = f (a, b, c)

    /// Transforms an uncurried function to a triple-curried function.
    let inline uncurry3 f (a,b,c) = f a b c

    /// Swap the elements of a pair.
    let inline swap (a,b) = (b,a)

    /// Given a value, creates a function with one ignored argument which returns the value.
    let inline konst a _ = a

    /// Given a value, creates a function with two ignored arguments which returns the value.
    let inline konst2 a _ _ = a

    /// Creates a pair
    let inline tuple2 a b = a,b
    
    /// Creates a 3-tuple
    let inline tuple3 a b c = a,b,c
    
    /// Creates a 4-tuple
    let inline tuple4 a b c d = a,b,c,d
    
    /// Creates a 5-tuple
    let inline tuple5 a b c d e = a,b,c,d,e
    
    /// Creates a 6-tuple
    let inline tuple6 a b c d e f = a,b,c,d,e,f

    /// Fixed point combinator.
    let rec fix f x = f (fix f) x

    /// Fixed point combinator.
    let rec fix2 f x y = f (fix2 f) x y

    /// Fixed point combinator.
    let rec fix3 f x y z = f (fix3 f) x y z

    /// Sequencing operator like Haskell's ($). Has better precedence than (<|) due to the
    /// first character used in the symbol.
    let (^) = (<|)

    /// Bottom value
    let undefined<'T> : 'T = raise (NotImplementedException("result was implemented as undefined"))

    /// Given a value, apply a function to it, ignore the result, then return the original value.
    let inline tee fn x = fn x |> ignore; x

    /// Performs an implicit conversion using op_Implicit
    let inline (!>) (x:^a) : ^b = ((^a or ^b) : (static member op_Implicit : ^a -> ^b) x)

    /// Custom operator for `tee`: Given a value, apply a function to it, ignore the result, then return the original value.
    let inline (|>!) x fn = tee fn x

    /// Rethrows an exception. This can be used even outside of try-with block. The exception object (stacktrace, etc) is not changed.
    /// More info in:
    /// http://thorarin.net/blog/post/2013/02/21/Preserving-Stack-Trace.aspx
    /// https://stackoverflow.com/questions/7168801/how-to-use-reraise-in-async-workflows-in-f
    let reraise' (e:exn) : 'T = ExceptionDispatchInfo.Capture(e).Throw() ; undefined
  

    /// Rethrows an exception, but before that applies a function on it. This can be used even outside of try-with block. The exception object (stacktrace, etc) is not changed.
    let reraiseWith (f : exn -> unit) (e:exn) : 'T = f e ; reraise' e

    /// TODO
    let inline toOption x = match x with
                            | true, v -> Some v
                            | _       -> None
    /// TODO
    let inline tryWith f x = f x |> toOption

    type Boolean with
        /// TODO
        static member parse =
            tryWith bool.TryParse

    type Byte with
        /// TODO
        static member parseWithOptions style provider x =
            Byte.TryParse(x, style, provider) |> toOption

        /// TODO
        static member parse x =
            Byte.parseWithOptions NumberStyles.Integer CultureInfo.InvariantCulture x

    type SByte with
        /// TODO
        static member parseWithOptions style provider x =
            SByte.TryParse(x, style, provider) |> toOption

        /// TODO
        static member parse x =
            SByte.parseWithOptions NumberStyles.Integer CultureInfo.InvariantCulture x

    type UInt16 with
        /// TODO
        static member parseWithOptions style provider x =
            UInt16.TryParse(x, style, provider) |> toOption

        /// TODO
        static member parse x =
            UInt16.parseWithOptions NumberStyles.Integer CultureInfo.InvariantCulture x

    type Int16 with
        /// TODO
        static member parseWithOptions style provider x =
            Int16.TryParse(x, style, provider) |> toOption

        /// TODO
        static member parse x =
            Int16.parseWithOptions NumberStyles.Integer CultureInfo.InvariantCulture x

    type UInt32 with
        /// TODO
        static member parseWithOptions style provider x =
            UInt32.TryParse(x, style, provider) |> toOption

        /// TODO
        static member parse x =
            UInt32.parseWithOptions NumberStyles.Integer CultureInfo.InvariantCulture x

    type Int32 with
        /// TODO
        static member parseWithOptions style provider x =
            Int32.TryParse(x, style, provider) |> toOption

        /// TODO
        static member parse x =
            Int32.parseWithOptions NumberStyles.Integer CultureInfo.InvariantCulture x

    type UInt64 with
        /// TODO
        static member parseWithOptions style provider x =
            UInt64.TryParse(x, style, provider) |> toOption

        /// TODO
        static member parse x =
            UInt64.parseWithOptions NumberStyles.Integer CultureInfo.InvariantCulture x

    type Int64 with
        /// TODO
        static member parseWithOptions style provider x =
            Int64.TryParse(x, style, provider) |> toOption

        /// TODO
        static member parse x =
            Int64.parseWithOptions NumberStyles.Integer CultureInfo.InvariantCulture x

    type Decimal with
        /// TODO
        static member parseWithOptions style provider x =
            Decimal.TryParse(x, style, provider) |> toOption

        /// TODO
        static member parse x =
            Decimal.parseWithOptions NumberStyles.Currency CultureInfo.InvariantCulture x

    type Single with
        /// TODO
        static member parseWithOptions style provider x =
            Single.TryParse(x, style, provider) |> toOption

        /// TODO
        static member parse x =
            Single.parseWithOptions NumberStyles.Float CultureInfo.InvariantCulture x

    type Double with
        /// TODO
        static member parseWithOptions style provider x =
            Double.TryParse(x, style, provider) |> toOption

        /// TODO
        static member parse x =
            Double.parseWithOptions NumberStyles.Float CultureInfo.InvariantCulture x

    type DateTime with
        /// TODO
        static member parseWithOptions style provider x =
            DateTime.TryParse(x, provider, style) |> toOption

        /// TODO
        static member parse x =
            DateTime.parseWithOptions DateTimeStyles.None CultureInfo.InvariantCulture x

        /// TODO
        static member parseExactWithOptions style provider (formats: string[]) x =
            DateTime.TryParseExact(x, formats, provider, style) |> toOption

        /// TODO
        static member parseExact formats x =
            DateTime.parseExactWithOptions DateTimeStyles.None CultureInfo.InvariantCulture formats x

    type DateTimeOffset with
        /// TODO
        static member parseWithOptions style provider x =
            DateTimeOffset.TryParse(x, provider, style) |> toOption

        /// TODO
        static member parse x =
            DateTimeOffset.parseWithOptions DateTimeStyles.None CultureInfo.InvariantCulture x

        /// TODO
        static member parseExactWithOptions style provider (formats: string[]) x =
            DateTimeOffset.TryParseExact(x, formats, provider, style) |> toOption

        /// TODO
        static member parseExact formats x =
            DateTimeOffset.parseExactWithOptions DateTimeStyles.None CultureInfo.InvariantCulture formats x

    // Active patterns
    /// TODO
    let (|Boolean       |_|) = Boolean.parse
    /// TODO
    let (|Byte          |_|) = Byte.parse
    /// TODO
    let (|SByte         |_|) = SByte.parse
    /// TODO
    let (|UInt16        |_|) = UInt16.parse
    /// TODO
    let (|Int16         |_|) = Int16.parse
    /// TODO
    let (|UInt32        |_|) = UInt32.parse
    /// TODO
    let (|Int32         |_|) = Int32.parse
    /// TODO
    let (|UInt64        |_|) = UInt64.parse
    /// TODO
    let (|Int64         |_|) = Int64.parse
    /// TODO
    let (|Decimal       |_|) = Decimal.parse
    /// TODO
    let (|Single        |_|) = Single.parse
    /// TODO
    let (|Double        |_|) = Double.parse
    /// TODO
    let (|DateTime      |_|) = DateTime.parse
    /// TODO
    let (|DateTimeOffset|_|) = DateTimeOffset.parse
