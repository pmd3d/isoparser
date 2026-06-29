// iso9660.fsx
// Single-file F# ISO9660 parser script.
// Simplified: PVD, directory records, basic SUSP/Rock Ridge NM names,
// path walking, and file content.
//
// Usage in FSI:
//   #load "iso9660.fsx";;
//   let iso = parse "/path/to/image.iso"

open System
open System.Collections.Generic
open System.IO
open System.Text

module Iso9660 =

    let SECTOR_LENGTH = 2048

    exception SourceError of string
    exception SUSPError of string

    let inline suspAssert condition =
        if not condition then
            raise (SUSPError "Failed SUSP assertion")

    type SUSPEntry =
        | SP of lenSkp: byte
        | CE of location: uint32 * offset: uint32 * length: uint32
        | PD
        | ST
        | ER of extId: string * extVer: byte * extDes: string * extSrc: string
        | ES of extSeq: byte
        | RR of flags: byte
        | PX of
            mode: uint32 *
            nlinks: uint32 *
            uid: uint32 *
            gid: uint32 *
            ino: uint32 option
        | PN of devHigh: uint32 * devLow: uint32
        | SL of flags: byte * path: string
        | NM of flags: byte * name: string
        | TF of
            flags: byte *
            creation: string option *
            modify: string option *
            access: string option *
            attributes: string option *
            backup: string option *
            expiration: string option *
            effective: string option
        | Unknown of signature: string * version: byte * raw: byte[]

    type VolumeDescriptor =
        abstract Name: string

    type BootVD() =
        interface VolumeDescriptor with
            member _.Name = "boot"

    type SupplementaryVD() =
        interface VolumeDescriptor with
            member _.Name = "supplementary"

    type PartitionVD() =
        interface VolumeDescriptor with
            member _.Name = "partition"

    type TerminatorVD() =
        interface VolumeDescriptor with
            member _.Name = "terminator"

    type Source(path: string) =
        let fs =
            new FileStream(path, FileMode.Open, FileAccess.Read, FileShare.Read)

        let mutable buff = Array.empty<byte>
        let mutable cursor = 0
        let mutable suspStartingIndex: int option = None
        let mutable suspExtensions: SUSPEntry list = []
        let mutable rockRidge = false

        member _.FileStream = fs

        member _.Cursor = cursor

        member _.SuspStartingIndex
            with get () = suspStartingIndex
            and set v = suspStartingIndex <- v

        member _.SuspExtensions
            with get () = suspExtensions
            and set v = suspExtensions <- v

        member _.RockRidge
            with get () = rockRidge
            and set v = rockRidge <- v

        member _.Len = buff.Length - cursor

        member _.RewindRaw(l: int) =
            if cursor < l then
                raise (SourceError "Rewind buffer under-run")

            cursor <- cursor - l

        member this.UnpackRaw(l: int) : byte[] =
            if l < 0 then
                raise (SourceError "Negative unpack length")

            if l > this.Len then
                raise (SourceError "Source buffer under-run")

            let data =
                if l = 0 then
                    Array.empty
                else
                    buff[cursor .. cursor + l - 1]

            cursor <- cursor + l
            data

        member this.UnpackAll() = this.UnpackRaw(this.Len)

        member this.UnpackBoundary() =
            let rem = cursor % SECTOR_LENGTH
            let toSkip =
                if rem = 0 then
                    min SECTOR_LENGTH this.Len
                else
                    min (SECTOR_LENGTH - rem) this.Len

            this.UnpackRaw(toSkip)

        member this.UnpackByte() : byte = this.UnpackRaw(1).[0]

        member this.UnpackUInt32LE() : uint32 =
            let b = this.UnpackRaw(4)
            uint32 b[0]
            ||| (uint32 b[1] <<< 8)
            ||| (uint32 b[2] <<< 16)
            ||| (uint32 b[3] <<< 24)

        member this.UnpackUInt32BE() : uint32 =
            let b = this.UnpackRaw(4)
            (uint32 b[0] <<< 24)
            ||| (uint32 b[1] <<< 16)
            ||| (uint32 b[2] <<< 8)
            ||| uint32 b[3]

        member this.UnpackInt32LE() : int32 = int32 (this.UnpackUInt32LE())

        member this.UnpackInt32BE() : int32 = int32 (this.UnpackUInt32BE())

        member this.UnpackUInt16LE() : uint16 =
            let b = this.UnpackRaw(2)
            uint16 b[0] ||| (uint16 b[1] <<< 8)

        member this.UnpackUInt16BE() : uint16 =
            let b = this.UnpackRaw(2)
            (uint16 b[0] <<< 8) ||| uint16 b[1]

        member this.UnpackInt16LE() : int16 = int16 (this.UnpackUInt16LE())

        member this.UnpackInt16BE() : int16 = int16 (this.UnpackUInt16BE())

        member this.UnpackBothUInt32() : uint32 =
            let a = this.UnpackUInt32LE()
            let b = this.UnpackUInt32BE()

            if a <> b then
                raise (SourceError "Both-endian uint32 value mismatch")

            a

        member this.UnpackBothInt32() : int32 =
            let a = this.UnpackInt32LE()
            let b = this.UnpackInt32BE()

            if a <> b then
                raise (SourceError "Both-endian int32 value mismatch")

            a

        member this.UnpackBothInt16() : int16 =
            let a = this.UnpackInt16LE()
            let b = this.UnpackInt16BE()

            if a <> b then
                raise (SourceError "Both-endian int16 value mismatch")

            a

        member this.UnpackString(l: int) : string =
            let raw = this.UnpackRaw(l)
            Encoding.ASCII.GetString(raw).TrimEnd(' ', char 0)

        member this.UnpackDirDateTime() : string =
            let date = this.UnpackRaw(7)

            if date.Length < 7 then
                "0000-00-00 00:00:00"
            else
                let year = int date[0] + 1900
                let month = int date[1]
                let day = int date[2]
                let hour = int date[3]
                let minute = int date[4]
                let second = int date[5]
                let tzOffsetSeconds = int (sbyte date[6]) * 15 * 60

                try
                    let dt =
                        DateTime(
                            year,
                            month,
                            day,
                            hour,
                            minute,
                            second,
                            DateTimeKind.Utc
                        )
                            .AddSeconds(-float tzOffsetSeconds)

                    dt.ToString("yyyy-MM-dd HH:mm:ss")
                with _ ->
                    sprintf
                        "%04d-%02d-%02d %02d:%02d:%02d"
                        year
                        month
                        day
                        hour
                        minute
                        second

        member this.UnpackVDDateTime() : string =
            let raw = this.UnpackRaw(17)
            Encoding.ASCII.GetString(raw).TrimEnd(' ', char 0)

        member _.Seek(startSector: int, ?length: int) =
            let len = defaultArg length SECTOR_LENGTH
            let offset = int64 startSector * int64 SECTOR_LENGTH

            fs.Seek(offset, SeekOrigin.Begin) |> ignore

            buff <- Array.zeroCreate len

            let read = fs.Read(buff, 0, len)

            if read < len then
                buff <-
                    if read > 0 then
                        buff[0 .. read - 1]
                    else
                        Array.empty

            cursor <- 0

        member _.SaveCursor() : byte[] * int = buff, cursor

        member _.RestoreCursor(state: byte[] * int) =
            let b, c = state
            buff <- b
            cursor <- c

        member this.UnpackSUSP(maxlen: int) : SUSPEntry option =
            if maxlen < 4 then
                None
            else
                let startCursor = cursor

                try
                    let sigBytes = this.UnpackRaw(2)
                    let signature = Encoding.ASCII.GetString(sigBytes)
                    let lenByte = this.UnpackByte()
                    let version = this.UnpackByte()

                    if lenByte < 4uy || int lenByte > maxlen then
                        cursor <- startCursor
                        None
                    else
                        let payloadLen = int lenByte - 4

                        let entry =
                            match signature, version with
                            | "SP", 1uy ->
                                suspAssert (payloadLen = 3)
                                let magic = this.UnpackRaw(2)
                                suspAssert (magic = [| 0xbeuy; 0xefuy |])
                                let lenSkp = this.UnpackByte()
                                SP lenSkp

                            | "CE", 1uy ->
                                suspAssert (payloadLen = 24)
                                let loc = this.UnpackBothUInt32()
                                let off = this.UnpackBothUInt32()
                                let leng = this.UnpackBothUInt32()
                                CE(loc, off, leng)

                            | "PD", 1uy ->
                                if payloadLen > 0 then
                                    this.UnpackRaw(payloadLen) |> ignore

                                PD

                            | "ST", 1uy ->
                                suspAssert (payloadLen = 0)
                                ST

                            | "ER", 1uy ->
                                suspAssert (payloadLen >= 4)
                                let lenId = int (this.UnpackByte())
                                let lenDes = int (this.UnpackByte())
                                let lenSrc = int (this.UnpackByte())

                                suspAssert (
                                    payloadLen = 4 + lenId + lenDes + lenSrc
                                )

                                let extVer = this.UnpackByte()
                                let extId =
                                    Encoding.ASCII.GetString(
                                        this.UnpackRaw(lenId)
                                    )

                                let extDes =
                                    Encoding.ASCII.GetString(
                                        this.UnpackRaw(lenDes)
                                    )

                                let extSrc =
                                    Encoding.ASCII.GetString(
                                        this.UnpackRaw(lenSrc)
                                    )

                                ER(extId, extVer, extDes, extSrc)

                            | "ES", 1uy ->
                                suspAssert (payloadLen = 1)
                                ES(this.UnpackByte())

                            | "RR", 1uy ->
                                suspAssert (payloadLen = 1)
                                RR(this.UnpackByte())

                            | "PX", 1uy ->
                                suspAssert (
                                    payloadLen = 32 || payloadLen = 40
                                )

                                let mode = this.UnpackBothUInt32()
                                let nlinks = this.UnpackBothUInt32()
                                let uid = this.UnpackBothUInt32()
                                let gid = this.UnpackBothUInt32()

                                let ino =
                                    if payloadLen >= 40 then
                                        Some(this.UnpackBothUInt32())
                                    else
                                        None

                                PX(mode, nlinks, uid, gid, ino)

                            | "PN", 1uy ->
                                suspAssert (payloadLen = 16)
                                let dh = this.UnpackBothUInt32()
                                let dl = this.UnpackBothUInt32()
                                PN(dh, dl)

                            | "SL", 1uy ->
                                suspAssert (payloadLen >= 2)

                                let flags = this.UnpackByte()
                                let pathB = ResizeArray<byte>()
                                let target = cursor + payloadLen - 1

                                while cursor < target do
                                    let compFlags = this.UnpackByte()
                                    let compLen = int (this.UnpackByte())
                                    let compContent = this.UnpackRaw(compLen)

                                    match compFlags with
                                    | 2uy ->
                                        suspAssert (compLen = 0)
                                        pathB.AddRange("."B)
                                    | 4uy ->
                                        suspAssert (compLen = 0)
                                        pathB.AddRange(".."B)
                                    | 8uy -> ()
                                    | 0uy
                                    | 1uy ->
                                        suspAssert (compLen > 0)
                                        pathB.AddRange(compContent)
                                    | _ -> suspAssert false

                                    if compFlags = 1uy then
                                        ()
                                    elif
                                        compFlags = 0uy
                                        && cursor = target
                                        && (flags &&& 1uy) = 0uy
                                    then
                                        ()
                                    else
                                        pathB.Add(byte '/')

                                let pathStr =
                                    Encoding.ASCII.GetString(pathB.ToArray())
                                        .TrimEnd('/')

                                SL(flags, pathStr)

                            | "NM", 1uy ->
                                suspAssert (payloadLen >= 1)

                                let flags = this.UnpackByte()
                                let nameContent =
                                    this.UnpackRaw(payloadLen - 1)

                                let nmName =
                                    if flags = 2uy then
                                        "."
                                    elif flags = 4uy then
                                        ".."
                                    else
                                        Encoding.ASCII.GetString(nameContent)

                                NM(flags, nmName)

                            | "TF", 1uy ->
                                suspAssert (payloadLen >= 1)

                                let flags = this.UnpackByte()

                                if payloadLen > 1 then
                                    this.UnpackRaw(payloadLen - 1) |> ignore

                                TF(
                                    flags,
                                    None,
                                    None,
                                    None,
                                    None,
                                    None,
                                    None,
                                    None
                                )

                            | _ ->
                                let raw = this.UnpackRaw(payloadLen)
                                Unknown(signature, version, raw)

                        if cursor <> startCursor + int lenByte then
                            cursor <- startCursor + int lenByte

                        Some entry

                with
                | SUSPError _ ->
                    cursor <- startCursor
                    None
                | _ ->
                    cursor <- startCursor
                    None

        member this.TryUnpackSUSP(maxlen: int) = this.UnpackSUSP(maxlen)

        member this.UnpackRecord(?suspStart: int) : Record option =
            if this.Len <= 0 then
                None
            else
                let len = int (this.UnpackByte())

                if len = 0 then
                    this.RewindRaw(1)
                    None
                else
                    let effectiveSuspStart =
                        match suspStart with
                        | Some i -> Some i
                        | None -> suspStartingIndex

                    Some(Record(this, len - 1, effectiveSuspStart))

        member this.UnpackVolumeDescriptor() : VolumeDescriptor =
            let ty = this.UnpackByte()
            let identifier = this.UnpackString(5)
            let version = this.UnpackByte()

            if identifier <> "CD001" then
                raise (SourceError "Wrong volume descriptor identifier")

            if version <> 1uy then
                raise (SourceError "Wrong volume descriptor version")

            match ty with
            | 0uy -> BootVD() :> VolumeDescriptor
            | 1uy -> PrimaryVD(this) :> VolumeDescriptor
            | 2uy -> SupplementaryVD() :> VolumeDescriptor
            | 3uy -> PartitionVD() :> VolumeDescriptor
            | 255uy -> TerminatorVD() :> VolumeDescriptor
            | _ ->
                raise (
                    SourceError(sprintf "Unknown volume descriptor type: %d" ty)
                )

        member _.Close() =
            fs.Close()
            fs.Dispose()

    and Record(source: Source, recLen: int, suspStartIdx: int option) =
        let target = source.Cursor + recLen

        let _eaLen = source.UnpackByte()
        let location = source.UnpackBothUInt32()
        let dataLength = source.UnpackBothUInt32()
        let dateTimeStr = source.UnpackDirDateTime()
        let flags = source.UnpackByte()

        let isHidden = (flags &&& 1uy) <> 0uy
        let isDirectory = (flags &&& 2uy) <> 0uy

        let _interleaveUnit = source.UnpackByte()
        let _gap = source.UnpackByte()
        let _volSeq = source.UnpackBothInt16()

        let nameLen = int (source.UnpackByte())
        let rawNameFull = source.UnpackRaw(nameLen)

        let rawName =
            if rawNameFull.Length > 0 && rawNameFull[0] = 0uy then
                Array.empty<byte>
            elif rawNameFull.Length > 0 && rawNameFull[0] = 1uy then
                ".."B
            else
                rawNameFull |> Array.takeWhile (fun b -> b <> byte ';')

        if nameLen % 2 = 0 && nameLen > 0 then
            source.UnpackRaw(1) |> ignore

        let mutable suspEntries: SUSPEntry list = []
        let mutable sIdx = suspStartIdx

        if sIdx.IsNone then
            match source.TryUnpackSUSP(target - source.Cursor) with
            | Some(SP lenSkp as sp) ->
                suspEntries <- sp :: suspEntries
                sIdx <- Some(max 0 (int lenSkp - 7))
            | Some entry ->
                suspEntries <- entry :: suspEntries
                sIdx <- Some 0
            | None -> ()

        if sIdx.IsSome then
            let skip = sIdx.Value

            if skip > 0 && source.Cursor + skip <= target then
                source.UnpackRaw(skip) |> ignore

            let mutable keepGoing = true

            while keepGoing && source.Cursor < target do
                match source.TryUnpackSUSP(target - source.Cursor) with
                | Some entry ->
                    suspEntries <- entry :: suspEntries

                    match entry with
                    | ST -> keepGoing <- false
                    | _ -> ()
                | None -> keepGoing <- false

        if source.Cursor < target then
            source.UnpackRaw(target - source.Cursor) |> ignore

        member _.Source = source
        member _.Location = location
        member _.DataLength = dataLength
        member _.DateTime = dateTimeStr
        member _.IsHidden = isHidden
        member _.IsDirectory = isDirectory
        member _.RawNameBytes = rawName
        member _.EmbeddedSUSP = List.rev suspEntries

        member this.RawName =
            Encoding.ASCII.GetString(rawName)

        member this.Name : string =
            let mutable nmName = ""

            for entry in this.EmbeddedSUSP do
                match entry with
                | NM(_, name) ->
                    if name = "." || name = ".." then
                        nmName <- name
                    else
                        nmName <- nmName + name
                | _ -> ()

            if String.IsNullOrEmpty nmName then
                this.RawName
            else
                nmName

        member this.SuspEntries : SUSPEntry list =
            this.EmbeddedSUSP

        member this.FindSUSPEntry(predicate: SUSPEntry -> bool) : SUSPEntry option =
            this.SuspEntries |> List.tryFind predicate

        member this.Children : Record list =
            if not this.IsDirectory then
                failwith "Not a directory"

            this.Source.Seek(int this.Location, int this.DataLength)

            this.Source.UnpackRecord() |> ignore
            this.Source.UnpackRecord() |> ignore

            let rec collect acc =
                if this.Source.Len <= 0 then
                    List.rev acc
                else
                    match this.Source.UnpackRecord() with
                    | Some r -> collect (r :: acc)
                    | None ->
                        let before = this.Source.Len
                        this.Source.UnpackBoundary() |> ignore

                        if this.Source.Len = before then
                            List.rev acc
                        else
                            collect acc

            collect []

        member this.CurrentDirectory : Record =
            if not this.IsDirectory then
                failwith "Not a directory"

            this.Source.Seek(int this.Location, int this.DataLength)

            match this.Source.UnpackRecord() with
            | Some r -> r
            | None -> failwith "Failed to read current directory record"

        member this.ParentDirectory : Record =
            if not this.IsDirectory then
                failwith "Not a directory"

            this.Source.Seek(int this.Location, int this.DataLength)
            this.Source.UnpackRecord() |> ignore

            match this.Source.UnpackRecord() with
            | Some r -> r
            | None -> failwith "Failed to read parent directory record"

        member this.Content : byte[] =
            if this.IsDirectory then
                failwith "Directories have no content"

            this.Source.Seek(int this.Location, int this.DataLength)
            this.Source.UnpackAll()

    and PrimaryVD(source: Source) =
        let _unused0 = source.UnpackRaw(1)
        let systemId = source.UnpackString(32)
        let volId = source.UnpackString(32)
        let _unused1 = source.UnpackRaw(8)
        let volSpaceSize = source.UnpackBothInt32()
        let _unused2 = source.UnpackRaw(32)
        let _volSetSize = source.UnpackBothInt16()
        let _volSeq = source.UnpackBothInt16()
        let _blockSize = source.UnpackBothInt16()
        let pathTableSize = source.UnpackBothInt32()
        let pathTableL = source.UnpackInt32LE()
        let _pathTableOptL = source.UnpackInt32LE()
        let _pathTableM = source.UnpackInt32BE()
        let _pathTableOptM = source.UnpackInt32BE()

        let rootRec =
            match source.UnpackRecord() with
            | Some r -> r
            | None -> failwith "No root record in PVD"

        let _volSetId = source.UnpackString(128)
        let _publisher = source.UnpackString(128)
        let _dataPrep = source.UnpackString(128)
        let _appId = source.UnpackString(128)
        let _copyright = source.UnpackString(38)
        let _abstractF = source.UnpackString(36)
        let _biblio = source.UnpackString(37)
        let _created = source.UnpackVDDateTime()
        let _modified = source.UnpackVDDateTime()
        let _expires = source.UnpackVDDateTime()
        let _effective = source.UnpackVDDateTime()
        let _structVer = source.UnpackByte()

        interface VolumeDescriptor with
            member _.Name = "primary"

        member _.SystemIdentifier = systemId
        member _.VolumeIdentifier = volId
        member _.VolumeSpaceSize = volSpaceSize
        member _.PathTableSize = pathTableSize
        member _.PathTableLLoc = pathTableL
        member _.RootRecord = rootRec

    type PathTable(source: Source) =
        let paths = Dictionary<string list, uint32>()

        do
            let mutable pathList: string list list = []

            while source.Len > 0 do
                let nl = int (source.UnpackByte())

                if nl > 0 || source.Len >= 7 then
                    source.UnpackByte() |> ignore

                    let loc = source.UnpackUInt32LE()
                    let pidx = int (source.UnpackUInt16LE()) - 1
                    let nameB = source.UnpackRaw(nl)
                    let name = Encoding.ASCII.GetString(nameB).Trim(char 0, ' ')

                    source.UnpackRaw(nl % 2) |> ignore

                    let parentPath =
                        if pidx >= 0 && pidx < pathList.Length then
                            pathList[pidx]
                        else
                            []

                    let thisPath =
                        if String.IsNullOrEmpty name then
                            parentPath
                        else
                            parentPath @ [ name ]

                    pathList <- pathList @ [ thisPath ]

                    if not (paths.ContainsKey(thisPath)) then
                        paths[thisPath] <- loc
                else
                    source.UnpackAll() |> ignore

        member _.Record(path: string list) : Record =
            match paths.TryGetValue(path) with
            | true, loc ->
                source.Seek(int loc)

                match source.UnpackRecord() with
                | Some r -> r
                | None ->
                    failwithf
                        "Failed to unpack record at path table location for %A"
                        path

            | false, _ ->
                raise (
                    KeyNotFoundException(
                        sprintf "Path not in path table: %A" path
                    )
                )

type ISO(path: string) =
        let src = Source(path)
        let volumeDescs = Dictionary<string, VolumeDescriptor>()

        let pvd =
            let mutable sec = 16
            let mutable parsingVD = true

            while parsingVD do
                src.Seek(sec)
                sec <- sec + 1

                let vd = src.UnpackVolumeDescriptor()
                volumeDescs.[vd.Name] <- vd

                if vd.Name = "terminator" then
                    parsingVD <- false

            volumeDescs.["primary"] :?> PrimaryVD

        let pathTab =
            src.Seek(pvd.PathTableLLoc, int pvd.PathTableSize)
            PathTable(src)

        let rootRec = pvd.RootRecord

        do
            let rootDot =
                try
                    rootRec.CurrentDirectory
                with _ ->
                    rootRec

            let hasSP =
                rootDot.EmbeddedSUSP
                |> List.exists (function
                    | SP _ -> true
                    | _ -> false)

            if hasSP then
                match
                    rootDot.FindSUSPEntry(function
                        | SP _ -> true
                        | _ -> false)
                with
                | Some(SP ls) ->
                    src.SuspStartingIndex <- Some(max 0 (int ls - 7))
                | _ ->
                    src.SuspStartingIndex <- Some 0

                let ers =
                    rootDot.SuspEntries
                    |> List.choose (function
                        | ER(i, v, d, s) -> Some(ER(i, v, d, s))
                        | _ -> None)

                src.SuspExtensions <- ers

                let isRR =
                    ers
                    |> List.exists (function
                        | ER(id, ver, _, _) ->
                            (id, ver) = ("RRIP_1991A", 1uy)
                            || (id, ver) = ("IEEE_P1282", 1uy)
                        | _ -> false)

                src.RockRidge <- isRR
            else
                src.SuspStartingIndex <- None

        member _.Source = src

        member _.Root : Record = rootRec

        member _.PathTable = pathTab

        member _.Record([<ParamArray>] parts: string[]) : Record =
            let path = Array.toList parts
            let upperPath = path |> List.map (fun s -> s.ToUpperInvariant())

            let mutable record: Record option = None

            let mutable pivot =
                if src.RockRidge then
                    0
                else
                    path.Length

            while pivot > 0 && record.IsNone do
                try
                    let p =
                        if src.RockRidge then
                            path |> List.take pivot
                        else
                            upperPath |> List.take pivot

                    record <- Some(pathTab.Record(p))
                with
                | :? KeyNotFoundException ->
                    pivot <- pivot - 1
                | _ ->
                    pivot <- pivot - 1

            let mutable recd = defaultArg record rootRec

            let remaining =
                if pivot < path.Length then
                    path |> List.skip pivot
                else
                    []

            for part in remaining do
                let mutable found = false

                for child in recd.Children do
                    if not found then
                        let matches =
                            child.Name = part
                            || child.RawName = part
                            || child.RawName.ToUpperInvariant()
                               = part.ToUpperInvariant()

                        if matches then
                            recd <- child
                            found <- true

                if not found then
                    raise (
                        KeyNotFoundException(
                            sprintf "Path component not found: %s" part
                        )
                    )

            recd

        member _.Close() = src.Close()

        interface IDisposable with
            member this.Dispose() = this.Close()

    let parse (path: string) : ISO = ISO(path)

// Convenience top-level alias for FSI, so this works:
//
//   #load "iso9660.fsx";;
//   let iso = parse "/path/to/image.iso"
let parse = Iso9660.parse
