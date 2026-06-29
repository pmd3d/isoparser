// iso9660.fsx
// Single-file F# port of pycdlib ISO9660 parser (simplified, no HTTP, .NET 8, quick script style)
// Focus on core functionality: PVD, records, basic Rock Ridge/SUSP long names, path walking, file content.
// Prefer simplicity; some advanced CE chain following and full caching omitted or basic.

open System
open System.Collections.Generic
open System.IO
open System.Text

module Iso9660 =

    let SECTOR_LENGTH = 2048

    exception SourceError of string
    exception SUSPError of string

    let inline suspAssert (condition: bool) =
        if not condition then raise (SUSPError "Failed SUSP assertion")

    // SUSP / Rock Ridge entries as DU for simplicity
    type SUSPEntry =
        | SP of lenSkp: byte
        | CE of location: uint32 * offset: uint32 * length: uint32
        | PD
        | ST
        | ER of extId: string * extVer: byte * extDes: string * extSrc: string
        | ES of extSeq: byte
        | RR of flags: byte
        | PX of mode: uint32 * nlinks: uint32 * uid: uint32 * gid: uint32 * ino: uint32 option
        | PN of devHigh: uint32 * devLow: uint32
        | SL of flags: byte * path: string
        | NM of flags: byte * name: string
        | TF of flags: byte
            * creation: string option
            * modify: string option
            * access: string option
            * attributes: string option
            * backup: string option
            * expiration: string option
            * effective: string option
        | Unknown of signature: string * version: byte * raw: byte[]

    type VolumeDescriptor =
        abstract Name: string

    type BootVD() =
        interface VolumeDescriptor with member _.Name = "boot"

    type SupplementaryVD() =
        interface VolumeDescriptor with member _.Name = "supplementary"

    type PartitionVD() =
        interface VolumeDescriptor with member _.Name = "partition"

    type TerminatorVD() =
        interface VolumeDescriptor with member _.Name = "terminator"

    type Source(path: string) =
        let fs = new FileStream(path, FileMode.Open, FileAccess.Read, FileShare.Read)
        let mutable buff = Array.empty<byte>
        let mutable cursor = 0
        let mutable _suspStartingIndex: int option = None
        let mutable _suspExtensions: SUSPEntry list = []
        let mutable _rockRidge = false

        member _.FileStream = fs
        member val SuspStartingIndex
            with get () = _suspStartingIndex
            and set v = _suspStartingIndex <- v
        member val SuspExtensions
            with get () = _suspExtensions
            and set v = _suspExtensions <- v
        member val RockRidge
            with get () = _rockRidge
            and set v = _rockRidge <- v

        member _.Len = buff.Length - cursor

        member this.RewindRaw(l: int) =
            if cursor < l then raise (SourceError "Rewind buffer under-run")
            cursor <- cursor - l

        member this.UnpackRaw(l: int) : byte[] =
            if l > this.Len then raise (SourceError "Source buffer under-run")
            let data = buff[cursor .. cursor + l - 1]
            cursor <- cursor + l
            data

        member this.UnpackAll() = this.UnpackRaw(this.Len)
        member this.UnpackBoundary() = this.UnpackRaw(SECTOR_LENGTH - (cursor % SECTOR_LENGTH))

        member private this.ReadUInt32LE(offset: int) : uint32 =
            uint32 buff[offset] ||| (uint32 buff[offset+1] <<< 8) ||| (uint32 buff[offset+2] <<< 16) ||| (uint32 buff[offset+3] <<< 24)

        member private this.ReadUInt32BE(offset: int) : uint32 =
            (uint32 buff[offset] <<< 24) ||| (uint32 buff[offset+1] <<< 16) ||| (uint32 buff[offset+2] <<< 8) ||| uint32 buff[offset+3]

        member private this.ReadInt32LE(offset: int) : int32 =
            int32 (this.ReadUInt32LE(offset))

        member private this.ReadInt32BE(offset: int) : int32 =
            int32 (this.ReadUInt32BE(offset))

        member private this.ReadUInt16LE(offset: int) : uint16 =
            uint16 buff[offset] ||| (uint16 buff[offset+1] <<< 8)

        member private this.ReadUInt16BE(offset: int) : uint16 =
            (uint16 buff[offset] <<< 8) ||| uint16 buff[offset+1]

        member private this.ReadInt16LE(offset: int) : int16 =
            int16 (this.ReadUInt16LE(offset))

        member private this.ReadInt16BE(offset: int) : int16 =
            int16 (this.ReadUInt16BE(offset))

        member this.UnpackByte() : byte = this.UnpackRaw(1).[0]

        // Direct unpack helpers (advance cursor)
        member this.UnpackUInt32LE() : uint32 =
            let b = this.UnpackRaw(4)
            uint32 b[0] ||| (uint32 b[1] <<< 8) ||| (uint32 b[2] <<< 16) ||| (uint32 b[3] <<< 24)

        member this.UnpackUInt32BE() : uint32 =
            let b = this.UnpackRaw(4)
            (uint32 b[0] <<< 24) ||| (uint32 b[1] <<< 16) ||| (uint32 b[2] <<< 8) ||| uint32 b[3]

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
            if a <> b then raise (SourceError "Both-endian value mismatch")
            a

        member this.UnpackBothInt32() : int32 =
            let a = this.UnpackInt32LE()
            let b = this.UnpackInt32BE()
            if a <> b then raise (SourceError "Both-endian value mismatch")
            a

        member this.UnpackBothInt16() : int16 =
            let a = this.UnpackInt16LE()
            let b = this.UnpackInt16BE()
            if a <> b then raise (SourceError "Both-endian value mismatch")
            a

        member this.UnpackString(l: int) : string =
            let raw = this.UnpackRaw(l)
            // rstrip spaces (0x20)
            let trimmed = raw |> Array.takeWhile (fun b -> b <> 0x20uy)
            Encoding.ASCII.GetString(trimmed)

        member this.UnpackDirDateTime() : string =
            let date = this.UnpackRaw(7)
            if date.Length < 7 then "0000-00-00 00:00:00"
            else
                let year = int date[0] + 1900
                let month = int date[1]
                let day = int date[2]
                let hour = int date[3]
                let minute = int date[4]
                let second = int date[5]
                let tzOffset = int (sbyte date[6]) * 15 * 60 // seconds from GMT
                try
                    let dt = DateTime(year, month, day, hour, minute, second, DateTimeKind.Utc).AddSeconds(-float tzOffset)
                    dt.ToString("yyyy-MM-dd HH:mm:ss")
                with _ ->
                    sprintf "%04d-%02d-%02d %02d:%02d:%02d" year month day hour minute second

        member this.UnpackVDDateTime() : string =
            // 17 bytes, original TODO - return raw string
            let raw = this.UnpackRaw(17)
            Encoding.ASCII.GetString(raw).TrimEnd()

        member this.Seek(startSector: int, ?length: int) =
            let len = defaultArg length SECTOR_LENGTH
            let offset = int64 startSector * int64 SECTOR_LENGTH
            fs.Seek(offset, SeekOrigin.Begin) |> ignore
            buff <- Array.zeroCreate len
            let read = fs.Read(buff, 0, len)
            if read < len then buff <- if read > 0 then buff[0..read-1] else Array.empty
            cursor <- 0

        member this.SaveCursor() : byte[] * int = (buff, cursor)
        member this.RestoreCursor((b, c): byte[] * int) =
            buff <- b
            cursor <- c

        member this.UnpackSUSP(maxlen: int) : SUSPEntry option =
            if maxlen < 4 then None
            else
                let startCursor = cursor
                let sigBytes = this.UnpackRaw(2)
                let signature = Encoding.ASCII.GetString(sigBytes)
                let lenByte = this.UnpackByte()
                let version = this.UnpackByte()
                let payloadLen = int lenByte - 4
                if payloadLen < 0 || maxlen < int lenByte then
                    cursor <- startCursor
                    None
                else
                    try
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
                                CE (loc, off, leng)
                            | "PD", 1uy ->
                                if payloadLen > 0 then ignore (this.UnpackRaw(payloadLen))
                                PD
                            | "ST", 1uy ->
                                suspAssert (payloadLen = 0)
                                ST
                            | "ER", 1uy ->
                                suspAssert (payloadLen >= 4)
                                let lenId = int (this.UnpackByte())
                                let lenDes = int (this.UnpackByte())
                                let lenSrc = int (this.UnpackByte())
                                suspAssert (payloadLen = 4 + lenId + lenDes + lenSrc)
                                let extVer = this.UnpackByte()
                                let extId = this.UnpackRaw(lenId) |> Encoding.ASCII.GetString
                                let extDes = this.UnpackRaw(lenDes) |> Encoding.ASCII.GetString
                                let extSrc = this.UnpackRaw(lenSrc) |> Encoding.ASCII.GetString
                                ER (extId, extVer, extDes, extSrc)
                            | "ES", 1uy ->
                                suspAssert (payloadLen = 1)
                                let eseq = this.UnpackByte()
                                ES eseq
                            | "RR", 1uy ->
                                suspAssert (payloadLen = 1)
                                let flags = this.UnpackByte()
                                RR flags
                            | "PX", 1uy ->
                                suspAssert (payloadLen = 32 || payloadLen = 40)
                                let mode = this.UnpackBothUInt32()
                                let nlinks = this.UnpackBothUInt32()
                                let uid = this.UnpackBothUInt32()
                                let gid = this.UnpackBothUInt32()
                                let ino = if payloadLen >= 40 then Some (this.UnpackBothUInt32()) else None
                                PX (mode, nlinks, uid, gid, ino)
                            | "PN", 1uy ->
                                suspAssert (payloadLen = 16)
                                let dh = this.UnpackBothUInt32()
                                let dl = this.UnpackBothUInt32()
                                PN (dh, dl)
                            | "SL", 1uy ->
                                suspAssert (payloadLen >= 2)
                                let flags = this.UnpackByte()
                                let mutable pathB = ResizeArray<byte>()
                                let target = cursor + payloadLen - 1
                                while cursor < target do
                                    let compFlags = this.UnpackByte()
                                    let compLen = int (this.UnpackByte())
                                    let compContent = this.UnpackRaw(compLen)
                                    if compFlags = 2uy then // CURRENT
                                        suspAssert (compLen = 0)
                                        pathB.AddRange( "."B )
                                    elif compFlags = 4uy then // PARENT
                                        suspAssert (compLen = 0)
                                        pathB.AddRange( ".."B )
                                    elif compFlags = 8uy then // ROOT
                                        ()
                                    elif compFlags = 0uy || compFlags = 1uy then // 0 or CONTINUE
                                        suspAssert (compLen > 0)
                                        pathB.AddRange(compContent)
                                    else
                                        suspAssert false
                                    // append / logic (simplified)
                                    if compFlags = 1uy then () // CONTINUE, no /
                                    elif compFlags = 0uy && cursor = target && (flags &&& 1uy) = 0uy then ()
                                    else pathB.Add( byte '/' )
                                let pathStr = Encoding.ASCII.GetString(pathB.ToArray()).TrimEnd('/')
                                SL (flags, pathStr)
                            | "NM", 1uy ->
                                suspAssert (payloadLen >= 1)
                                let flags = this.UnpackByte()
                                let nameContent = this.UnpackRaw(payloadLen - 1)
                                let nmName =
                                    if flags = 2uy then "." // CURRENT
                                    elif flags = 4uy then ".." // PARENT
                                    else Encoding.ASCII.GetString(nameContent)
                                NM (flags, nmName)
                            | "TF", 1uy ->
                                suspAssert (payloadLen >= 1)
                                let flags = this.UnpackByte()
                                // For simplicity, skip detailed time parsing or use dir datetime
                                // Advance the cursor by remaining payload
                                if payloadLen > 1 then ignore (this.UnpackRaw(payloadLen - 1))
                                TF (flags, None, None, None, None, None, None, None)
                            | _ ->
                                let raw = this.UnpackRaw(payloadLen)
                                Unknown (signature, version, raw)
                        // ensure consumed exactly
                        if cursor <> startCursor + int lenByte then
                            cursor <- startCursor + int lenByte
                        Some entry
                    with
                    | :? SUSPError ->
                        cursor <- startCursor
                        let raw = this.UnpackRaw(max (0, min payloadLen (maxlen-4)))
                        Some (Unknown (signature, version, raw))
                    | _ ->
                        cursor <- startCursor + int lenByte
                        Some (Unknown (signature, version, [||]))

        member this.TryUnpackSUSP(maxlen: int) = this.UnpackSUSP(maxlen)

        member this.UnpackRecord(?suspStart: int option) : Record option =
            let startCur = cursor
            let len = int (this.UnpackByte())
            if len = 0 then
                this.RewindRaw(1)
                None
            else
                let r = Record(this, len - 1, defaultArg suspStart _suspStartingIndex)
                // best effort assert
                Some r

        member this.UnpackVolumeDescriptor() : VolumeDescriptor =
            let ty = this.UnpackByte()
            let identifier = this.UnpackString(5)
            let version = this.UnpackByte()
            if identifier <> "CD001" then raise (SourceError "Wrong volume descriptor identifier")
            if version <> 1uy then raise (SourceError "Wrong volume descriptor version")
            match ty with
            | 0uy -> BootVD() :> VolumeDescriptor
            | 1uy -> PrimaryVD(this) :> VolumeDescriptor
            | 2uy -> SupplementaryVD() :> VolumeDescriptor
            | 3uy -> PartitionVD() :> VolumeDescriptor
            | 255uy -> TerminatorVD() :> VolumeDescriptor
            | _ -> raise (SourceError (sprintf "Unknown volume descriptor type: %d" ty))

        member this.Close() = fs.Close(); fs.Dispose()

    and Record(source: Source, recLen: int, ?suspStartIdx: int option) =
        let target = source.Cursor + recLen
        let _ = source.UnpackByte() // ea len, TODO
        let location = source.UnpackBothUInt32()
        let dataLength = source.UnpackBothUInt32()
        let dateTimeStr = source.UnpackDirDateTime()
        let flags = source.UnpackByte()
        let isHidden = (flags &&& 1uy) <> 0uy
        let isDirectory = (flags &&& 2uy) <> 0uy
        let _ = source.UnpackByte() // interleave unit
        let _ = source.UnpackByte() // gap
        let _ = source.UnpackBothInt16() // vol seq
        let nameLen = int (source.UnpackByte())
        let rawNameFull = source.UnpackRaw(nameLen)
        let rawName = 
            if rawNameFull.Length > 0 && rawNameFull[0] = 0uy then [||]
            else rawNameFull |> Array.takeWhile (fun b -> b <> byte ';')
        if nameLen % 2 = 0 && nameLen > 0 then ignore (source.UnpackRaw(1))
        // SUSP parsing (simplified faithful logic)
        let mutable suspEntries: SUSPEntry list = []
        let mutable sIdx = defaultArg suspStartIdx source.SuspStartingIndex
        if sIdx.IsNone then
            match source.TryUnpackSUSP(target - source.Cursor) with
            | Some (SP _ as sp) ->
                suspEntries <- sp :: suspEntries
                match sp with
                | SP ls when ls > 7uy -> sIdx <- Some (int ls - 7)
                | _ -> ()
            | Some _ -> () // non-SP first entry discarded per original logic
            | None -> ()
        if sIdx <> Some false then
            if sIdx.IsSome && sIdx.Value > 0 then
                ignore (source.UnpackRaw(sIdx.Value))
            let mutable keepGoing = true
            while keepGoing && source.Cursor < target do
                match source.TryUnpackSUSP(target - source.Cursor) with
                | Some entry ->
                    suspEntries <- entry :: suspEntries
                    match entry with
                    | ST -> keepGoing <- false
                    | _ -> ()
                | None -> keepGoing <- false
        // skip any remaining in record
        if source.Cursor < target then
            ignore (source.UnpackRaw(target - source.Cursor))
        // public members
        member val Source = source
        member val Location = location
        member val DataLength = dataLength
        member val DateTime = dateTimeStr
        member val IsHidden = isHidden
        member val IsDirectory = isDirectory
        member val RawNameBytes = rawName
        member val EmbeddedSUSP = List.rev suspEntries
        member this.RawName = Encoding.ASCII.GetString(rawName)
        member this.Name : string =
            let mutable nmName = ""
            for entry in this.EmbeddedSUSP do
                match entry with
                | NM (_, name) ->
                    if name = "." || name = ".." then nmName <- name
                    else nmName <- nmName + name
                    // Note: full CONTINUE handling omitted for simplicity; most cases single NM
                | _ -> ()
            if String.IsNullOrEmpty nmName then this.RawName else nmName
        member this.SuspEntries : SUSPEntry list = this.EmbeddedSUSP // simplified, full CE chain omitted
        member this.FindSUSPEntry (predicate: SUSPEntry -> bool) : SUSPEntry option =
            this.SuspEntries |> List.tryFind predicate
        member this.Children : Record list =
            if not this.IsDirectory then failwith "Not a directory"
            this.Source.Seek(int this.Location, int this.DataLength)
            let _cur = this.Source.UnpackRecord()
            let _par = this.Source.UnpackRecord()
            let rec collect acc =
                if this.Source.Len <= 0 then List.rev acc
                else
                    match this.Source.UnpackRecord() with
                    | Some r -> collect (r :: acc)
                    | None ->
                        ignore (this.Source.UnpackBoundary())
                        collect acc
            collect []
        member this.CurrentDirectory : Record =
            if not this.IsDirectory then failwith "Not a directory"
            this.Source.Seek(int this.Location, int this.DataLength)
            match this.Source.UnpackRecord() with
            | Some r -> r
            | None -> failwith "Failed to read current directory record"
        member this.ParentDirectory : Record =
            if not this.IsDirectory then failwith "Not a directory"
            this.Source.Seek(int this.Location, int this.DataLength)
            ignore (this.Source.UnpackRecord())
            match this.Source.UnpackRecord() with
            | Some r -> r
            | None -> failwith "Failed to read parent directory record"
        member this.Content : byte[] =
            if this.IsDirectory then failwith "Directories have no content"
            this.Source.Seek(int this.Location, int this.DataLength)
            this.Source.UnpackAll()

    and PrimaryVD(source: Source) =
        interface VolumeDescriptor with member _.Name = "primary"
        let _ = source.UnpackRaw(1)
        let systemId = source.UnpackString(32)
        let volId = source.UnpackString(32)
        let _ = source.UnpackRaw(8)
        let volSpaceSize = source.UnpackBothInt32()
        let _ = source.UnpackRaw(32)
        let volSetSize = source.UnpackBothInt16()
        let volSeq = source.UnpackBothInt16()
        let blockSize = source.UnpackBothInt16()
        let pathTableSize = source.UnpackBothInt32()
        let pathTableL = source.UnpackInt32LE()
        let pathTableOptL = source.UnpackInt32LE()
        let pathTableM = source.UnpackInt32BE()
        let pathTableOptM = source.UnpackInt32BE()
        let rootRec = 
            match source.UnpackRecord() with
            | Some r -> r
            | None -> failwith "No root record in PVD"
        let volSetId = source.UnpackString(128)
        let publisher = source.UnpackString(128)
        let dataPrep = source.UnpackString(128)
        let appId = source.UnpackString(128)
        let copyright = source.UnpackString(38)
        let abstractF = source.UnpackString(36)
        let biblio = source.UnpackString(37)
        let created = source.UnpackVDDateTime()
        let modified = source.UnpackVDDateTime()
        let expires = source.UnpackVDDateTime()
        let effective = source.UnpackVDDateTime()
        let structVer = source.UnpackByte()
        member val SystemIdentifier = systemId
        member val VolumeIdentifier = volId
        member val VolumeSpaceSize = volSpaceSize
        member val PathTableSize = pathTableSize
        member val PathTableLLoc = pathTableL
        member val RootRecord = rootRec

    type PathTable(source: Source) =
        let paths = Dictionary<string list, uint32>()
        do
            let mutable pathList : string list list = []
            while source.Len > 0 do
                let nl = int (source.UnpackByte())
                ignore (source.UnpackByte())
                let loc = source.UnpackUInt32LE()
                let pidx = int (source.UnpackUInt16LE()) - 1
                let nameB = source.UnpackRaw(nl)
                let name = Encoding.ASCII.GetString(nameB).Trim()
                ignore (source.UnpackRaw(nl % 2))
                let parentPath = if pidx >= 0 && pidx < pathList.Length then pathList.[pidx] else []
                let thisPath = if String.IsNullOrEmpty(name) || name = "\x00" then parentPath else parentPath @ [name]
                pathList <- pathList @ [thisPath]
                if not (paths.ContainsKey(thisPath)) then
                    paths.[thisPath] <- loc
        member _.Record(path: string list) : Record =
            match paths.TryGetValue(path) with
            | true, loc ->
                source.Seek(int loc)
                match source.UnpackRecord() with
                | Some r -> r
                | None -> failwithf "Failed to unpack record at path table location for %A" path
            | false, _ -> raise (KeyNotFoundException(sprintf "Path not in path table: %A" path))

    type ISO(path: string) =
        let src = Source(path)
        let volumeDescs = Dictionary<string, VolumeDescriptor>()
        let mutable sec = 16
        let mutable parsingVD = true
        while parsingVD do
            src.Seek(sec)
            sec <- sec + 1
            let vd = src.UnpackVolumeDescriptor()
            volumeDescs[vd.Name] <- vd
            if vd.Name = "terminator" then parsingVD <- false
        let pvd = volumeDescs.["primary"] :?> PrimaryVD
        src.Seek(pvd.PathTableLLoc, int pvd.PathTableSize)
        let pathTab = PathTable(src)
        let rootRec = pvd.RootRecord
        // Bootstrap SUSP / RockRidge from root's current directory . record
        let rootDot = 
            try rootRec.CurrentDirectory 
            with _ -> rootRec // fallback
        do
            let hasSP = rootDot.EmbeddedSUSP |> List.exists (function SP _ -> true | _ -> false)
            if hasSP then
                match rootDot.FindSUSPEntry (function SP ls -> true | _ -> false) with
                | Some (SP ls) ->
                    if int ls > 7 then src.SuspStartingIndex <- Some (int ls - 7)
                    else src.SuspStartingIndex <- Some 0
                | _ -> src.SuspStartingIndex <- Some 0
                let ers = rootDot.SuspEntries |> List.choose (function ER (i,v,d,s) -> Some (ER(i,v,d,s)) | _ -> None)
                src.SuspExtensions <- ers
                let isRR = ers |> List.exists (fun (ER(id, ver, _, _)) -> 
                    (id, ver) = ("RRIP_1991A", 1uy) || (id, ver) = ("IEEE_P1282", 1uy))
                src.RockRidge <- isRR
            else
                src.SuspStartingIndex <- None
        member _.Source = src
        member _.Root : Record = rootRec
        member _.PathTable = pathTab
        member this.Record([<ParamArray>] parts: string[]) : Record =
            let path = Array.toList parts
            let mutable record = None
            let mutable pivot = if src.RockRidge then 0 else path.Length
            // Try path table for non-RR (upper case)
            let upperPath = path |> List.map (fun s -> s.ToUpperInvariant())
            while pivot > 0 && record.IsNone do
                try
                    let p = if src.RockRidge then path.[0..pivot-1] else upperPath.[0..pivot-1]
                    record <- Some (pathTab.Record(p))
                with
                | :? KeyNotFoundException -> pivot <- pivot - 1
                | _ -> pivot <- pivot - 1
            let mutable recd = defaultArg record rootRec
            // Walk remaining with children
            let remaining = if pivot < path.Length then path.[pivot..] else []
            for part in remaining do
                let mutable found = false
                for child in recd.Children do
                    let saved = src.SaveCursor()
                    if child.Name = part || child.RawName = part then
                        recd <- child
                        found <- true
                    else
                        src.RestoreCursor(saved)
                if not found then raise (KeyNotFoundException(sprintf "Path component not found: %s" part))
            recd
        member _.Close() = src.Close()
        interface IDisposable with member this.Dispose() = this.Close()

    let parse (path: string) : ISO =
        ISO(path)

    // Example usage comment:
    // let iso = Iso9660.parse "/path/to/file.iso"
    // let rootChildren = iso.Root.Children |> List.map (fun r -> r.Name)
    // let readme = iso.Record("README.TXT").Content |> Encoding.ASCII.GetString
    // iso.Close()
