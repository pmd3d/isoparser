isoparser
=========

This is based on python library can parse the `ISO 9660`_ disk image format including
`Rock Ridge`_ extensions. It can load ISOs from the local filesystem. You list directory
contents, extract files, and retrieve metadata.

UNMAINTAINED
------------

This package is unmaintained. The author now maintains the pathlab_ package,
which includes support for ISO 9660 and Rock Ridge. The pycdlib_ package
may also be of interest if you need Joliet / UDF support.

Usage
-----

dotnet fsi

#load "iso9660.fsx"

open Iso9660

let iso = parse "/path/to/image.iso"

printfn "Volume: %s" iso.Root.Name

let kids = iso.Root.Children |> List.map (fun r -> r.Name, r.IsDirectory)

printfn "Children: %A" kids

let fileRec = iso.Record("DIR", "README.TXT")  // or iso.Record("README.TXT")

let content = fileRec.Content |> System.Text.Encoding.ASCII.GetString

printfn "Content preview: %s" (content.Substring(0, min 200 content.Length))

iso.Close()
