#load "GitModel.fsx"
open GitModel

let inline private (|Unreachable|) _ = failwith "unreachable!"

// $ git init
let mutable objects = Map<ObjectId, Object> []
let mutable refs: Reference list = []

// $ echo "Hello, world!" > hello.txt
// $ git add hello.txt
let blob = Blob { content = "Hello, world!\n" }
let blob_id = generateId blob
objects.Add(blob_id, blob)

// $ git commit -m "First commit."
let tree =
    Tree
        { children =
            [| { kind = TextFile
                 name = "hello.txt"
                 id =
                   let (BlobId inner | Unreachable inner) = blob_id
                   EntryId.BlobId inner } |] }

let tree_id = generateId tree
objects.Add(tree_id, tree)

let atree =
    { name = "atree4728"
      email = "atree.public[at]gmail.com" }

let now = getTimeInfo ()

let commit =
    Commit
        { tree =
            let (TreeId inner | Unreachable inner) = tree_id
            inner
          parent = [||]
          authorInfo = atree, now
          committerInfo = atree, now
          message = "First commit." }

let commit_id = generateId commit
objects.Add(tree_id, tree)

let mutable main =
    {| branchName = "main"
       id =
        let (CommitId inner | Unreachable inner) = commit_id
        inner |}
    |> Heads
    |> Branch

let mutable HEAD = Reference main

// $ git tag -a -m "version 0" v0
let tag =
    Tag
        { name = "v0"
          id = commit_id
          taggerInfo = atree, now
          message = "version 0" }

let tag_id = generateId tag
objects.Add(tag_id, tag)

refs <-
    List.append
        refs
        [ AnnotatedTag
              {| tagName = "v0"
                 id =
                  let (TagId inner | Unreachable inner) = tag_id
                  inner |} ]

// $ git tag "hello_txt" <blob_id>
refs <-
    List.append
        refs
        [ LightweightTag
              {| tagName = "hello_txt"
                 id = blob_id |} ]
