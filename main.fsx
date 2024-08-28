#load "GitModel.fsx"
open GitModel

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
                   let inner =
                       match blob_id with
                       | BlobId inner -> inner
                       | _ -> failwith "unreachable!"

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
            let inner =
                match tree_id with
                | TreeId inner -> inner
                | _ -> failwith "unreachable!"

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
        let inner =
            match commit_id with
            | CommitId inner -> inner
            | _ -> failwith "unreachable!"

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
                  let inner =
                      match tag_id with
                      | TagId inner -> inner
                      | _ -> failwith "unreachable!"

                  inner |} ]

// $ git tag "hello_txt" <blob_id>
refs <-
    List.append
        refs
        [ LightweightTag
              {| tagName = "hello_txt"
                 id = blob_id |} ]
