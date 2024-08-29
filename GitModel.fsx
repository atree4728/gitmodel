type TimeInfo =
    { unixTimestamp: uint
      timezoneOffset: System.TimeSpan }

    override this.ToString() =
        let prefix = if this.timezoneOffset.Ticks >= 0 then "+" else "-"
        let span = this.timezoneOffset.ToString("hhmm")
        $"{this.unixTimestamp} {prefix}{span}"

let getTimeInfo () =
    let unixTimestamp =
        let now = System.DateTime.Now
        let start = System.DateTime(1970, 1, 1, 0, 0, 0, System.DateTimeKind.Utc)
        (now - start).TotalSeconds |> uint

    let timezoneOffset = System.TimeZoneInfo.Local.BaseUtcOffset

    { unixTimestamp = unixTimestamp
      timezoneOffset = timezoneOffset }

type User =
    { name: string
      email: string }

    override this.ToString() : string = $"{this.name} <{this.email}>"

type Id<'T> =
    | Id of byte array

    override this.ToString() =
        let bytes =
            match this with
            | Id bytes -> bytes

        bytes |> Array.fold (fun str byte -> str + $"%02x{byte}") ""

type Blob = { content: string }

type EntryKind =
    | Directory
    | Executable
    | TextFile
    | Symlink
    | SubModule

type EntryId =
    | BlobId of Id<Blob>
    | TreeId of Id<Tree>

and Entry =
    { kind: EntryKind
      name: string
      id: EntryId }

and Tree = { children: Entry array }

type Commit =
    { tree: Id<Tree>
      parent: Id<Commit> array
      authorInfo: User * TimeInfo
      committerInfo: User * TimeInfo
      message: string }

and AnnotatedTag =
    { name: string
      id: ObjectId
      taggerInfo: User * TimeInfo
      message: string }

and ObjectId =
    | BlobId of Id<Blob>
    | TreeId of Id<Tree>
    | CommitId of Id<Commit>
    | TagId of Id<AnnotatedTag>

type Object =
    | Blob of Blob
    | Tree of Tree
    | Commit of Commit
    | Tag of AnnotatedTag

let rec store object =
    let content =
        match object with
        | Blob blob -> blob.content
        | Tree tree ->
            tree.children
            |> Array.map (fun entry ->
                let kind =
                    match entry.kind with
                    | Directory -> 40000
                    | Executable -> 100755
                    | TextFile -> 100644
                    | Symlink -> 120000
                    | SubModule -> 160000

                let (EntryId.BlobId(Id bytes) | EntryId.TreeId(Id bytes)) = entry.id

                $"{kind} {entry.name}\x00{id}")
            |> Array.fold (fun str child -> str + child) ""
        | Commit commit ->
            let tree = $"tree {commit.tree}\n"

            let parents =
                commit.parent
                |> Array.map (fun id -> $"parent {id}")
                |> Array.fold (fun str parent -> str + parent + "\n") ""

            let author =
                let (user, time) = commit.authorInfo
                $"author {user} {time}\n"

            let committer =
                let (user, time) = commit.committerInfo
                $"committer {user} {time}\n"

            let message = commit.message + "\n"
            tree + parents + author + committer + "\n" + message
        | Tag tag ->
            let objectId = $"object {tag.id}\n"

            let objectType =
                let objectType =
                    match tag.id with
                    | BlobId _ -> "blob"
                    | TreeId _ -> "tree"
                    | CommitId _ -> "commit"
                    | TagId _ -> "tag"

                $"type {objectType}\n"

            let name = $"tag {tag.name}\n"

            let tagger =
                let (user, time) = tag.taggerInfo
                $"tagger {user} {time}\n"

            let message = tag.message + "\n"
            objectId + objectType + name + tagger + "\n" + message

    let header =
        let objectType =
            match object with
            | Blob _ -> "blob"
            | Tree _ -> "tree"
            | Commit _ -> "commit"
            | Tag _ -> "tag"

        $"{objectType} {content.Length}\x00"

    header + content

let generateId object =
    let sha1 = System.Security.Cryptography.SHA1.Create()
    let hash = store >> System.Text.Encoding.ASCII.GetBytes >> sha1.ComputeHash
    let id = object |> hash

    match object with
    | Blob _ -> id |> Id |> BlobId
    | Tree _ -> id |> Id |> TreeId
    | Commit _ -> id |> Id |> CommitId
    | Tag _ -> id |> Id |> TagId

type Branch =
    | Heads of {| branchName: string; id: Id<Commit> |}
    | Remote of
        {| remoteName: string
           branchName: string
           id: Id<Commit> |}

type Reference =
    | AnnotatedTag of
        {| tagName: string
           id: Id<AnnotatedTag> |}
    | LightweightTag of {| tagName: string; id: ObjectId |}
    | Branch of Branch
    | Reference of Reference
