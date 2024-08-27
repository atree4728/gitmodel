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

        bytes |> Array.fold (fun str byte -> str + $"%02X{byte}") ""

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

and Tree = { children: Entry list }

type Commit =
    { tree: Tree
      parent: Id<Commit> list
      authorInfo: User * TimeInfo
      committerInfo: User * TimeInfo
      message: string }

and AnnotatedTag =
    { name: string
      id: ObjectID
      taggerInfo: User * TimeInfo
      message: string }

and ObjectID =
    | TreeId of Id<Tree>
    | BlobId of Id<Blob>
    | CommitId of Id<Commit>
    | TagId of Id<AnnotatedTag>

type Object =
    | Tree of Tree
    | Blob of Blob
    | Commit of Commit
    | Tag of AnnotatedTag

let rec store object =
    let content =
        match object with
        | Tree tree ->
            tree.children
            |> List.map (fun entry ->
                let kind =
                    match entry.kind with
                    | Directory -> 40000
                    | Executable -> 100755
                    | TextFile -> 100644
                    | Symlink -> 120000
                    | SubModule -> 160000

                let id =
                    match entry.id with
                    | EntryId.BlobId(Id id)
                    | EntryId.TreeId(Id id) -> id |> string

                $"{kind} {entry.name}\x00{id}")
            |> List.fold (fun str child -> str + child) ""
        | Blob blob -> blob.content
        | Commit commit ->
            let tree =
                let treeId = Tree commit.tree |> store
                $"tree {treeId}\n"

            let parents =
                commit.parent
                |> List.map (fun id -> $"parent {id}")
                |> List.fold (fun str parent -> str + parent + "\n") ""

            let author =
                let (user, time) = commit.authorInfo
                $"author {user} {time}\n"

            let committer =
                let (user, time) = commit.committerInfo
                $"committer {user} {time}\n"

            let message = commit.message + "\n"
            tree + parents + author + committer + "\n" + message
        | Tag tag ->
            let object = $"object {tag.id}\n"

            let objectType =
                let objectType =
                    match tag.id with
                    | TreeId _ -> "tree"
                    | BlobId _ -> "blob"
                    | CommitId _ -> "commit"
                    | TagId _ -> "tag"

                $"type {objectType}\n"

            let name = $"tag {tag.name}\n"

            let tagger =
                let (user, time) = tag.taggerInfo
                $"tagger {user} {time}\n"

            let message = tag.message + "\n"
            object + objectType + name + tagger + "\n" + message

    let header =
        let objectType =
            match object with
            | Tree _ -> "tree"
            | Blob _ -> "blob"
            | Commit _ -> "commit"
            | Tag _ -> "tag"

        $"{objectType} {content.Length}\x00"

    header + content

let hash object =
    let sha1 = System.Security.Cryptography.SHA1.Create()

    let hash =
        object |> store |> System.Text.Encoding.ASCII.GetBytes |> sha1.ComputeHash

    match object with
    | Tree _ -> Id hash |> TreeId
    | Blob _ -> Id hash |> BlobId
    | Commit _ -> Id hash |> CommitId
    | Tag _ -> Id hash |> TagId

type Reference =
    | Object of ObjectID
    | Reference of Reference

type LightweightTag = string * ObjectID

type Branch =
    | Heads of {| branchName: string; id: Id<Commit> |}
    | Remote of
        {| remoteName: string
           branchName: string
           id: Id<Commit> |}

let objects = Map<ObjectID, Object> []
let refs: Branch list = []
