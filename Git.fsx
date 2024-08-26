type TimeInfo =
    member this.unixTimestamp =
        let now = System.DateTime.Now
        let start = System.DateTime(1970, 1, 1, 0, 0, 0, System.DateTimeKind.Utc)
        (now - start).TotalSeconds |> uint

    member this.timezoneOffset = System.TimeZoneInfo.Local.BaseUtcOffset

    override this.ToString() =
        let prefix = if this.timezoneOffset.Ticks >= 0 then "+" else "-"
        let span = this.timezoneOffset.ToString("hhmm")
        sprintf "%i %s%s" this.unixTimestamp prefix span

type User =
    { name: string
      email: string }

    override this.ToString() : string = sprintf "%s <%s>" this.name this.email

type Id<'T> =
    | Id of byte array

    override this.ToString() =
        let bytes =
            match this with
            | Id bytes -> bytes

        bytes |> Array.fold (fun str byte -> str + sprintf "%02X" byte) ""

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
      authorinfo: User * TimeInfo
      committerinfo: User * TimeInfo
      message: string }

and AnnotatedTag =
    { name: string
      id: ObjectID
      taggerinfo: User * TimeInfo
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

    member this.ToScore() =
        let content =
            match this with
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
                        | EntryId.TreeId(Id id) -> id.ToString()

                    sprintf "%i %s\0%s" kind entry.name id)
                |> List.fold (fun str child -> str + child) ""
            | Blob blob -> blob.content
            | Commit commit ->
                let tree =
                    let treeId = (Tree commit.tree).ToScore()
                    sprintf "tree %s\n" treeId

                let parents =
                    commit.parent
                    |> List.map (fun id -> id.ToString() |> sprintf "parent %s")
                    |> List.fold (fun str parent -> str + parent + "\n") ""

                let author =
                    let (user, time) = commit.authorinfo
                    sprintf "author %s %s\n" (user.ToString()) (time.ToString())

                let committer =
                    let (user, time) = commit.committerinfo
                    sprintf "committer %s %s\n" (user.ToString()) (time.ToString())

                let message = commit.message + "\n"
                tree + parents + author + committer + "\n" + message
            | Tag tag ->
                let object = tag.id.ToString() |> sprintf "object %s\n"

                let objectType =
                    let objectType =
                        match tag.id with
                        | TreeId _ -> "tree"
                        | BlobId _ -> "blob"
                        | CommitId _ -> "commit"
                        | TagId _ -> "tag"

                    sprintf "type %s\n" objectType

                let name = sprintf "tag %s\n" tag.name

                let tagger =
                    let (user, time) = tag.taggerinfo
                    sprintf "tagger %s %s\n" (user.ToString()) (time.ToString())

                let message = tag.message + "\n"
                object + objectType + tagger + "\n" + message

        let header =
            let objectType =
                match this with
                | Tree _ -> "tree"
                | Blob _ -> "blob"
                | Commit _ -> "commit"
                | Tag _ -> "tag"

            sprintf "%s %i\x00" objectType content.Length

        header + content

    member this.Hash() =
        let score = this.ToScore() |> System.Text.Encoding.ASCII.GetBytes
        let sha1 = System.Security.Cryptography.SHA1.Create()
        let hash = sha1.ComputeHash score
        Id hash // <T>

type Reference =
    | Object of ObjectID
    | Reference of Reference

type Branch = Id<Commit>
type LightweightTag = string * ObjectID
