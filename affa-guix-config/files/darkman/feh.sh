#!/bin/sh
tag="wallpaper-${1}"         # make tags: wallpaper-light, wallpaper-dark

# Point this at your actual digikam DB file.
# With SQLite, digiKam stores DB files in (or under) the collection folder by default. :contentReference[oaicite:2]{index=2}
db="${DIGIKAM_DB:-$HOME/Pictures/digikam4.db}"

# escape single quotes for sqlite
tag_sql=$(printf "%s" "$tag" | sed "s/'/''/g")

path="$(sqlite3 -noheader "$db" "
SELECT ar.specificPath || a.relativePath || '/' || i.name
FROM Images i
JOIN Albums a      ON a.id = i.album
JOIN AlbumRoots ar ON ar.id = a.albumRoot
JOIN ImageTags it  ON it.imageid = i.id
JOIN Tags t        ON t.id = it.tagid
WHERE t.name = '$tag_sql'
ORDER BY RANDOM()
LIMIT 1;
")"

[ -n "$path" ] || { echo "No images found with tag: $tag" >&2; exit 1; }
[ -e "$path" ] || { echo "DB returned path but file missing: $path" >&2; exit 1; }

echo "$path"
feh --bg-fill --no-xinerama "$path"
