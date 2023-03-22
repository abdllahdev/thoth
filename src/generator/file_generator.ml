open FilePath
open Core
open Core_unix

let write_file filename code =
  let file = DefaultPath.filename_of_string filename in
  FileUtil.touch file;
  Out_channel.write_all ~data:code file

let create_folder output_dir source =
  let source = DefaultPath.filename_of_string source in
  let destination =
    DefaultPath.filename_of_string (Fmt.str "%s/%s" (getcwd ()) output_dir)
  in
  FileUtil.mkdir destination;
  FileUtil.cp [ source ] destination ~recurse:true

let make_directory output_dir directory_page =
  let destination =
    DefaultPath.filename_of_string
      (Fmt.str "%s/%s/%s" (getcwd ()) output_dir directory_page)
  in
  FileUtil.mkdir ~parent:true destination
