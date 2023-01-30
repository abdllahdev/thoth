open FilePath
open Sys
open Core

let write_file (filename : string) (code : string) : unit =
  let file = DefaultPath.filename_of_string filename in
  FileUtil.touch file;
  Out_channel.write_all ~data:code file

let create_folder (source : string) : unit =
  let source = DefaultPath.filename_of_string source in
  let destination = DefaultPath.filename_of_string (getcwd () ^ "/.out") in
  FileUtil.mkdir destination;
  FileUtil.cp [ source ] destination ~recurse:true

let delete_files (directory_path : string) : unit =
  let destination =
    DefaultPath.filename_of_string (getcwd () ^ "/.out" ^ directory_path)
  in
  FileUtil.rm [ destination ] ~recurse:true

let make_directory (directory_page : string) : unit =
  let destination =
    DefaultPath.filename_of_string (getcwd () ^ "/.out" ^ directory_page)
  in
  FileUtil.mkdir destination
