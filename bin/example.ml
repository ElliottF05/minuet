open Core
open Minuet.Executor
module Http = Minuet.Http

(* Echoing HTTP/1.1 server for manual / curl testing. Run with:
     dune exec minuet
     curl -X POST -d 'hello' http://localhost:8132/
   The server echoes the request body back as the response body. *)
let port = 8132
let () =
  Printf.printf
    "listening on http://localhost:%d; will echo any POST body back\n%!" port;
  start (fun () ->
    Http.serve port (fun reqd ->
      let request = H1.Reqd.request reqd in
      let content_type =
        match H1.Headers.get request.H1.Request.headers "content-type" with
        | None -> "application/octet-stream"
        | Some ct -> ct
      in
      let request_body = H1.Reqd.request_body reqd in
      let buf = Buffer.create 256 in
      let rec on_read bs ~off ~len =
        Buffer.add_string buf (Bigstringaf.substring bs ~off ~len);
        H1.Body.Reader.schedule_read request_body ~on_eof ~on_read
      and on_eof () =
        let body = Buffer.contents buf in
        Printf.eprintf "[server] received '%s', echoing\n%!" body;
        let response = H1.Response.create
          ~headers:(H1.Headers.of_list
            [ "content-type",   content_type
            ; "content-length", Int.to_string (String.length body)
            ; "connection",     "close"
            ])
          `OK
        in
        H1.Reqd.respond_with_string reqd response body
      in
      H1.Body.Reader.schedule_read request_body ~on_eof ~on_read))
