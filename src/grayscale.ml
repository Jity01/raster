open Core

(* You need to change the implementation of this function so that it does
   something to the image instead of just leaving it untouched. *)
let transform (image : Image.t) : Image.t =
  Image.map image ~f:(fun (r, g, b) ->
    let avg = (r + g + b) / 3 in
    avg, avg, avg)
;;

let%expect_test "transform" =
  let correct_output =
    Image.load_ppm ~filename:"../images/reference-beach_portrait_gray.ppm"
  in
  let output_to_test =
    Image.load_ppm ~filename:"../images/beach_portrait_gray.ppm" |> transform
  in
  let _r = Image.mapi output_to_test ~f:(fun ~x ~y (r, g, b) ->
    let (cr, cg, cb) = (Image.get correct_output ~x ~y) in
    match Pixel.equal (cr, cg, cb) (r, g, b) with
    | true -> (r, g, b)
    | false -> printf "output:%i %i %i\ncorrect: %i %i %i\n\n" r g b cr cg cb; (r, g, b)) in
  [%expect {||}]
;;

let command =
  Command.basic
    ~summary:"Convert an image to grayscale"
    [%map_open.Command
      let filename =
        flag
          "filename"
          (required Command.Param.string)
          ~doc:"IMAGE_FILE the PPM image file"
      in
      fun () ->
        let image = Image.load_ppm ~filename |> transform in
        Image.save_ppm
          image
          ~filename:
            (String.chop_suffix_exn filename ~suffix:".ppm" ^ "_gray.ppm")]
;;
