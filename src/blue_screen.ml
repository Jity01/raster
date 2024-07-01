open Core

(* You need to change the implementation of this function so that it replaces
   the "blue" pixels of the foreground image with pixels from the
   corresponding position in the background image instead of just ignoring
   the background image and returning the foreground image. *)
let transform ~(foreground : Image.t) ~(background : Image.t) : _ =
  let is_blue (r, g, b) = b > r + g in
  Image.mapi foreground ~f:(fun ~x ~y (r, g, b) ->
    match is_blue (r, g, b) with
    | true -> Image.get background ~x ~y
    | false -> r, g, b)
;;

(* let%expect_test "transform" = let correct_output = Image.load_ppm
   ~filename:"../images/reference-beach_portrait_gray.ppm" in let
   output_to_test = Image.load_ppm
   ~filename:"../images/beach_portrait_gray.ppm" |> transform in let _r =
   Image.mapi output_to_test ~f:(fun ~x ~y (r, g, b) -> let (cr, cg, cb) =
   (Image.get correct_output ~x ~y) in match Pixel.equal (cr, cg, cb) (r, g,
   b) with | true -> (r, g, b) | false -> printf "output:%i %i %i\ncorrect:
   %i %i %i\n\n" r g b cr cg cb; (r, g, b)) in [%expect {||}] ;; *)

let command =
  Command.basic
    ~summary:
      "Replace the 'blue' pixels of an image with those from another image"
    [%map_open.Command
      let foreground_file =
        flag
          "foreground"
          (required Command.Param.string)
          ~doc:"IMAGE_FILE the foreground PPM image file"
      and background_file =
        flag
          "background"
          (required Command.Param.string)
          ~doc:"IMAGE_FILE the background PPM image file"
      in
      fun () ->
        let foreground = Image.load_ppm ~filename:foreground_file in
        let background = Image.load_ppm ~filename:background_file in
        let image' = transform ~foreground ~background in
        Image.save_ppm
          image'
          ~filename:
            (String.chop_suffix_exn foreground_file ~suffix:".ppm"
             ^ "_vfx.ppm")]
;;
