open Core

(* You need to modify this function to blur the input image based on the
   provided radius instead of ignoring it. *)
let transform image ~(radius : int) : _ =
  let new_image = Image.make ~height:(Image.height image) ~width:3 (0, 0, 0) in
  let nn = Image.mapi new_image ~f:(fun ~x ~y _ ->
    let x_start, x_end, y_start, y_end =
      ( Int.max (x - radius) 0
      , Int.min (x + radius) (Image.width image)
      , Int.max (y - radius) 0
      , Int.min (Image.height image) (y + radius) )
    in
    let sliced_image = Image.slice image ~x_start ~y_start ~x_end ~y_end in
    Image.mean_pixel sliced_image) in
  nn
;;

let%expect_test "transform" =
  let correct_output =
    Image.load_ppm ~filename:"../images/reference-beach_portrait_blur.ppm"
  in
  let output_to_test =
    Image.load_ppm ~filename:"../images/beach_portrait.ppm" |> transform ~radius:3
  in
  let _r =
    Image.mapi output_to_test ~f:(fun ~x ~y (r, g, b) ->
      let cr, cg, cb = Image.get correct_output ~x ~y in
      match Pixel.equal (cr, cg, cb) (r, g, b) with
      | true -> r, g, b
      | false ->
        printf "output:%i %i %i\ncorrect: %i %i %i\n\n" r g b cr cg cb;
        r, g, b)
  in
  [%expect {||}]
;;

let command =
  Command.basic
    ~summary:"Blur an image"
    [%map_open.Command
      let filename =
        flag
          "filename"
          (required Command.Param.string)
          ~doc:"IMAGE_FILE the PPM image file"
      and radius =
        flag
          "radius"
          (required Command.Param.int)
          ~doc:"N the radius to use when blurring (higher = more blurred)"
      in
      fun () ->
        let image = Image.load_ppm ~filename in
        let image' = transform image ~radius in
        Image.save_ppm
          image'
          ~filename:
            (String.chop_suffix_exn filename ~suffix:".ppm" ^ "_blur.ppm")]
;;
