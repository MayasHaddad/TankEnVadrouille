(**************************************************************************)
(* Copyright (c) 2009, Romain BARDOU                                      *)
(* All rights reserved.                                                   *)
(*                                                                        *)
(* Redistribution and  use in  source and binary  forms, with  or without *)
(* modification, are permitted provided that the following conditions are *)
(* met:                                                                   *)
(*                                                                        *)
(* * Redistributions  of  source code  must  retain  the above  copyright *)
(*   notice, this list of conditions and the following disclaimer.        *)
(* * Redistributions in  binary form  must reproduce the  above copyright *)
(*   notice, this list of conditions  and the following disclaimer in the *)
(*   documentation and/or other materials provided with the distribution. *)
(* * Neither the name of Fury Puyo nor  the names of its contributors may *)
(*   be used  to endorse or  promote products derived  from this software *)
(*   without specific prior written permission.                           *)
(*                                                                        *)
(* THIS SOFTWARE  IS PROVIDED BY  THE COPYRIGHT HOLDERS  AND CONTRIBUTORS *)
(* "AS  IS" AND  ANY EXPRESS  OR IMPLIED  WARRANTIES, INCLUDING,  BUT NOT *)
(* LIMITED TO, THE IMPLIED  WARRANTIES OF MERCHANTABILITY AND FITNESS FOR *)
(* A PARTICULAR PURPOSE  ARE DISCLAIMED. IN NO EVENT  SHALL THE COPYRIGHT *)
(* OWNER OR CONTRIBUTORS BE  LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, *)
(* SPECIAL,  EXEMPLARY,  OR  CONSEQUENTIAL  DAMAGES (INCLUDING,  BUT  NOT *)
(* LIMITED TO, PROCUREMENT OF SUBSTITUTE  GOODS OR SERVICES; LOSS OF USE, *)
(* DATA, OR PROFITS; OR BUSINESS  INTERRUPTION) HOWEVER CAUSED AND ON ANY *)
(* THEORY OF  LIABILITY, WHETHER IN  CONTRACT, STRICT LIABILITY,  OR TORT *)
(* (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING  IN ANY WAY OUT OF THE USE *)
(* OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.   *)
(**************************************************************************)

open Sdlvideo
open Sdlevent
open Sdlttf

let screen = ref (fun () -> assert false)

let width = ref 0
let height = ref 0

let last_tick = ref 0

let () =
  Sdlttf.init ()

let init w h =
  Sdl.init [`TIMER; `VIDEO];
  enable_events
    (make_mask [
       KEYDOWN_EVENT;
       KEYUP_EVENT;
       MOUSEBUTTONDOWN_EVENT;
     ]);
  width := w;
  height := h;
  let the_screen = set_video_mode ~w ~h ~bpp: 32 [`HWSURFACE; `DOUBLEBUF] in
  screen := (fun () -> the_screen);
  last_tick := Sdltimer.get_ticks ()

let frame_delay d =
  let now = Sdltimer.get_ticks () in
  if !last_tick + d > now then
    Sdltimer.delay (d - now + !last_tick);
  last_tick := Sdltimer.get_ticks ()

let screen () = !screen ()

let update () =
  flip (screen ())

let quit = Sdl.quit

type align =
  | Center
  | Left
  | Right
  | Top
  | Bottom
  | TopLeft
  | TopRight
  | BottomLeft
  | BottomRight
  | Custom of float * int * float * int

let hotxy w h = function
  | Center -> w / 2, h / 2
  | Left -> 0, h / 2
  | Right -> w, h / 2
  | Top -> w / 2, 0
  | Bottom -> w / 2, h
  | TopLeft -> 0, 0
  | TopRight -> w, 0
  | BottomLeft -> 0, h
  | BottomRight -> w, h
  | Custom (xf, xp, yf, yp) ->
      int_of_float (float_of_int w *. xf) + xp,
      int_of_float (float_of_int h *. yf) + yp

module Text = struct
  type t = Sdlttf.font

  let load file size = open_font file size

  let write font ?(align = TopLeft) ?(color = white) x y txt =
    let (w, h) = Sdlttf.size_text font txt in
    let txt = Sdlttf.render_text_solid font txt ~fg: color in
    let hotx, hoty = hotxy w h align in
    blit_surface
      ~src: txt
      ~dst: (screen ())
      ~dst_rect: {
        r_x = x - hotx;
        r_y = y - hoty;
        r_w = 0;
        r_h = 0;
      } ()
end

module Sprite = struct
  type t = {
    hotx: int;
    hoty: int;
    surface: surface;
  }

  let of_surface ?(align = TopLeft) surface =
    let w, h, _ = surface_dims surface in
    let hotx, hoty = hotxy w h align in
    {
      hotx = hotx;
      hoty = hoty;
      surface = surface;
    }

  let load ?align file =
    of_surface ?align (Sdlloader.load_image file)

  let draw sprite x y =
    blit_surface
      ~src: sprite.surface
      ~dst: (screen ())
      ~dst_rect: {
        r_x = x - sprite.hotx;
        r_y = y - sprite.hoty;
        r_w = 0;
        r_h = 0;
      } ()
  let pi = 3.1415926535897932384626433832795028841971693993751
  let deg_to_rad = pi /. 180.

  let relative_to (cx,cy) (x,y) =
    (x - cx, y - cy)

  let rotate deg (x,y) =
    let rad = deg *. deg_to_rad in
    x *. (cos rad) -. y *. (sin rad),
    x *. (sin rad) +. y *. (cos rad)


  let foi (x,y) = float_of_int x, float_of_int y
  let inv (x,y) = (y,x)

    (** The after the rotation the origin (the top left corner changed)
        so we must compute the new coordinate of the hot point *)
  let new_hot_point deg surface old_hot_point =
    let w, h, _ = surface_dims surface in
    let quart =  int_of_float (deg /. 90.) in
    let px,py = match quart with
      | 0 -> TopLeft, TopRight
      | 1 -> TopRight, BottomRight
      | 2 -> BottomRight, BottomLeft
      | 3 -> BottomLeft, TopLeft
      | _ -> assert false in
    let pxx, pxy =
      rotate deg (inv (foi (relative_to old_hot_point (hotxy w h px)))) in
    let pyx, pyy =
      rotate deg (inv (foi (relative_to old_hot_point (hotxy w h py)))) in
    inv (-.pyx,-.pxy)

  (** round *)
  let round x = int_of_float (floor (x +. 0.5))

  let deg_norm deg =
    let deg_mod = (mod_float deg 360.) in
    let deg_mod = if deg_mod < 0. then 360. +. deg_mod else deg_mod in
    deg_mod

  let rotozoom ?(aa=false) sprite deg zoom =
    let deg = deg_norm deg in
    let hot = sprite.hotx, sprite.hoty in
    let hotx,hoty = new_hot_point deg sprite.surface hot in
    { hotx = round (hotx *. zoom);
      hoty = round (hoty *. zoom);
      surface = Sdlgfx.rotozoomSurface sprite.surface deg zoom aa}

  let screenshot ?align () =
    let surface =
      create_RGB_surface_format
        (screen ())
        [`HWSURFACE]
        ~w: !width
        ~h: !height
    in
    blit_surface
      ~src: (screen ())
      ~dst: surface
      ~dst_rect: {
        r_x = 0;
        r_y = 0;
        r_w = 0;
        r_h = 0;
      } ();
    of_surface ?align surface
end

module type ACTION = sig
  type t
end

module MakeReader(A: ACTION) = struct

  type key = Sdlkey.t =
  | KEY_UNKNOWN  
  | KEY_BACKSPACE
  | KEY_TAB      
  | KEY_CLEAR    
  | KEY_RETURN   
  | KEY_PAUSE    
  | KEY_ESCAPE   
  | KEY_SPACE    
  | KEY_EXCLAIM
  | KEY_QUOTEDBL
  | KEY_HASH
  | KEY_DOLLAR
  | KEY_AMPERSAND
  | KEY_QUOTE
  | KEY_LEFTPAREN
  | KEY_RIGHTPAREN
  | KEY_ASTERISK
  | KEY_PLUS
  | KEY_COMMA
  | KEY_MINUS
  | KEY_PERIOD
  | KEY_SLASH
  | KEY_0
  | KEY_1
  | KEY_2
  | KEY_3
  | KEY_4
  | KEY_5
  | KEY_6
  | KEY_7
  | KEY_8
  | KEY_9
  | KEY_COLON
  | KEY_SEMICOLON
  | KEY_LESS
  | KEY_EQUALS
  | KEY_GREATER
  | KEY_QUESTION
  | KEY_AT           (** Skip uppercase letters *)
  | KEY_LEFTBRACKET
  | KEY_BACKSLASH
  | KEY_RIGHTBRACKET
  | KEY_CARET
  | KEY_UNDERSCORE
  | KEY_BACKQUOTE
  | KEY_a
  | KEY_b
  | KEY_c
  | KEY_d
  | KEY_e
  | KEY_f
  | KEY_g
  | KEY_h
  | KEY_i
  | KEY_j
  | KEY_k
  | KEY_l
  | KEY_m
  | KEY_n
  | KEY_o
  | KEY_p
  | KEY_q
  | KEY_r
  | KEY_s
  | KEY_t
  | KEY_u
  | KEY_v
  | KEY_w
  | KEY_x
  | KEY_y
  | KEY_z
  | KEY_DELETE     (** End of ASCII mapped keysyms *)
  | KEY_WORLD_0    (** International keyboard syms *)
  | KEY_WORLD_1
  | KEY_WORLD_2
  | KEY_WORLD_3
  | KEY_WORLD_4
  | KEY_WORLD_5
  | KEY_WORLD_6
  | KEY_WORLD_7
  | KEY_WORLD_8
  | KEY_WORLD_9
  | KEY_WORLD_10
  | KEY_WORLD_11
  | KEY_WORLD_12
  | KEY_WORLD_13
  | KEY_WORLD_14
  | KEY_WORLD_15
  | KEY_WORLD_16
  | KEY_WORLD_17
  | KEY_WORLD_18
  | KEY_WORLD_19
  | KEY_WORLD_20
  | KEY_WORLD_21
  | KEY_WORLD_22
  | KEY_WORLD_23
  | KEY_WORLD_24
  | KEY_WORLD_25
  | KEY_WORLD_26
  | KEY_WORLD_27
  | KEY_WORLD_28
  | KEY_WORLD_29
  | KEY_WORLD_30
  | KEY_WORLD_31
  | KEY_WORLD_32
  | KEY_WORLD_33
  | KEY_WORLD_34
  | KEY_WORLD_35
  | KEY_WORLD_36
  | KEY_WORLD_37
  | KEY_WORLD_38
  | KEY_WORLD_39
  | KEY_WORLD_40
  | KEY_WORLD_41
  | KEY_WORLD_42
  | KEY_WORLD_43
  | KEY_WORLD_44
  | KEY_WORLD_45
  | KEY_WORLD_46
  | KEY_WORLD_47
  | KEY_WORLD_48
  | KEY_WORLD_49
  | KEY_WORLD_50
  | KEY_WORLD_51
  | KEY_WORLD_52
  | KEY_WORLD_53
  | KEY_WORLD_54
  | KEY_WORLD_55
  | KEY_WORLD_56
  | KEY_WORLD_57
  | KEY_WORLD_58
  | KEY_WORLD_59
  | KEY_WORLD_60
  | KEY_WORLD_61
  | KEY_WORLD_62
  | KEY_WORLD_63
  | KEY_WORLD_64
  | KEY_WORLD_65
  | KEY_WORLD_66
  | KEY_WORLD_67
  | KEY_WORLD_68
  | KEY_WORLD_69
  | KEY_WORLD_70
  | KEY_WORLD_71
  | KEY_WORLD_72
  | KEY_WORLD_73
  | KEY_WORLD_74
  | KEY_WORLD_75
  | KEY_WORLD_76
  | KEY_WORLD_77
  | KEY_WORLD_78
  | KEY_WORLD_79
  | KEY_WORLD_80
  | KEY_WORLD_81
  | KEY_WORLD_82
  | KEY_WORLD_83
  | KEY_WORLD_84
  | KEY_WORLD_85
  | KEY_WORLD_86
  | KEY_WORLD_87
  | KEY_WORLD_88
  | KEY_WORLD_89
  | KEY_WORLD_90
  | KEY_WORLD_91
  | KEY_WORLD_92
  | KEY_WORLD_93
  | KEY_WORLD_94
  | KEY_WORLD_95
  | KEY_KP0        (** Numeric keypad *)
  | KEY_KP1
  | KEY_KP2
  | KEY_KP3
  | KEY_KP4
  | KEY_KP5
  | KEY_KP6
  | KEY_KP7
  | KEY_KP8
  | KEY_KP9
  | KEY_KP_PERIOD
  | KEY_KP_DIVIDE
  | KEY_KP_MULTIPLY
  | KEY_KP_MINUS
  | KEY_KP_PLUS
  | KEY_KP_ENTER
  | KEY_KP_EQUALS
  | KEY_UP         (** Arrows + Home/End pad *)
  | KEY_DOWN
  | KEY_RIGHT
  | KEY_LEFT
  | KEY_INSERT
  | KEY_HOME
  | KEY_END
  | KEY_PAGEUP
  | KEY_PAGEDOWN
  | KEY_F1         (** Function keys *)
  | KEY_F2
  | KEY_F3
  | KEY_F4
  | KEY_F5
  | KEY_F6
  | KEY_F7
  | KEY_F8
  | KEY_F9
  | KEY_F10
  | KEY_F11
  | KEY_F12
  | KEY_F13
  | KEY_F14
  | KEY_F15
  | KEY_NUMLOCK    (** Key state modifier keys *)
  | KEY_CAPSLOCK
  | KEY_SCROLLOCK
  | KEY_RSHIFT
  | KEY_LSHIFT
  | KEY_RCTRL
  | KEY_LCTRL
  | KEY_RALT
  | KEY_LALT
  | KEY_RMETA
  | KEY_LMETA
  | KEY_LSUPER		(** Left "Windows" key *)
  | KEY_RSUPER		(** Right "Windows" key *)
  | KEY_MODE		(** "Alt Gr" key *)
  | KEY_COMPOSE         (** Multi-key compose key *)
  | KEY_HELP       (** Miscellaneous function keys *)
  | KEY_PRINT
  | KEY_SYSREQ
  | KEY_BREAK
  | KEY_MENU
  | KEY_POWER		(** Power Macintosh power key *)
  | KEY_EURO		(** Some european keyboards *)
  | KEY_UNDO	        (** Atari keyboard has Undo *)

  let aquit = ref None

  (** key engine *)
  module Key = struct
    type t = Sdlkey.t
    let compare = compare
  end
  module KeyMap = Map.Make(Key)

  type one_more = Zero | One of int | More of int

  let continuous = ref KeyMap.empty
  let up = ref KeyMap.empty
  let down = ref KeyMap.empty
  let auto = ref KeyMap.empty

  let pressed_keys = ref KeyMap.empty

  let action mapref key acc =
    try KeyMap.find key !mapref :: acc
    with Not_found -> acc

  let action_auto now key since acc =
    try
      let a, ini, rep = KeyMap.find key !auto in
      let next = match since with
        | Zero -> now
        | One since -> since + ini
        | More since -> since + rep
      in
      if next <= now then begin
        pressed_keys :=
          KeyMap.add key
            (if since = Zero then One next else More next)
            !pressed_keys;
        a :: acc
      end else acc
    with Not_found -> acc

  (** mouse engine *)
  type button = Sdlmouse.button =
    | BUTTON_LEFT
    | BUTTON_MIDDLE
    | BUTTON_RIGHT
    | BUTTON_WHEELUP
    | BUTTON_WHEELDOWN

  module Mouse = struct
    type t = button
    let compare = compare
  end
  module MouseMap = Map.Make(Mouse)

  module MouseL = struct
    type t = button list
    let compare = compare
  end
  module MouseLMap = Map.Make(MouseL)

  let mup = ref MouseMap.empty
  let mdown = ref MouseMap.empty
  let mmotion = ref MouseLMap.empty

  let mouse_action mapref ev acc =
    try ((MouseMap.find ev.mbe_button !mapref) ev.mbe_x ev.mbe_y) :: acc
    with Not_found -> acc

  let mousel_action mapref ev acc =
    try ((MouseLMap.find (List.fast_sort compare ev.mme_state) !mapref)
            ev.mme_x ev.mme_y ev.mme_xrel ev.mme_yrel)
          :: acc
    with Not_found -> acc

  (** main function *)
  let read () =
    let now = Sdltimer.get_ticks () in
    let rec read_events acc =
      match poll () with
        | None -> acc
        | Some event ->
            let acc = begin match event with
              | KEYDOWN ke ->
                  pressed_keys := KeyMap.add ke.keysym Zero !pressed_keys;
                  action down ke.keysym acc
              | KEYUP ke ->
                  pressed_keys := KeyMap.remove ke.keysym !pressed_keys;
                  action up ke.keysym acc
              | MOUSEBUTTONDOWN ev -> mouse_action mdown ev acc
              | MOUSEBUTTONUP ev -> mouse_action mup ev acc
              | MOUSEMOTION ev -> mousel_action mmotion ev acc
              | QUIT ->
                begin match !aquit with None -> acc | Some a -> a::acc end
              | _ -> acc
            end in
            read_events acc
    in
    let actions = read_events [] in
    let actions =
      KeyMap.fold (fun k _ -> action continuous k) !pressed_keys actions in
    let actions =
      KeyMap.fold (action_auto now) !pressed_keys actions in
    List.rev actions

  (** register function *)

  let key_continuous k a = continuous := KeyMap.add k a !continuous
  let key_up k a = up := KeyMap.add k a !up
  let key_down k a = down := KeyMap.add k a !down
  let key_auto ini rep k a = auto := KeyMap.add k (a, ini, rep) !auto
  let quit_action a = aquit := Some a
  let mouse_up b a = mup := MouseMap.add b a !mup
  let mouse_down b a = mdown := MouseMap.add b a !mdown
  let mouse_motion bl a =
    let bl = List.fast_sort compare bl in
    mmotion := MouseLMap.add bl a !mmotion

end
