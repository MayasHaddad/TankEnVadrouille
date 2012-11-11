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

(** Input / output abstraction. *)

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
      (** Custom position. *)
  (** Position of the hot spot of an image.

      When the image is drawn at position [(x, y)], the image is moved so that
      the hot spot is placed at [(x, y)].

      Default value is [TopLeft].

      A custom position of [(xf, xp, yf, yp)] will place the hot spot
      at [(xf * w + xp, yf * h + yp)], where [w] and [h] are the width and
      height of the image. The position is relative to the top left of the
      image (the default hot spot). For instance, [Bottom] is equivalent to
      [Custom(0.5, 0, 1., 0)]. *)

val init: int -> int -> unit
  (** Initialize the module and open a window.

      [init w h]: open a window of width [w] and of height [h]. *)

val update: unit -> unit
  (** Update the window.

      This must be called after you finish drawing a frame. If you don't,
      nothing will change on the screen. *)

val frame_delay: int -> unit
  (** Wait a specified number of milliseconds before returning.

      If [delay] was last called 5ms ago, and now you call [delay 8], the
      game will actually sleep for 3ms. If you call [delay 3], the system
      will not sleep at all. This is useful to ensure a fixed frame rate. *)

val quit: unit -> unit
  (** Close the window.

      Should always be called before you exit. *)

(** TTF font loading and writing. *)
module Text: sig
  type t
    (** The type of TTF fonts. *)

  val load: string -> int -> t
    (** Load a TTF font from a file.

        [load file size]: load a TTF font from file [file]
        with font size [size]. *)

  val write: t -> ?align: align -> ?color: Sdlvideo.color -> int -> int ->
    string -> unit
    (** Write some text.

        [write font x y text]: write [text] at position [(x, y)] using font
        [font]. *)
end

(** Image making and drawing. *)
module Sprite: sig
  type t
    (** The type of sprites. *)

  val load: ?align: align -> string -> t
    (** Load a sprite from a file. *)

  val draw: t -> int -> int -> unit
    (** Draw a sprite. *)

  val rotozoom : ?aa:bool -> t -> float -> float -> t
    (** Rotate and zoom the sprite in degree (counter clockwise)
        aa: anti-aliasing (default off)
    *)

  val screenshot: ?align: align -> unit -> t
    (** Get a screenshot.

        Return a sprite whose image is the content of the current screen.
        By "screen" we mean "screen buffer", i.e. not the actual content of the
        screen but what would be on the screen after an [update]. *)
end

(** Action descriptor. *)
module type ACTION = sig
  type t
    (** The type of actions. *)
end

(** Input registering and reading. *)
module MakeReader(A: ACTION): sig
  val read: unit -> A.t list
    (** Read inputs and return corresponding actions. *)

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

  val key_continuous: Sdlkey.t -> A.t -> unit
    (** Register a continuous key action.

        The action will be returned by [read] if the key is down
        (i.e. has been pressed and not released yet) at the moment of
        reading. *)

  val key_down: Sdlkey.t -> A.t -> unit
    (** Register a key down action.

        The action will be returned by [read] if the key has been pressed
        since the last call to [read]. *)

  val key_up: Sdlkey.t -> A.t -> unit
    (** Register a key up action.

        The action will be returned by [read] if the key has been released
        since the last call to [read]. *)

  val key_auto: int -> int -> Sdlkey.t -> A.t -> unit
    (** Register a repeatable key action.

        [key_auto ini rep k a]: the action [a] will be returned when [k] is
        pressed (i.e. goes from up to down). If the key stays pressed for
        [ini] initial milliseconds, the action is repeated. Then the action
        will be repeated every [rep] milliseconds as long as [k] stays
        pressed. *)

  val quit_action : A.t -> unit
    (** Register the action used when a user ask to quit *)

  type button = Sdlmouse.button =
    | BUTTON_LEFT
    | BUTTON_MIDDLE
    | BUTTON_RIGHT
    | BUTTON_WHEELUP
    | BUTTON_WHEELDOWN


  val mouse_up : button -> (int -> int -> A.t) -> unit
    (** Register a mouse up action

        [mouse_up but fa]: the action [fa x y] will be returned when
        the mouse button [but] is released at ([x],[y]) coordinate.  *)

  val mouse_down : button -> (int -> int -> A.t) -> unit
  (** Register a mouse down action

      [mouse_down but fa]: the action [fa x y] will be returned when
      the mouse button [but] is pressed at ([x],[y]) coordinate.  *)

  val mouse_motion : button list -> (int -> int -> int -> int -> A.t) -> unit
    (** Register a mouse motion action

        [mouse_down but fa]: the action [fa x y rx ry] will be returned
        when the mouse is moved to ([x],[y]) coordinate with the
        relative motion ([rx],[ry]). *)

end
