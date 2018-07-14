#lang racket

(require ffi/unsafe ffi/unsafe/define)
(require sdl)
(require opengl)

(define-ffi-definer define-nuklear (ffi-lib "libnuklear.so"))

(define-cstruct _NK_Rect
  ((x _float)
   (y _float)
   (w _float)
   (h _float)))

(define-nuklear nk_sdl_init (_fun _SDL_Window -> _pointer))
(define-nuklear nk_begin (_fun _pointer _string _NK_Rect _uint -> _int))
(define-nuklear racket_load_atlas (_fun -> _void))
(define-nuklear nk_layout_row_static (_fun _pointer _int _int _int -> _void))
(define-nuklear nk_sdl_render (_fun _int _int _int -> _void))

(define testrect (make-NK_Rect 1.0 1.0 50.0 50.0))
(define window (SDL_CreateWindow "Hello" 0 0 640 480 2))
(SDL_GL_CreateContext window)

(define ctx (nk_sdl_init window))
(racket_load_atlas)
(nk_begin ctx "Nuklear" testrect 0)
(nk_layout_row_static ctx 30 80 1)
(glViewport 0 0 640 480)
(glClear 0)
(nk_sdl_render 0 (* 512 1024) (* 128 1024))
(SDL_GL_SwapWindow window)
(sleep 10)
