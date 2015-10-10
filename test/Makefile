sync: sync.ml
	ocamlbuild -pkgs lwt,lwt.syntax,lwt.unix,irmin.unix sync.native

dedup: dedup.ml
	ocamlbuild -pkgs imaplet,imaplet.commands,imaplet.parsemail,lwt,lwt.syntax,lwt.unix dedup.native

maildir_read: maildir_read.ml
	ocamlbuild -pkgs lwt,lwt.unix,lwt.syntax,re,re.posix,irmin.unix,imaplet,imaplet.commands,tc,cstruct maildir_read.native

irvsfsio: irvsfsio.ml compress.ml compress.mli
	ocamlbuild -pkgs lwt,lwt.syntax,lwt.unix,re,re.posix,irmin.unix,zip irvsfsio.native

irvsfsio_cl: irvsfsio_cl.ml
	ocamlbuild -pkgs lwt,lwt.syntax,lwt.unix irvsfsio_cl.native

imaplet_git: imaplet_git.ml compress.ml compress.mli
	ocamlbuild -pkgs lwt,lwt.syntax,lwt.unix,re,re.posix,zip,hex imaplet_git.native
