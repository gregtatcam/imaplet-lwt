sync: sync.ml
	ocamlbuild -pkgs lwt,lwt.syntax,lwt.unix,irmin.unix sync.native

dedup: dedup.ml
	ocamlbuild -pkgs imaplet,imaplet.commands,imaplet.parsemail,lwt,lwt.syntax,lwt.unix dedup.native

maildir_read: maildir_read.ml imaplet_git.* compress.*
	ocamlbuild -pkgs unix,threads,lwt,lwt.unix,lwt.syntax,re,re.posix,irmin.unix,imaplet,imaplet.commands maildir_read.native

maildir_read_preempt: maildir_read.ml imaplet_git.* compress.*
	ocamlfind ocamlopt -thread -o maildir_read -linkpkg -package unix,threads,lwt,lwt.syntax,lwt.unix,lwt.preemptive,zip,re,re.posix,imaplet,imaplet.commands compress.mli compress.ml imaplet_git.mli imaplet_git.ml maildir_read.ml

irvsfsio: irvsfsio.ml compress.ml compress.mli
	ocamlbuild -pkgs lwt,lwt.syntax,lwt.unix,re,re.posix,irmin.unix,zip irvsfsio.native

irvsfsio_cl: irvsfsio_cl.ml
	ocamlbuild -pkgs lwt,lwt.syntax,lwt.unix irvsfsio_cl.native

rd_git: rd_git.ml imaplet_git.ml imaplet_git.mli compress.ml compress.mli
	ocamlbuild -pkgs lwt,lwt.syntax,lwt.unix,re,re.posix,zip,hex rd_git.native
