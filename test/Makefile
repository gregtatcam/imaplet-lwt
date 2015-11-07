test_sync: test_sync.ml
	ocamlbuild -pkgs lwt,lwt.syntax,lwt.unix,irmin.unix test_sync.native

dedup: dedup.ml
	ocamlbuild -pkgs imaplet,imaplet.commands,imaplet.parsemail,lwt,lwt.syntax,lwt.unix dedup.native

maildir_read: maildir_read.ml imaplet_gitl.* compress.*
	ocamlbuild -pkgs unix,threads,lwt,lwt.unix,lwt.syntax,re,re.posix,irmin.unix,imaplet,nocrypto,cstruct,imaplet.commands maildir_read.native

maildir_read_preempt: maildir_read.ml imaplet_gitl.* compress.*
	ocamlfind ocamlopt -thread -o maildir_read -linkpkg -package unix,threads,lwt,lwt.syntax,lwt.unix,lwt.preemptive,zip,re,re.posix,nocrypto,cstruct,imaplet,imaplet.commands compress.mli compress.ml imaplet_gitl.mli imaplet_gitl.ml maildir_read.ml

irvsfsio: irvsfsio.ml compress.ml compress.mli
	ocamlbuild -pkgs lwt,lwt.syntax,lwt.unix,re,re.posix,irmin.unix,zip irvsfsio.native

irvsfsio_cl: irvsfsio_cl.ml
	ocamlbuild -pkgs lwt,lwt.syntax,lwt.unix irvsfsio_cl.native

test_gitl: test_gitl.ml
	ocamlfind ocamlopt -thread -o gitl -linkpkg -package unix,threads,lwt,lwt.syntax,lwt.unix,lwt.preemptive,zip,re,re.posix,imaplet,imaplet.commands test_gitl.ml

clean:
	rm -rf *.cmx *.cmi *.o *.native _build 

email_stats: email_stats.ml
	ocamlfind ocamlopt -thread -o email_stats -linkpkg -package imaplet,imaplet.commands,imaplet.parsemail,lwt,lwt.syntax,lwt.unix,re,re.posix email_stats.ml
