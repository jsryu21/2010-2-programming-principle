all:
	ocamlc server.mli server.cmo avatar1.mli avatar1.ml center1.mli center1.ml avatar2.mli avatar2.ml center2.mli center2.ml project.cmo -o prj
