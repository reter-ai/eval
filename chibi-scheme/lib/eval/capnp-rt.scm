;; eval/capnp-rt.scm -- Cap'n Proto Schema and serialization OO wrappers
;;
;; Schema("capnp schema text") returns an object where:
;;   ->StructName returns a struct accessor with ->build(...), ->read(bytes),
;;   ->save(bytes, filename), and ->mmap(filename)
;;
;; Reader objects support ->fieldName to read fields, ->close() to release,
;; and work with `with(r = ...) body` for automatic cleanup.

(define (__capnp_make_reader__ reader)
  ;; Return a dispatch lambda: ->close releases, anything else reads a field.
  (lambda (__field__)
    (cond
      ((eq? __field__ 'close)
       (lambda () (__capnp_reader_close__ reader)))
      (else
       (__capnp_reader_get__ reader (symbol->string __field__))))))

(define (Schema text)
  (let ((raw (__capnp_schema_create__ text)))
    (lambda (__msg__)
      ;; ->StructName dispatches to struct accessor
      (let ((sname (symbol->string __msg__)))
        ;; Return a struct accessor object
        (lambda (__msg2__)
          (cond
            ((eq? __msg2__ 'build)
             (lambda __args__
               ;; args are field-name/value pairs as a flat list
               (apply __capnp_build__ raw sname __args__)))
            ((eq? __msg2__ 'read)
             (lambda (bytes)
               (__capnp_make_reader__ (__capnp_read__ raw sname bytes))))
            ((eq? __msg2__ 'save)
             (lambda (bytes filename)
               (__capnp_save__ filename bytes)))
            ((eq? __msg2__ 'mmap)
             (lambda (filename)
               (__capnp_make_reader__ (__capnp_mmap__ raw sname filename))))
            ((eq? __msg2__ 'fields)
             (__capnp_reader_field_names__
               (__capnp_read__ raw sname
                 (__capnp_build__ raw sname))))
            (else (error "CapnpStruct: unknown method" __msg2__))))))))
