;; eval/aliases.scm -- Underscore aliases for Eval syntax
;; Loaded in both Python and worker contexts.

;; SRFI-18 thread aliases
(define make_thread make-thread)
(define thread_start thread-start!)
(define thread_yield thread-yield!)
(define thread_join thread-join!)
(define thread_sleep thread-sleep!)
(define thread_terminate thread-terminate!)
(define current_thread current-thread)
(define make_mutex make-mutex)
(define mutex_lock mutex-lock!)
(define mutex_unlock mutex-unlock!)
(define make_condvar make-condition-variable)
(define condvar_signal condition-variable-signal!)
(define condvar_broadcast condition-variable-broadcast!)

;; Random, JSON, environment, clock
(define random_integer random-integer)
(define random_real random-real)
(define json_read json-read)
(define json_write json-write)
(define get_env get-environment-variable)
(define current_clock_second current-clock-second)

;; Bitwise operations
(define bit_and bit-and)
(define bit_ior bit-ior)
(define bit_xor bit-xor)
(define bit_count bit-count)
(define arithmetic_shift arithmetic-shift)
(define integer_length integer-length)

;; Misc
(define object_cmp object-cmp)
(define heap_stats heap-stats)
(define hash_table_cell hash-table-cell)

;; Hash table aliases (srfi/69)
(define make_hash_table make-hash-table)
(define hash_table_ref hash-table-ref)
(define hash_table_set hash-table-set!)
(define hash_table_delete hash-table-delete!)
(define hash_table_exists hash-table-exists?)
(define hash_table_keys hash-table-keys)
(define hash_table_values hash-table-values)
(define hash_table_size hash-table-size)
(define hash_table_to_alist hash-table->alist)

;; List library aliases (srfi/1)
(define take_while take-while)
(define drop_while drop-while)
(define list_index list-index)
(define delete_duplicates delete-duplicates)
(define alist_cons alist-cons)
(define alist_copy alist-copy)
(define alist_delete alist-delete)
(define append_map append-map)
(define filter_map filter-map)
(define fold_right fold-right)
(define take_right take-right)
(define drop_right drop-right)
(define split_at split-at)
(define last_pair last-pair)
(define circular_list circular-list)
(define is_proper_list proper-list?)
(define is_dotted_list dotted-list?)

;; Math/prime aliases
(define is_prime prime?)
(define is_probable_prime probable-prime?)
(define random_prime random-prime)
(define nth_prime nth-prime)
(define prime_above prime-above)
(define prime_below prime-below)

;; Type conversion — only those WITHOUT OO wrappers
;; (string->number has ->to_number(), string->symbol has ->to_symbol(),
;;  list->vector has ->to_vector(), vector->list has ->to_list(),
;;  string->list has ->chars() — all covered by arrow methods)
(define number_to_string number->string)
(define symbol_to_string symbol->string)
(define char_to_integer char->integer)
(define integer_to_char integer->char)
(define list_to_string list->string)

;; String functions — only those without OO wrappers
;; (string-length has ->length, string-ref has s[i], string-append has ++,
;;  string-map/for-each already in C aliases and have ->map/->for_each)
(define string_hash string-hash)
(define string_copy string-copy)

;; File I/O
(define call_with_output_file call-with-output-file)
(define call_with_input_file call-with-input-file)
(define open_output_file_append open-output-file/append)
(define close_output_port close-output-port)
(define close_input_port close-input-port)
(define open_input_file open-input-file)
(define open_output_file open-output-file)
(define open_input_string open-input-string)
(define open_output_string open-output-string)
(define get_output_string get-output-string)
;; write_string, read_line, read_string already in C aliases
(define read_char read-char)
(define write_char write-char)
(define peek_char peek-char)

;; Char predicates
(define char_alphabetic char-alphabetic?)
(define char_numeric char-numeric?)

;; Test framework
(define test_begin test-begin)
(define test_end test-end)
(define test_assert test-assert)
(define test_error test-error)
(define test_group test-group)
