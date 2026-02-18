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

;; Test framework
(define test_begin test-begin)
(define test_end test-end)
(define test_assert test-assert)
(define test_error test-error)
(define test_group test-group)
