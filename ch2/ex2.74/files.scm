;; Data-directed programming.
;; Each filetype will have their own specific selectors that will use the same names, but stored inside a operation-and-type table to
;; store procedures organized by file type. Each file type will have an installation package to add their specific selectors and other
;; procedures to the table.

;; For example:
;; |-----------------------|--------------------------------Types--------------------------------|
;; | Operations            | division-1                       | division-2                       |
;; |-----------------------|----------------------------------|----------------------------------|
;; | get-record            | get-record                       | get-record                       |
;; | get-salary            | get-salary                       | get-salary                       |
;; | find-employee-record  | find-employee-record             | find-employee-record             |
;; | make-personnel-record | make-personnel-record            | make-personnel-record            |
;; |-----------------------|----------------------------------|----------------------------------|

(define (main)
  ;; Install packages to use their stored procedures with the table.
  (install-division-1-file-package)
  (install-division-2-file-package)

  (println "--- DIVISION 1 ---")
  (let* ((address-1 ((get 'make-address 'division-1) "123 abc street" "nyc" "ny" "1234"))
         (salary-1 ((get 'make-salary 'division-1) "salary" "10000000000"))
         (record-1 ((get 'make-personnel-record 'division-1) "nate" address-1 salary-1))
         (address-2 ((get 'make-address 'division-1) "456 def main" "la" "ca" "5678"))
         (salary-2 ((get 'make-salary 'division-1) "hourly" "100"))
         (record-2 ((get 'make-personnel-record 'division-1) "john" address-2 salary-2))
         (records (list record-1 record-2))
         (division-1-file ((get 'make-file 'division-1) records)))
    (println ((get 'get-address 'division-1) ((get 'get-record 'division-1) "nate" division-1-file)))
    (println ((get 'get-salary 'division-1) ((get 'get-record 'division-1) "nate" division-1-file)))
    (println ((get 'get-address 'division-1) ((get 'get-record 'division-1) "john" division-1-file)))
    (println ((get 'get-salary 'division-1) ((get 'get-record 'division-1) "john" division-1-file)))

    (newline)

    (println "--- DIVISION 2 ---")
    (let* ((address-1 ((get 'make-address 'division-2) "123 abc street" "nyc" "ny" "1020"))
           (salary-1 ((get 'make-salary 'division-2) "salary" "9999"))
           (record-1 ((get 'make-personnel-record 'division-2) "west" address-1 salary-1))
           (address-2 ((get 'make-address 'division-2) "456 def main" "la" "ca" "3020"))
           (salary-2 ((get 'make-salary 'division-2) "hourly" "999"))
           (record-2 ((get 'make-personnel-record 'division-2) "smith" address-2 salary-2))
           (records (list record-1 record-2))
           (division-2-file ((get 'make-file 'division-2) records)))
      (println ((get 'get-address 'division-2) ((get 'get-record 'division-2) "west" division-2-file)))
      (println ((get 'get-salary 'division-2) ((get 'get-record 'division-2) "west" division-2-file)))
      (println ((get 'get-address 'division-2) ((get 'get-record 'division-2) "smith" division-2-file)))
      (println ((get 'get-salary 'division-2) ((get 'get-record 'division-2) "smith" division-2-file)))

      (newline)

      (println (find-employee-record "smith" (list division-1-file division-2-file))))))

;; Finds

(define (find-by-tag tag sequence)
  (cond ((null? sequence) #f)
        ((eq? tag (caar sequence)) (cdar sequence))
        (else (find-by-tag tag (cdr sequence)))))

(define (find-procedure-for-operations operation ops)
  (cond ((null? ops) (error "operation not found in operations" operation ops))
        ((eq? operation (caar ops)) (cdar ops))
        (else (find-procedure-for-operations operation (cdr ops)))))

(define (find-operations-for-type type table . default-value)
  (cond ((null? table) (if (null? default-value) (error "operations not found for type" type) (car default-value)))
        ((eq? type (caar table)) (cadar table))
        (else (find-operations-for-type type (cdr table) (if (null? default-value) '() (car default-value))))))

;; Operations Table

;; NOTE: Structure: ((type ((operation procedure) (operation procedure) ...)) (type ...))
(define operation-type-table '())

(define (get operation type)
  (find-procedure-for-operations operation (find-operations-for-type type operation-type-table)))

;; Puts the 'procedure' under the 'types' 'operations' list, replacing the existing value if that 'operation' is already present
;; 1. Search the table for the correct type, return it's operations or an empty list if that type is not present
;; 2. With the list of operations, search it for an existing 'operation', if present, replace it. Otherwise append to the end of the list.
;; 3. With the updated operations, traverse the table again, and replace the tables operations for that type.
(define (put operation type procedure)
  (let ((operations (find-operations-for-type type operation-type-table '())))

    ;; Search the operations to see if 'operation' already exists or isn't present, replacing or appending and returning the new operations
    (define (replace-or-append tag item sequence)
      (cond ((null? sequence)  (cons item '())) ;; append to end if not found
            ((eq? tag (caar sequence))  (cons item (cdr sequence)))
            (else (cons (car sequence) (replace-or-append tag item (cdr sequence))))))

    (let ((new-operations (replace-or-append operation (cons operation procedure) operations)))
      (let ((new-table (replace-or-append type (list type new-operations) operation-type-table)))
        (set! operation-type-table new-table)))))

;; ---

(define (install-division-2-file-package)
  (define (make-file records)
    (cons 'division-2 records))
  (define (make-personnel-record name address salary)
    (cons name (cons address salary)))
  (define (make-address street city state zip)
    (list 'address street city state zip))
  (define (make-salary type amount)
    (list 'salary type amount))
  (define (get-record name f)
    (find-by-tag name (cdr f)))
  (define get-address cdar)
  (define get-salary cddr)

  (put 'make-file 'division-2 make-file)
  (put 'make-personnel-record 'division-2 make-personnel-record)
  (put 'make-address 'division-2 make-address)
  (put 'make-salary 'division-2 make-salary)
  (put 'get-record 'division-2 get-record)
  (put 'get-address 'division-2 get-address)
  (put 'get-salary 'division-2 get-salary))

;; ---

(define (install-division-1-file-package)
  (define (make-file records)
    (list 'division-1 records))
  (define (make-personnel-record name address salary)
    (list name address salary))
  (define (make-address street city state zip)
    (list 'address street city state zip))
  (define (make-salary type amount)
    (list 'salary type amount))
  (define (get-record name f)
    (find-by-tag name (cadr f)))
  (define get-address cdar)
  (define get-salary cdadr)

  (put 'make-file 'division-1 make-file)
  (put 'make-personnel-record 'division-1 make-personnel-record)
  (put 'make-address 'division-1 make-address)
  (put 'make-salary 'division-1 make-salary)
  (put 'get-record 'division-1 get-record)
  (put 'get-address 'division-1 get-address)
  (put 'get-salary 'division-1 get-salary))

;; ---

;; A. The different filetypes must have the following type/tag information as the first element of their lists or pairs:
;;    - File: e.g. ('division-1-file ...)
;;    Using get-record implementation for devision-1:
;;    -  ((get 'get-record 'division-1) division-1-file)

;; B. The actual structure of the salary record may vary, as long as the divisions selectors meet the requirements of returning the correct data.
;;    To get a salary from any personnel file, the division file type will need to be known. Then all you would have to do would be
;;    to request the correct procedure by type (assuming the package was installed) and then you would only have to pass the file to
;;    the procedure to get the salary:
;;      - ((get 'get-salary 'division-2) division-2-personnel-file)

;; C. This procedure searches across all divisions, so not apart of an individual package
;;    This is why it is important for all divisions to have their type information as the first element, otherwise we wouldn't know what
;;    type, and a type getter wouldn't work because we still wouldn't know which type getter to use for the file.
(define (find-employee-record name files)
  (if (null? files)
    (error "employee not found in files" name)
    (let* ((f (car files))
           (type (car f))
           (record ((get 'get-record type) name f)))
      (if record
        record
        (find-employee-record name (cdr files))))))

;; D. During a company takeover, Insatiable would have to audit their file structure and maybe make modifications to ensure the following:
;;    1. A new installation package would need to be made with required constructors and selectors supporting all data fields (address, salary, etc).
;;    2. The location of the types in the structure will have to be the same for the 'find-employee-record' to work properly across all files still.
;;    These are all required to ensure no changes are required to the existing divisions and code bases.
