;; Data-directed programming.
;; Each filetype will have their own specific selectors that will use the same names, but stored inside a operation-and-type table to
;; store procedures organized by file type. Each file type will have an installation package to add their specific selectors and other
;; procedures to the table.
;;
;; For example:
;; |-----------------------|------------------------Types----------------------------------------|
;; | Operations            | division-1                       | division-2                       |
;; |-----------------------|----------------------------------|----------------------------------|
;; | get-record            | get-record-division-1            | get-record-division-2            |
;; | get-salary            | get-salary-division-1            | get-salary-division-2            |
;; | find-employee-record  | find-employee-division-1         | find-employee-division-2         |
;; | make-personnel-record | make-personnel-record-division-1 | make-personnel-record-division-2 |
;; |-----------------------|----------------------------------|----------------------------------|

;; ---

(define (main)
  (put 'op-1 'type-1 cons)
  (put 'op-1 'type-1 car)
  (put 'op-2 'type-1 car)
  (put 'op-2 'type-1 cdr)
  (put 'op-1 'type-2 cddr)
  (put 'op-1 'type-1 display)
  (put 'test-op 'test-1 display)
  (put 'test-op 'test-1 display)
  (put 'test-op2 'test-1 display)
  (put 'test-op2 'test-1 display)
  ((get 'op-1 'type-1) "asdf\n"))

;; Finds
;; ---

(define (find-procedure-for-operations operation ops)
  (cond ((null? ops) (error "operation not found in operations" operation ops))
        ((eq? operation (caar ops)) (cdar ops))
        (else (find-procedure-for-operations operation (cdr ops)))))

(define (find-operations-for-type type table . default-value)
  (cond ((null? table) (if (null? default-value) (error "operations not found for type" type) (car default-value)))
        ((eq? type (caar table)) (cadar table))
        (else (find-operations-for-type type (cdr table) (if (null? default-value) '() (car default-value))))))


;; Operations Table
;; ---

;; NOTE: Will hold like this: ((type ((operation procedure) (operation procedure) ...)) (type ...))
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
  (define (make-division-2-file records)
    (cons 'division-2 records))
  (define (make-personnel-record address salary)
    (cons 'personnel-record (cons address salary)))
  (define (make-address street city state zip)
    (list 'address street city state zip))
  (define (make-salary type amount)
    (list 'salary type amount))
  (define (get-record name f)
    (find name (cdr f)))
  (define get-address cadr)
  (define get-salary cddr)

  (put 'make-division-2-file 'division-2 make-division-2-file)
  (put 'make-personnel-record 'division-2 make-personnel-record)
  (put 'make-address 'division-2 make-address)
  (put 'make-salary 'division-2 make-salary)
  (put 'get-record 'division-2 get-record)
  (put 'get-address 'division-2 get-address)
  (put 'get-salary 'division-2 get-salary))

;; ---

(define (install-division-1-file-package)
  (define (make-division-1-file records)
    (list 'division-1 records))
  (define (make-personnel-record address salary)
    (list 'personnel-record address salary))
  (define (make-address street city state zip)
    (list 'address street city state zip))
  (define (make-salary type amount)
    (list 'salary type amount))
  (define (get-record name f)
    (find name (cadr f)))
  (define get-address cadr)
  (define get-salary caddr)

  (put 'make-division-1-file 'division-1 make-division-1-file)
  (put 'make-personnel-record 'division-1 make-personnel-record)
  (put 'make-address 'division-1 make-address)
  (put 'make-salary 'division-1 make-salary)
  (put 'get-record 'division-1 get-record)
  (put 'get-address 'division-1 get-address)
  (put 'get-salary 'division-1 get-salary))


;; NOTE: this procedure searches across all divisions, so not apart of an individual package
;; This is why it is important for all divisions to have their type information as the first element, otherwise we wouldn't know what
;; type, and a type getter wouldn't work because we still wouldn't know which type getter to use for the file.
(define (find-employee-record name files)
  (cond ((null? files) #f)
        (((let* ((f (car files)) ;; if we found the record, return it
                 (type (car f)))
            ((get 'get-record type) name f))) ((get 'get-record type) name f))
        (else (find-employee-record name (cdr files)))))

;; A. The different filetypes must have the following type/tag information as the first element of their lists or pairs:
;;    - File: (personnel_record_file ...)
;;    - Personnel Records: the employee name e.g. (john ...)
;;    - Address: (address ...)
;;    - Salary: (salary ...)

;; TODO: move the two file structures from paper to the code.
;; TODO: create an installation package with the following:
;;       - the required procedures
;;       - constructors to construct representations to test with
;; TODO: test.
;; TODO: reread the problem requirements to make sure you checked every box.

;; Using get-record implementation for devision-1
;; ((get 'get-record 'division-1) division-1-personnel-file)

;; B. To get a salary from any personnel file, the division file type will need to be known. Then all you would have to do would be
;;    to request the correct procedure by type (assuming the package was installed) and then you would only have to pass the file to
;;    the procedure:
;; ((get 'get-salary 'division-2) division-2-personnel-file)


;; C.
;; ((get 'find-employee-record 'division-1) division-1-files)

;; D. During a company takeover, Insatiable would have to audit their file structure and maybe make modifications to ensure the following:
;;    1. Company specific selectors would have to be created within the new package to select the type of each entity.
;;       This is because the type names may be different even if they have the same meaning, the type infomration could also be stored
;;       in a different location. So procedures for selecting and then converting to the correct type to then provide to the operation-and-type
;;       table would have to be created.
;;    2. The required employee data such as salary and address are required.
;;    3. Regular selectors and constructors will need to be created and installed via a new package (for get-record, get-salary, etc)

