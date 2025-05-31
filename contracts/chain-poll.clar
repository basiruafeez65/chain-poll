;; Set contract owner
(define-constant contract-owner tx-sender)

;; Data variables with initial values
(define-map polls uint (tuple (question (string-ascii 128)) (options (list 5 (string-ascii 64))) (is-open bool)))
(define-map votes {poll-id: uint, voter: principal} uint)
(define-data-var poll-count uint u0)
(define-map results {poll-id: uint, option-index: uint} uint)

;; Only contract deployer (owner) can create polls
(define-private (is-owner (sender principal))
  (is-eq sender contract-owner))

;; Create a new poll with question and options
(define-public (create-poll (question (string-ascii 128)) (options (list 5 (string-ascii 64))))
  (let ((new-id (+ (var-get poll-count) u1)))
    (begin
      (asserts! (is-owner tx-sender) (err "Only owner can create polls"))
      (asserts! (> (len options) u0) (err "Must provide at least one option"))
      (var-set poll-count new-id)
      (map-set polls new-id {
        question: question,
        options: options,
        is-open: true
      })
      (ok "Poll created")
    )
  )
)

;; Vote on a poll option (option-index starts from 0)
(define-public (vote (poll-id uint) (option-index uint))
  (begin
    (let ((poll-opt (map-get? polls poll-id)))
      (asserts! (is-some poll-opt) (err "Poll not found"))
      (let ((poll (unwrap! poll-opt (err "Poll not found"))))
        (asserts! (get is-open poll) (err "Poll is closed"))
        (let ((voter-key {poll-id: poll-id, voter: tx-sender}))
          (asserts! (is-none (map-get? votes voter-key)) (err "Already voted"))
          (let ((options (get options poll)))
            (asserts! (< option-index (len options)) (err "Invalid option index"))
            (map-set votes voter-key u1)
            (let ((current-count (default-to u0 (map-get? results {poll-id: poll-id, option-index: option-index}))))
              (map-set results {poll-id: poll-id, option-index: option-index} (+ current-count u1))
              (ok "Vote counted")
            )
          )
        )
      )
    )
  )
)

;; Close poll (owner only)
(define-public (close-poll (poll-id uint))
  (begin
    (asserts! (is-owner tx-sender) (err "Only owner can close polls"))
    (let ((poll-opt (map-get? polls poll-id)))
      (asserts! (is-some poll-opt) (err "Poll not found"))
      (let ((poll (unwrap! poll-opt (err "Poll not found"))))
        (map-set polls poll-id {question: (get question poll), options: (get options poll), is-open: false})
        (ok "Poll closed")
      )
    )
  )
)

;; Read poll info
(define-read-only (get-poll (poll-id uint))
  (map-get? polls poll-id)
)

;; Read vote count for an option
(define-read-only (get-votes (poll-id uint) (option-index uint))
  (default-to u0 (map-get? results {poll-id: poll-id, option-index: option-index}))
)
