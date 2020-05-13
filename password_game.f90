MODULE word_tools
IMPLICIT NONE
! The number of words similar to the secret word
INTEGER, PARAMETER :: MAX_SIMILAR = 3
INTEGER, PARAMETER :: MAX_LINE_LENGTH = 65
! Store words from a word list, organized by length. 900 is the most there is of
! any single word length in the word list being used.
CHARACTER(len=16), DIMENSION(16,900) :: length_dict
! Keep track of how many words there are at each possible length.
INTEGER, DIMENSION(16) :: counters
CONTAINS

  ! Load words from a word list and organize them by length.
  SUBROUTINE load_words()
  IMPLICIT NONE
  CHARACTER(len=80) :: msg
  INTEGER :: status
  CHARACTER(len=16) :: word ! 16 is the length of the longest word in our file
  CHARACTER(len=16), DIMENSION(5000) :: words ! There's 5000 words in our file
  INTEGER :: i
  OPEN(UNIT=13,FILE="resources/top5000.txt",STATUS='OLD',ACTION='READ', &
       IOSTAT=status,IOMSG=msg)
  fileopen: IF (status /= 0) THEN
    WRITE(*, *) msg
  ELSE
    READ(13, '(A)') words
    counters = 1
    length_dict = ''
    DO i = 1, SIZE(words)
      length_dict( LEN_TRIM(words(i)), counters( LEN_TRIM(words(i)) ) ) = words(i)
      counters(LEN_TRIM(words(i))) = counters(LEN_TRIM(words(i))) + 1
    END DO
    counters = counters - 1
  END IF fileopen
  CLOSE(13)
  END SUBROUTINE load_words

  ! Get a random word from the word list
  SUBROUTINE get_rand_word(w)
  IMPLICIT NONE
  CHARACTER(len=*), INTENT(OUT) :: w
  INTEGER :: i, j
  i = LEN_TRIM(w)
  j =  CEILING(RAND() * counters(i))
  w = TRIM(length_dict(i, j))
  END SUBROUTINE get_rand_word

  ! Keep getting a random word until it doesn't match a given word
  SUBROUTINE get_decoy_word(d, sim)
  IMPLICIT NONE
  CHARACTER(len=*), INTENT(OUT) :: d
  CHARACTER(len=*), DIMENSION(MAX_SIMILAR), INTENT(IN) :: sim
  DO
    CALL get_rand_word(d)
    !IF (d /= w) EXIT
    IF (ALL(sim /= d)) EXIT
  END DO
  END SUBROUTINE get_decoy_word

  ! Count the number of letters in a guess that were at the correct position
  SUBROUTINE count_correct(g, w, c)
  IMPLICIT NONE
  CHARACTER(len=*), INTENT(IN) :: g, w ! The user's guess and the secret word
  INTEGER, INTENT(OUT) :: c ! The number of correct letters
  INTEGER :: i
  c = 0
  DO i = 1, LEN(w)
    IF (g(i:i) == w(i:i)) THEN
      c = c + 1
    END IF
  END DO
  END SUBROUTINE count_correct

  ! Get words that are similar to a given word
  SUBROUTINE get_similar(w, sim)
  IMPLICIT NONE
  CHARACTER(len=*), INTENT(IN) :: w
  CHARACTER(len=LEN(w)), DIMENSION(MAX_SIMILAR), INTENT(OUT) :: sim
  CHARACTER(len=LEN(w)) :: candidate
  INTEGER :: i, j, k, l, same, found
  found = 0
  ! Iterate through words of LEN(w) in length_dict. Add words that have
  ! LEN(w) - i letters in common with w.
  DO i = 1, LEN(w) - 1
    ! Iterate through all available words of a given length. The number of
    ! available words for each length is stored in counters.
    DO j = 1, counters(LEN(w))
      ! If there are LEN(w) - j shared letters at k positions, add word to sim.
      same = 0
      candidate = length_dict(LEN(w),j)
      ! If candidate is already in sim, skip it.
      IF (ANY(sim == candidate)) CYCLE
      DO k = 1, LEN(w)
        IF (w(k:k) == candidate(k:k)) THEN
          same = same + 1
        END IF
      END DO
      ! If there are enough letters in common, add candidate word to sim
      IF (same == LEN(w) - i) THEN
        sim(found + 1) = candidate
        found = found + 1
        !WRITE(*, *) candidate, LEN(w) - i
        IF (LEN(w) - i > 1) EXIT
      END IF
      IF (found == MAX_SIMILAR) EXIT
    END DO
    IF (found == MAX_SIMILAR) EXIT
  END DO
  END SUBROUTINE get_similar

  SUBROUTINE shuffle(arr)
  IMPLICIT NONE
  CHARACTER(len=*), DIMENSION(:), INTENT(INOUT) :: arr
  CHARACTER(len=LEN(arr(1))) :: temp
  INTEGER :: i, r
  !WRITE(*, '(5A8)') (arr(i), i = 1, SIZE(arr))
  DO i = 1, SIZE(arr)
    !WRITE(*, *) arr(i)
    r = CEILING(RAND() * (MAX_SIMILAR + 2))
    temp = arr(i)
    arr(i) = arr(r)
    arr(r) = temp
    !WRITE(*, *)
  END DO
  !WRITE(*, '(5A8)') (arr(i), i = 1, SIZE(arr))
  END SUBROUTINE

  ! Convert a string into gibberish. Uses most printable characters in the basic
  ! Latin set.
  SUBROUTINE gib(str)
  IMPLICIT NONE
  CHARACTER(len=*), INTENT(OUT) :: str
  INTEGER :: i
  DO i = 1, LEN(str)
    str(i:i) = ACHAR(CEILING(RAND() * 95) + 31)
  END DO
  END SUBROUTINE gib

  SUBROUTINE print_play_field()
  IMPLICIT NONE
  !CHARACTER(len=MAX_LINE_LENGTH), DIMENSION()
  END SUBROUTINE print_play_field

END MODULE word_tools

PROGRAM password_game
USE word_tools
IMPLICIT NONE
INTEGER, PARAMETER :: MAX_ATTEMPTS = 3
INTEGER :: i, j, n, offset, level, correct
CHARACTER(len=:), ALLOCATABLE :: w, decoy, guess ! The secret word, decoy, and player guess
CHARACTER(len=:), ALLOCATABLE :: s(:) ! Words that are similar to the secret word
CHARACTER(len=:), ALLOCATABLE :: game_words(:) ! The secret word, similar words, and a decoy
LOGICAL :: locked ! The fail state of the game
CALL SYSTEM_CLOCK(i)
CALL SRAND((i + 1)**2)
offset = -1
level = -1
DO
  IF (locked) EXIT
  offset = MOD(offset + 1, 3)
  n = offset + 5
  level = level + 1
  !WRITE(*, *) "Enter a word length between 2 and 14"
  !READ(*, *) n
  IF (ALLOCATED(w)) DEALLOCATE(w)
  IF (ALLOCATED(decoy)) DEALLOCATE(decoy)
  IF (ALLOCATED(guess)) DEALLOCATE(guess)
  IF (ALLOCATED(s)) DEALLOCATE(s)
  IF (ALLOCATED(game_words)) DEALLOCATE(game_words)
  ALLOCATE(CHARACTER(len=n) :: w, decoy, guess)
  ALLOCATE(CHARACTER(len=n) :: s(MAX_SIMILAR))
  ALLOCATE(CHARACTER(len=n) :: game_words(MAX_SIMILAR + 2))
  CALL load_words()
  CALL get_rand_word(w)
  CALL get_decoy_word(decoy, s)
  !WRITE(*, '(A,I3,A,1X,A)') "Random word of length", n, ":", w
  CALL get_similar(w, s)
  game_words(1) = w
  game_words(2) = decoy
  game_words(3:MAX_SIMILAR + 2) = s
  CALL shuffle(game_words)
  !WRITE(*, *) "Similar: ", s
  DO i = 1, MAX_ATTEMPTS
    WRITE(*, '(A, I3)') "SECURITY LEVEL:", level
    WRITE(*, '(5A8)') (game_words(j), j = 1, SIZE(game_words))
    WRITE(*, '(2A)') NEW_LINE(' '), "USERNAME: ADMIN"
    WRITE(*, '(A)') "PASSWORD: "
    READ(*, *) guess
    !CALL SLEEP(1)
    CALL count_correct(guess, w, correct)
    WRITE(*, '(A,I2,1X,A,I2)') "MATCH:", correct, "/", n
    IF (guess == w) THEN
      WRITE(*, '(A)') "ACCESS GRANTED"
      EXIT
    ELSE
      IF (i /= MAX_ATTEMPTS) THEN
        WRITE(*, '(A, I2)') "ACCESS DENIED. ATTEMPTS REMAINING: ", MAX_ATTEMPTS - i
      ELSE
        WRITE(*, '(A)') "ACCESS DENIED. SYSTEM LOCKED."
        locked = .TRUE.
      END IF
    END IF
    !WRITE(*, *) "PASSWORD = ", w, " DECOY = ", decoy
  END DO
END DO
END PROGRAM password_game
