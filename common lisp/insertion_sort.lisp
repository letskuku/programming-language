(defun solve (lst i) (setq key (nth i lst)) (setq tmp (- i 1)) 
                    (loop while (equal (and (>= tmp 0) (> (nth tmp lst) key)) T) ;end of while
                        do (progn (setf (nth (+ 1 tmp) lst) (nth tmp lst)) 
                                (setf (nth tmp lst) key) 
                                (print lst) ;정렬 과정 출력 
			                    (setq tmp (- tmp 1))
                        ) ;end of do
                    ) ;end of loop
                    (setf (nth (+ 1 tmp) lst) key) 
) ;lst의 i번째 원소를 0~(i-1)번째 원소들과 비교하여 올바른 위치에 삽입


(defun insertionSort (lst) (princ "초기 리스트 ") (princ lst)
                            (loop for i from 1 to 7 do(solve lst i)) ;i번째 원소 정렬 
                            (terpri)
                            (princ "정렬 후 리스트 ") (princ lst)
) ;초기 리스트, 정렬 과정, 정렬 후 리스트를 출력





(insertionSort '(11 33 23 45 13 25 8 135))
(terpri)
(terpri)
(insertionSort '(83 72 65 54 47 33 29 11))
