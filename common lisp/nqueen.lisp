(setq list1 (list 0 0 0 0)) ;열 검사하는 리스트
(setq list2 '(0 0 0 0 0 0 0)) ;우상향&좌하향 대각선 검사하는 리스트
(setq list3 (list 0 0 0 0 0 0 0)) ;좌상향&우하향 대각선 검사하는 리스트
(setq result '(0 0 0 0)) ;퀸 배치 저장할 리스트


(defun check (a b) (cond ((= (nth b list1) 1) 1) 
                        ((= (nth (+ a b) list2) 1) 1) 
                        ((= (nth (+ (- a b) 3) list3) 1) 1) 
                        (0)) ;end of cond
) ;a행 b열에서 이동 가능한 방향에 퀸이 놓여있으면 1, 아니면 0을 반환


(defun visit(a b) (setf (nth b list1) 1) 
                (setf (nth (+ a b) list2) 1) 
                (setf (nth (+ (- a b) 3) list3) 1) 
                (setf (nth a result) (+ b 1))
) ;a행 b열에 퀸을 배치하므로 해당하는 리스트들의 원소값을 1로 변경


(defun cancel(a b) (setf (nth b list1) 0) 
                (setf (nth (+ a b) list2) 0) 
                (setf (nth (+ (- a b) 3) list3) 0)
) ;a행 b열에 놓인 퀸 회수하므로 해당하는 리스트들의 원소값 0으로 변경


(defun nqueen (n) (cond ((= n 4) (print result))
                        ((< n 4) (loop for i from 0 to 3
                                    do (cond ((= (check n i) 0) (visit n i) 
					                                        (nqueen (+ n 1)) 
					                                        (cancel n i))
                                    ) ;end of do
                                ) ;end of loop
                        ) ;end of second condition
                    ) ;end of cond
) ;이미 배치된 퀸 개수 인자로 받아 4개면 배치 출력, 아니면 남은 퀸 배치

(nqueen 0)
