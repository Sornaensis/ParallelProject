package main

import (
	"flag"
	"fmt"
	"runtime"
	"sync"
	"sync/atomic"
	"time"
	"unsafe"
)

const processorCores int = 1

type node struct {
	data      int32
	next      *node
	isRequest bool
}

type queue struct {
	head *node
	tail *node
}

func (q *queue) enq(data int32) {
	offer := &node{data, nil, false}

	for {
		//fmt.Printf("error %d\n", data)

		t := q.head
		h := q.tail

		if h == t || !t.isRequest {
			n := t.next

			if t == q.tail {
				if n != nil {
					atomic.CompareAndSwapUintptr((*uintptr)(unsafe.Pointer(t)),
						(uintptr)(unsafe.Pointer(t)), (uintptr)(unsafe.Pointer(n)))

				} else if atomic.CompareAndSwapUintptr((*uintptr)(unsafe.Pointer(t.next)),
					(uintptr)(unsafe.Pointer(n)), (uintptr)(unsafe.Pointer(offer))) {

					atomic.CompareAndSwapUintptr((*uintptr)(unsafe.Pointer(t)),
						(uintptr)(unsafe.Pointer(t)), (uintptr)(unsafe.Pointer(offer)))

					for offer.data == data {
						/*spin*/
					}
					h = q.head
					if offer == h.next {
						atomic.CompareAndSwapUintptr((*uintptr)(unsafe.Pointer(h)),
							(uintptr)(unsafe.Pointer(n)), (uintptr)(unsafe.Pointer(offer)))
					}
					return
				}

			}

		} else {
			n := h.next
			if t != q.tail || h != q.head || n == nil {

				continue
			}
			success := atomic.CompareAndSwapUintptr((*uintptr)(unsafe.Pointer(n)),
				(uintptr)(unsafe.Pointer(nil)), (uintptr)(unsafe.Pointer(n)))
			if success {
				return
			}
		}

	}
}

func (q *queue) deq(data int32) {
	offer := &node{data, nil, true}

	for {

		t := q.head
		h := q.tail

		if h == t || t.isRequest {
			n := t.next

			if t == q.tail {
				if n != nil {

					atomic.CompareAndSwapUintptr((*uintptr)(unsafe.Pointer(t)), (uintptr)(unsafe.Pointer(t)), (uintptr)(unsafe.Pointer(n)))
				} else if atomic.CompareAndSwapUintptr((*uintptr)(unsafe.Pointer(t.next)), (uintptr)(unsafe.Pointer(n)), (uintptr)(unsafe.Pointer(offer))) {
					atomic.CompareAndSwapUintptr((*uintptr)(unsafe.Pointer(t)), (uintptr)(unsafe.Pointer(t)), (uintptr)(unsafe.Pointer(offer)))
					for offer.data == data {
						/*spin*/
						//fmt.Print("Like a record baby")
					}
					h = q.head
					if offer == h.next {
						atomic.CompareAndSwapUintptr((*uintptr)(unsafe.Pointer(h)), (uintptr)(unsafe.Pointer(n)), (uintptr)(unsafe.Pointer(offer)))
					}
					return
				}

			}

		} else {

			n := h.next
			if t != q.tail || h != q.head || n == nil {

				continue
			}
			success := atomic.CompareAndSwapUintptr((*uintptr)(unsafe.Pointer(n)), (uintptr)(unsafe.Pointer(nil)), (uintptr)(unsafe.Pointer(n)))
			if success {

				fmt.Println(n.data)
				return
			}
		}

	}
}

func threadHelperEnq(q *queue, numOps int, wg *sync.WaitGroup) {
	defer wg.Done()

	for i := 0; i < numOps; i++ {
		//.Print(i)
		//q.enq(0)
	}
}

func threadHelperDeq(q *queue, numOps int, wg *sync.WaitGroup) {
	defer wg.Done()
	for i := 0; i < numOps; i++ {
		//q.deq(0)
	}
}

func main() {
	var wg sync.WaitGroup

	numProducerPtr := flag.Int("numPThreads", 1, "int: the number of producer threads to run")
	numConsumerPtr := flag.Int("numCThreads", 1, "int: the number of threads to run")
	numEnqOpsPtr := flag.Int("numEnqOps", 500000, "int: number of ops each enq should run")
	numDeqOpsPtr := flag.Int("numDeqOps", 500000, "int: number of ops each deq should run")

	que := &queue{&node{}, &node{}}
	//this sets the maximium number of cores to be used by all goroutines
	runtime.GOMAXPROCS(*numProducerPtr + *numConsumerPtr)

	wg.Add(*numProducerPtr + *numConsumerPtr)

	startTime := time.Now()

	for threads := 0; threads < *numProducerPtr; threads++ {
		go threadHelperEnq(que, *numEnqOpsPtr, &wg)
	}

	for threads := 0; threads < *numConsumerPtr; threads++ {
		go threadHelperDeq(que, *numDeqOpsPtr, &wg)
	}

	wg.Wait()

	elasped := time.Since(startTime)
	fmt.Print(elasped, "\n")

}
