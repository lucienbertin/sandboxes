package main

import (
	"log"
	"sync"

	"github.com/joho/godotenv"
)

func main() {
	log.Print("starting service")
	var wg sync.WaitGroup
	err := godotenv.Load()
	if err != nil {
		log.Print("couldnt load .env file but will try to keep on anyway")
	}

	wg.Add(1)
	go subToRmq(&wg)
	wg.Wait()
}
