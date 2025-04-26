package main

import (
	"fmt"
	"log"
	"strconv"
	"sync"

	"os"

	"github.com/joho/godotenv"
	amqp "github.com/rabbitmq/amqp091-go"
	gomail "gopkg.in/mail.v2"
)

func sendmail(wg *sync.WaitGroup) {
	host := os.Getenv("SMTP_HOST")
	port, err := strconv.ParseInt(os.Getenv("SMTP_PORT"), 10, 32)
	if err != nil {
		log.Fatal("port is not a valid int")
	}

	log.Print("env vars loaded succesfully")
	log.Print("smtp host: " + host)
	log.Print("port: " + strconv.Itoa(int(port)))

	// Create a new message
	message := gomail.NewMessage()

	// Set email headers
	message.SetHeader("From", "go@sandboxes.local")
	message.SetHeader("To", "me@lucienbertin.com")
	message.SetHeader("Subject", "Hello from the go micro service")

	// Set email body
	message.SetBody("text/plain", "This is the Test Body")

	log.Print("message initailised")

	// Set up the SMTP dialer
	dialer := gomail.NewDialer(host, int(port), "", "")

	// Send the email
	if err := dialer.DialAndSend(message); err != nil {
		// log.Fatal("error sending message")
		fmt.Println("Error:", err)
		panic(err)
	} else {
		log.Print("Email sent successfully!")
	}
	defer wg.Done()
}

func subToRmq(wg *sync.WaitGroup) {
	url := os.Getenv("AMQP_URL")

	log.Printf("connecting to rmq at url: %s", url)
	conn, err := amqp.Dial(url)
	if err != nil {
		log.Panicf("Failed to connect to RabbitMQ %s", err)
	}
	log.Print("connection success")
	defer conn.Close()

	defer wg.Done()
}

func main() {
	log.Print("starting service")
	var wg sync.WaitGroup
	err := godotenv.Load()
	if err != nil {
		log.Print("couldnt load .env file but will try to keep on anyway")
	}

	wg.Add(2)
	go sendmail(&wg)
	go subToRmq(&wg)
	wg.Wait()
}
