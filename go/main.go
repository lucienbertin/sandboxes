package main

import (
	"fmt"
	"log"
	"strconv"

	"os"

	"github.com/joho/godotenv"
	gomail "gopkg.in/mail.v2"
)

func main() {
	log.Print("starting service")

	err := godotenv.Load()
	if err != nil {
		log.Print("couldnt load .env file")
	}
	host := os.Getenv("HOST")
	port, err := strconv.ParseInt(os.Getenv("PORT"), 10, 32)
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
}
