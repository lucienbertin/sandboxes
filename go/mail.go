package main

import (
	"fmt"
	"log"
	"os"
	"strconv"

	gomail "gopkg.in/mail.v2"
)

func sendmail(to string, subject string, body string) {
	host := os.Getenv("SMTP_HOST")
	port, err := strconv.ParseInt(os.Getenv("SMTP_PORT"), 10, 32)
	if err != nil {
		log.Fatal("port is not a valid int")
	}
	from := os.Getenv("SENDER")

	log.Print("env vars loaded succesfully")
	log.Print("smtp host: " + host)
	log.Print("port: " + strconv.Itoa(int(port)))

	// Create a new message
	message := gomail.NewMessage()

	// Set email headers
	message.SetHeader("From", from)
	message.SetHeader("To", to)
	message.SetHeader("Subject", subject)

	// Set email body
	message.SetBody("text/plain", body)

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
