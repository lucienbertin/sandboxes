package main

import (
	"fmt"

	gomail "gopkg.in/mail.v2"
)

func main() {
	// Create a new message
	message := gomail.NewMessage()

	// Set email headers
	message.SetHeader("From", "go@sandboxes.local")
	message.SetHeader("To", "me@lucienbertin.com")
	message.SetHeader("Subject", "Hello from the Mailtrap team")

	// Set email body
	message.SetBody("text/plain", "This is the Test Body")

	// Set up the SMTP dialer
	dialer := gomail.NewDialer("localhost", 25, "", "")

	// Send the email
	if err := dialer.DialAndSend(message); err != nil {
		fmt.Println("Error:", err)
		panic(err)
	} else {
		fmt.Println("Email sent successfully!")
	}
}
