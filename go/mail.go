package main

import (
	"fmt"
	"os"
	"strconv"

	gomail "gopkg.in/mail.v2"
)

func sendmail(to string, subject string, body string) error {
	host := os.Getenv("SMTP_HOST")
	port, err := strconv.ParseInt(os.Getenv("SMTP_PORT"), 10, 32)
	if err != nil {
		return fmt.Errorf("env smtp port is not a valid int: %s", err)
	}
	from := os.Getenv("SENDER")

	// Create a new message
	message := gomail.NewMessage()

	// Set email headers
	message.SetHeader("From", from)
	message.SetHeader("To", to)
	message.SetHeader("Subject", subject)

	// Set email body
	message.SetBody("text/plain", body)

	// Set up the SMTP dialer
	dialer := gomail.NewDialer(host, int(port), "", "")

	// Send the email
	if err := dialer.DialAndSend(message); err != nil {
		return fmt.Errorf("mail send error: %s", err)
	}
	return nil
}
