package main

import (
	"encoding/json"
	"fmt"
	"log"
	"strconv"
	"sync"

	"os"

	"github.com/joho/godotenv"
	amqp "github.com/rabbitmq/amqp091-go"
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

func subToRmq(wg *sync.WaitGroup) {
	url := os.Getenv("AMQP_URL")

	log.Printf("connecting to rmq at url: %s", url)
	conn, err := amqp.Dial(url)
	if err != nil {
		log.Panicf("Failed to connect to RabbitMQ %s", err)
		defer wg.Done()
	}
	log.Print("connection success")

	ch, err := conn.Channel()
	if err != nil {
		log.Panicf("Failed to open a channel %s", err)
		defer wg.Done()
	}

	q, err := ch.QueueDeclare(
		"go-mailer", // name
		false,       // durable
		false,       // delete when unused
		false,       // exclusive
		false,       // no-wait
		nil,         // arguments
	)
	if err != nil {
		log.Panicf("Failed to declare a queue %s", err)
		defer wg.Done()
	}
	msgs, err := ch.Consume(
		q.Name, // queue
		"",     // consumer
		true,   // auto-ack
		false,  // exclusive
		false,  // no-local
		false,  // no-wait
		nil,    // args
	)
	if err != nil {
		log.Panicf("Failed to register a consumer %s", err)
		defer wg.Done()
	}
	go func() {
		for d := range msgs {
			log.Printf("message recieve with body: %s", d.Body)
			var parsedMessage map[string]string
			err := json.Unmarshal([]byte(d.Body), &parsedMessage)
			if err != nil {
				log.Printf("Failed to parse message body %s as json - err: %s", d.Body, err)
				err := d.Reject(false)
				if err != nil {
					log.Printf("Failed to reject message: %s", err)
				}
				log.Print("Message rejected")

			} else {

				log.Printf("after parse, hopefully, recipient: %s", parsedMessage["to"])
				to := parsedMessage["to"]
				subject := parsedMessage["subject"]
				body := parsedMessage["body"]
				log.Printf("message recieve with contentType: %s", d.ContentType)

				sendmail(to, subject, body)
			}
		}
	}()

	// defer ch.Close() // never close
	// defer wg.Done() // never be done
}

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
