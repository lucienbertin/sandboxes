package main

import (
	"encoding/json"
	"log"
	"sync"

	"os"

	amqp "github.com/rabbitmq/amqp091-go"
)

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
