package main

import (
	"encoding/json"
	"fmt"
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
	deliveries, err := ch.Consume(
		q.Name, // queue
		"",     // consumer
		false,  // auto-ack
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
		for d := range deliveries {
			log.Printf("message recieve with body: %s", d.Body)
			err := handleDelivery(d)
			if err != nil {
				log.Printf("failed to handle message: %s", err)
				d.Reject(false) // might throw but idc
				log.Print("message rejected")
			} else {
				d.Ack(false) // might throw but idc
				log.Print("message acknowledged")
			}
		}

	}()
	// defer ch.Close() // never close
	// defer wg.Done() // never be done
}

func handleDelivery(d amqp.Delivery) error {
	var err error
	var parsedMessage map[string]string
	err = json.Unmarshal([]byte(d.Body), &parsedMessage)
	if err != nil {
		return fmt.Errorf("cant parse message body: %s", err)
	}

	to := parsedMessage["to"]
	subject := parsedMessage["subject"]
	body := parsedMessage["body"]
	err = sendmail(to, subject, body)
	if err != nil {
		return fmt.Errorf("failed to send mail: %s", err)
	}

	return nil
}
