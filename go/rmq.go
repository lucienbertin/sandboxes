package main

import (
	"encoding/json"
	"fmt"
	"log"
	"sync"
	"time"

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

	mails_queue, err := ch.QueueDeclare(
		"go-mailer", // name
		false,       // durable
		false,       // delete when unused
		false,       // exclusive
		false,       // no-wait
		nil,         // arguments
	)
	if err != nil {
		log.Panicf("Failed to declare mailer queue %s", err)
		defer wg.Done()
	}
	ch.QueueBind(
		mails_queue.Name, // queue name
		"job.sendmail",   // routing key
		"rust",           // exchange
		false,            // no-wait
		nil,              // arguments
	)

	logs_queue, err := ch.QueueDeclare(
		"go-logger", // name
		false,       // durable
		false,       // delete when unused
		false,       // exclusive
		false,       // no-wait
		nil,         // arguments
	)
	if err != nil {
		log.Panicf("Failed to declare logger queue %s", err)
		defer wg.Done()
	}
	ch.QueueBind(
		logs_queue.Name, // queue name
		"evt.#",         // routing key
		"rust",          // exchange
		false,           // no-wait
		nil,             // arguments
	)
	mail_jobs, err := ch.Consume(
		mails_queue.Name, // queue
		"",               // consumer
		false,            // auto-ack
		false,            // exclusive
		false,            // no-local
		false,            // no-wait
		nil,              // args
	)
	if err != nil {
		log.Panicf("Failed to register a consumer %s", err)
		defer wg.Done()
	}
	go func() {
		for d := range mail_jobs {
			log.Printf("message recieve with body: %s", d.Body)
			err := handleMailJob(d)
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

	logs, err := ch.Consume(
		logs_queue.Name, // queue
		"",              // consumer
		true,            // auto-ack
		false,           // exclusive
		false,           // no-local
		false,           // no-wait
		nil,             // args
	)
	if err != nil {
		log.Panicf("Failed to register a consumer %s", err)
		defer wg.Done()
	}
	go func() {
		f, err := os.OpenFile("./logs/logfile.log", os.O_APPEND|os.O_WRONLY|os.O_CREATE, 0600)
		if err != nil {
			panic(err)
		}

		for d := range logs {
			log.Printf("message recieve with key: %s - body: %s", d.RoutingKey, d.Body)
			err := handleEventLog(d, f)
			if err != nil {
				log.Printf("couldnt write to file: %s", err)
			}
		}

	}()

	// defer ch.Close() // never close
	// defer wg.Done() // never be done
}

func handleMailJob(d amqp.Delivery) error {
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

func handleEventLog(d amqp.Delivery, f *os.File) error {
	l := fmt.Sprintf("%s: %s - %s\n", time.Now(), d.RoutingKey, d.Body)

	_, err := f.WriteString(l)

	return err
}
