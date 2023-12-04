package main

import (
	"bufio"
	"fmt"
	"os"
	"strconv"
	"strings"
	"unicode"
)

func main() {
	if err := start(); err != nil {
		fmt.Println("Encountered an unexpected error.")
		fmt.Println(err)
		os.Exit(1)
	}
}

func start() error {
	inputPath, err := processArguments()
	if err != nil {
		return err
	}

	file, err := os.Open(inputPath)
	if err != nil {
		return err
	}
	defer file.Close()
	scanner := bufio.NewScanner(file)

	var (
		cardCounter = NewCounter()
		totalPoints = 0
	)
	for scanner.Scan() {
		err := processRawCard(scanner.Text(), &totalPoints, cardCounter)
		if err != nil {
			return err
		}
	}

	fmt.Printf("Part 1: %d\n", totalPoints)
	fmt.Printf("Part 2: %d\n", cardCounter.Sum())

	return nil
}

func processArguments() (string, error) {
	switch {
	case len(os.Args) != 2:
		return "", fmt.Errorf("no input file provided")
	case len(os.Args) > 2:
		return "", fmt.Errorf("multiple input files provided")
	default:
		return os.Args[1], nil
	}
}

func processRawCard(rawCard string, totalPoints *int, cardCounter *CardCounter) error {
	card, err := NewCard(rawCard)
	switch {
	case cardCounter == nil:
		return fmt.Errorf("cardCounter should be set")
	case totalPoints == nil:
		return fmt.Errorf("totalPoints should be set")
	case err != nil:
		return err
	}
	*totalPoints += card.Points()
	cardCounter.IncreaseBy(card.ID, 1)
	for i := range card.Matches() {
		cardCounter.IncreaseBy(card.ID+i+1, cardCounter.Get(card.ID))
	}
	return nil
}

// --- Card --- //

type Card struct {
	ID             int
	WinningNumbers []int
	GivenNumbers   []int
}

// NewCard creates a Card struct from a string like
//   Card 1: 41 48 | 83 86  6
func NewCard(raw string) (*Card, error) {
	var (
		card  = Card{}
		stage = 0
		runes = []rune{}
	)
	for _, ch := range raw + " " {
		if unicode.IsDigit(ch) {
			runes = append(runes, ch)
			continue
		}
		if len(runes) != 0 {
			number, err := strconv.Atoi(string(runes))
			if err != nil {
				return nil, err
			}
			switch stage {
			case 0:
				card.ID = number
			case 1:
				card.WinningNumbers = append(card.WinningNumbers, number)
			case 2:
				card.GivenNumbers = append(card.GivenNumbers, number)
			default:
				return nil, fmt.Errorf("Unexpected stage: %d", stage)
			}
		}
		if ch == ':' || ch == '|' {
			stage++
		}
		runes = []rune{}
	}
	return &card, nil
}

func (c Card) String() string {
	return fmt.Sprintf(
		"Card %d: %s | %s",
		c.ID,
		strings.Trim(fmt.Sprintf("%2d", c.WinningNumbers), "[]"),
		strings.Trim(fmt.Sprintf("%2d", c.GivenNumbers), "[]"),
	)
}

// Points calculates the value of the card based on the number of matches.
func (c Card) Points() int {
	res := len(c.Matches())
	if res == 0 {
		return 0
	}
	return 2 << (res - 1)
}

// Matches compares GivenNumbers with WinningNumbers and returns the
// numbers that both slices contain.
func (c Card) Matches() []int {
	result := []int{}
	for _, given := range c.GivenNumbers {
		for _, winning := range c.WinningNumbers {
			if given != winning {
				continue
			}
			result = append(result, winning)
			break
		}
	}
	return result
}

// --- CardCounter ---

type CardCounter struct {
	totals map[int]int
}

func NewCounter() *CardCounter {
	return &CardCounter{totals: map[int]int{}}
}

func (counter *CardCounter) IncreaseBy(id int, amount int) {
	counter.totals[id] += amount
}

func (counter CardCounter) Get(id int) int {
	return counter.totals[id]
}

func (counter CardCounter) Sum() int {
	result := 0
	for _, amount := range counter.totals {
		result += amount
	}
	return result
}
