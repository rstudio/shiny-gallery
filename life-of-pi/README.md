# Life of Pi: A Monte Carlo Simulation

---

- Developed by: [Zauad Shahreer Abeer](https://github.com/shahreyar-abeer)
- App on gallery: https://gallery.shinyapps.io/life-of-pi
- Original code: https://github.com/shahreyar-abeer/life_of_pi

---

This app is designed to run a Monte Carlo Simulation to estimate the value of π.
I'm sorry to disappoint some of you who might have though it had something to do with the movie.
But I can share a bit of history though.

Pi wasn’t always known as pi. Before the 1700s, people referred to the number we know as pi as
'the quantity which when the diameter is multiplied by it, yields the circumference'.
Not surprisingly, people got tired of saying so much whenever they wanted to talk about Pi.
The Welsh mathematician William Jones, a friend of Sir Isaac Newton, began using the symbol for π in 1706.


## The Algorithm
At first, we inscribe a circle with unit radius (r=1) in a square (length=2r),
note down the area of the circle and the square.
Then we let some points fall freely on the canvass.
The points are independent and may fall at any place within the square.
We then take a note of the poportion (p) of points that have fallen inside the circle to the total number of points.
This proportion gives 1/4 th of π. We then multiply the result with 4 to get an estimate of π.