# Working with {pins}

For the **calibration** project, we use the [{pins} package](https://pins.rstudio.com/) as the back-end "database" for storing each workshop user's responses. If you are not yet familiar with {pins}, the [pins_intro.R](pins_intro.R) script demonstrates how to connect to a (local) {pins} board, search for existing pins, and add/remove pins.

## *boards* versus *pins*

The hierarchy between **boards** and **pins** is such that a **board** can contain multiple **pins**. We envision that each *workshop* will have its own **board**, and each *student + question type* (i.e., "binary" or "range" question types) will be a separate **pin** on the *workshop* **board**.

Here's what the tree structure will look like:

    workshop_2022_01_01_board
    |
    |__ binary_student_A_pin
    |
    |__ range_student_A_pin
    |
    |__ binary_student_B_pin
    |
    |__ range_student_B_pin
    .
    .
    .
