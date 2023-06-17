function solveWord(word) {
    return word.split("").sort();
}

function solve() {
    let input = document.getElementById("text-input");
    let wordEl = document.getElementById("word")

    if (wordEl) {
        let word = wordEl.textContent;
        let solution = solveWord(word);

        solution.forEach((letter) => {
            input.value += letter;
            input.dispatchEvent(new Event("input"));
        });
        setTimeout(solve, 0);
    }
}

function setup() {
    const startButton = document.getElementById("start-button");
    startButton.addEventListener("click", () => setTimeout(solve));
}

setTimeout(setup, 100);
