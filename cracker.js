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

document.getElementById("start-button").click();
setTimeout(solve, 0);
