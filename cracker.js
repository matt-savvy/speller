let input, wordEl, word, solution;

function solveWord(word) {
    return word.split("").sort();
}

function solve() {
    if (input.value.length === 0) {
        wordEl = document.getElementById("word")
        word = wordEl.textContent;
        solution = solveWord(word);
    }

    if (wordEl) {
        let word = wordEl.textContent;
        let solution = solveWord(word);

        for (let i = 0; i < solution.length; i += 1) {
            applySolution(solution);
        }
        setTimeout(solve);
    }
}

function applySolution(solution) {
    const letter = solution[input.value.length];
    input.value += letter;
    input.dispatchEvent(new Event("input"));
}

function setup() {
    const startButton = document.getElementById("start-button");
    startButton.addEventListener("click", () => setTimeout(() => {
        input = document.getElementById("text-input");
        solve();
    }));
}

setTimeout(setup, 100);
