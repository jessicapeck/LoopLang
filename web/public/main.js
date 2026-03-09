async function compile() {
    const editor = document.getElementById('editor');
    const codeInput = editor.value;

    const outputArea = document.getElementById('output-area');

    const response = await fetch('/compile', {
        method: 'POST',
        headers: {
            'Content-Type': 'application/json'
        },
        body: JSON.stringify({ code: codeInput })
    });

    const data = await response.json();

    if (data.error) {
        outputArea.textContent = data.error;
    } else {
        outputArea.textContent = data.result;
    }
}

document.addEventListener('DOMContentLoaded', () => {
    const compileButton = document.getElementById('compile-button');
    compileButton.addEventListener('click', compile);

    const editor = document.getElementById('editor');
    editor.addEventListener('keydown', function(e) {
        if (e.key === 'Tab') {
            e.preventDefault();

            const start = this.selectionStart;
            const end = this.selectionEnd;

            this.value = this.value.substring(0, start) + '\t' + this.value.substring(end);

            this.selectionStart = this.selectionEnd = start + 1;
        }
    });
});