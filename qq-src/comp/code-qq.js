// Create a class for the element
// class WordCount extends HTMLParagraphElement {
class QQ extends HTMLElement {
  constructor() {
    // Always call super first in constructor
    super();
  }

  connectedCallback() {

    // const sheet = new CSSStyleSheet();
    // sheet.replaceSync(`
    //   .code-qq-source {
    //     color: green;
    //   }
    //   .code-qq-report {
    //     color: red;
    //   }
    //   `);


    // Create a shadow root
    const shadow = this.attachShadow({ mode: 'open' });
    // shadow.adoptedStyleSheets = [sheet];

    // Create text node and add word count to it
    const code = document.createElement('div');

    code.setAttribute("contenteditable", "true")
    code.setAttribute('class', "code-qq-source");
    code.textContent = this.textContent;

    const report = document.createElement('div');
    report.setAttribute('class', "code-qq-report");

    function countWords(text) {
      return text.trim().split(/\s+/g).filter(a => a.trim().length > 0).length;
    }

    function updateReport() {
      const sourceCode = code.textContent;
      report.textContent = `Words: ${countWords(sourceCode)}`;
    }

    code.addEventListener("input", function () {
      console.log("input event fired");
      updateReport();

    }, false);

    // Append it to the shadow root
    shadow.innerHTML = `
    <style>
      .code-qq-source {
        color: violet;
      }
      .code-qq-report {
        color: red;
      }
      </style
    `;
    shadow.appendChild(code);
    shadow.appendChild(document.createElement('p'));
    shadow.appendChild(report);

    updateReport();
  }
}

// Define the new element
// customElements.define('word-count', WordCount, { extends: 'p' });

customElements.define('code-qq', QQ);
