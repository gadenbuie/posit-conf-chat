class CollapsibleAbstract extends HTMLElement {
  static get observedAttributes() {
    return ["lines"];
  }

  connectedCallback() {
    if (this.shadowRoot) return;
    const text = (this.getAttribute("text") || this.textContent).trim();
    this.textContent = "";

    const root = this.attachShadow({ mode: "open" });
    root.innerHTML = `
      <style>
        :host { display: block; }
        .wrap { position: relative; }
        .text {
          margin: 0;
          font: inherit;
          color: inherit;
        }
        .wrap.collapsed .text {
          display: -webkit-box;
          -webkit-line-clamp: var(--lines, 2);
          -webkit-box-orient: vertical;
          overflow: hidden;
          -webkit-mask-image: linear-gradient(to bottom, black 55%, transparent);
          mask-image: linear-gradient(to bottom, black 55%, transparent);
        }
        .toggle {
          all: unset;
          cursor: pointer;
          color: var(--bs-link-color, var(--brand-indigo, #6610f2));
          font: inherit;
          font-size: 0.8125rem;
          font-weight: 600;
          display: flex;
          align-items: center;
          justify-content: center;
          gap: 0.25rem;
          margin: 0.125rem auto 0;
        }
        .toggle:hover {
          text-decoration: underline;
          text-underline-offset: 3px;
        }
        .toggle .chev { transition: transform 0.15s ease; }
        .toggle[aria-expanded="true"] .chev { transform: rotate(180deg); }
      </style>
      <div class="wrap collapsed">
        <p class="text"></p>
        <button type="button" class="toggle" aria-expanded="false">
          <span class="label">Show more</span>
          <svg class="chev" width="12" height="12" viewBox="0 0 24 24" fill="none"
            stroke="currentColor" stroke-width="2.5" stroke-linecap="round"
            stroke-linejoin="round" aria-hidden="true">
            <path d="m6 9 6 6 6-6"/>
          </svg>
        </button>
      </div>
    `;

    root.querySelector(".text").textContent = text;

    const wrap = root.querySelector(".wrap");
    const btn = root.querySelector(".toggle");
    const label = root.querySelector(".label");

    const setLines = () => {
      const lines = parseInt(this.getAttribute("lines"), 10) || 2;
      wrap.style.setProperty("--lines", lines);
    };

    const updateToggle = () => {
      const collapsed = wrap.classList.contains("collapsed");
      const text = root.querySelector(".text");
      const clamped = text.scrollHeight > text.clientHeight + 1;
      btn.hidden = !collapsed && !clamped;
    };

    btn.addEventListener("click", () => {
      const collapsed = wrap.classList.toggle("collapsed");
      btn.setAttribute("aria-expanded", collapsed ? "false" : "true");
      label.textContent = collapsed ? "Show more" : "Show less";
      updateToggle();
    });

    setLines();
    updateToggle();

    if (document.fonts && document.fonts.ready) {
      document.fonts.ready.then(() => updateToggle());
    }
  }
}

customElements.define("collapsible-abstract", CollapsibleAbstract);
