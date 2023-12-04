"use strict";

window.Gleam = (function () {
  /* Global Object */
  const self = {};

  /* Public Properties */

  self.hashOffset = undefined;

  /* Public Methods */

  self.getProperty = function (property) {
    let value;
    try {
      value = localStorage.getItem(`Gleam.${property}`);
    } catch (_error) {}
    if (-1 < [null, undefined].indexOf(value)) {
      return gleamConfig[property].values[0].value;
    }
    return value;
  };

  self.icons = function () {
    return Array.from(arguments).reduce(
      (acc, name) =>
        `${acc}
        <svg class="icon icon-${name}"><use xlink:href="#icon-${name}"></use></svg>`,
      ""
    );
  };

  self.scrollToHash = function () {
    const locationHash = arguments[0] || window.location.hash;
    const query = locationHash ? locationHash : "body";
    const hashTop = document.querySelector(query).offsetTop;
    window.scrollTo(0, hashTop - self.hashOffset);
    return locationHash;
  };

  self.toggleSidebar = function () {
    const previousState = bodyClasses.contains("drawer-open")
      ? "open"
      : "closed";

    let state;
    if (0 < arguments.length) {
      state = false === arguments[0] ? "closed" : "open";
    } else {
      state = "open" === previousState ? "closed" : "open";
    }

    bodyClasses.remove(`drawer-${previousState}`);
    bodyClasses.add(`drawer-${state}`);

    if ("open" === state) {
      document.addEventListener("click", closeSidebar, false);
    }
  };

  /* Private Properties */

  const html = document.documentElement;
  const body = document.body;
  const bodyClasses = body.classList;
  const sidebar = document.querySelector(".sidebar");
  const sidebarToggles = document.querySelectorAll(".sidebar-toggle");
  const displayControls = document.createElement("div");

  displayControls.classList.add("display-controls");
  sidebar.appendChild(displayControls);

  /* Private Methods */

  const initProperty = function (property) {
    const config = gleamConfig[property];

    displayControls.insertAdjacentHTML(
      "beforeend",
      config.values.reduce(
        (acc, item, index) => {
          const tooltip = item.label
            ? `alt="${item.label}" title="${item.label}"`
            : "";
          let inner;
          if (item.icons) {
            inner = self.icons(...item.icons);
          } else if (item.label) {
            inner = item.label;
          } else {
            inner = "";
          }
          return `
            ${acc}
            <span class="label label-${index}" ${tooltip}>
              ${inner}
            </span>
          `;
        },
        `<button
          id="${property}-toggle"
          class="control control-${property} toggle toggle-0">
        `
      ) +
        `
        </button>
      `
    );

    setProperty(null, property, function () {
      return self.getProperty(property);
    });
  };

  const setProperty = function (_event, property) {
    const previousValue = self.getProperty(property);

    const update =
      2 < arguments.length ? arguments[2] : gleamConfig[property].update;
    const value = update();

    try {
      localStorage.setItem("Gleam." + property, value);
    } catch (_error) {}

    bodyClasses.remove(`${property}-${previousValue}`);
    bodyClasses.add(`${property}-${value}`);

    const isDefault = value === gleamConfig[property].values[0].value;
    const toggleClasses = document.querySelector(
      `#${property}-toggle`
    ).classList;
    toggleClasses.remove(`toggle-${isDefault ? 1 : 0}`);
    toggleClasses.add(`toggle-${isDefault ? 0 : 1}`);

    try {
      gleamConfig[property].callback(value);
    } catch (_error) {}

    return value;
  };

  const setHashOffset = function () {
    const el = document.createElement("div");
    el.style.cssText = `
      height: var(--hash-offset);
      pointer-events: none;
      position: absolute;
      visibility: hidden;
      width: 0;
      `;
    body.appendChild(el);
    self.hashOffset = parseInt(
      getComputedStyle(el).getPropertyValue("height") || "0"
    );
    body.removeChild(el);
  };

  const closeSidebar = function (event) {
    if (!event.target.closest(".sidebar-toggle")) {
      document.removeEventListener("click", closeSidebar, false);
      self.toggleSidebar(false);
    }
  };

  const addEvent = function (el, type, handler) {
    if (el.attachEvent) el.attachEvent("on" + type, handler);
    else el.addEventListener(type, handler);
  };

  const searchLoaded = function (index, docs) {
    const preview_words_after = 10;
    const preview_words_before = 5;
    const previews = 3;

    const searchInput = document.getElementById("search-input");
    const searchNavButton = document.getElementById("search-nav-button");
    const searchResults = document.getElementById("search-results");
    let currentInput;
    let currentSearchIndex = 0;

    function showSearch() {
      document.documentElement.classList.add("search-active");
    }

    searchNavButton.addEventListener("click", function (e) {
      e.stopPropagation();
      showSearch();
      setTimeout(function () {
        searchInput.focus();
      }, 0);
    });

    function hideSearch() {
      document.documentElement.classList.remove("search-active");
    }

    function update() {
      currentSearchIndex++;

      const input = searchInput.value;
      showSearch();
      if (input === currentInput) {
        return;
      }
      currentInput = input;
      searchResults.innerHTML = "";
      if (input === "") {
        return;
      }

      let results = index.query(function (query) {
        const tokens = lunr.tokenizer(input);
        query.term(tokens, {
          boost: 10,
        });
        query.term(tokens, {
          boost: 5,
          wildcard: lunr.Query.wildcard.TRAILING,
        });
        query.term(tokens, {
          wildcard: lunr.Query.wildcard.LEADING | lunr.Query.wildcard.TRAILING,
        });
      });

      if (results.length == 0 && input.length > 2) {
        const tokens = lunr.tokenizer(input).filter(function (token, i) {
          return token.str.length < 20;
        });
        if (tokens.length > 0) {
          results = index.query(function (query) {
            query.term(tokens, {
              editDistance: Math.round(Math.sqrt(input.length / 2 - 1)),
            });
          });
        }
      }

      if (results.length == 0) {
        const noResultsDiv = document.createElement("div");
        noResultsDiv.classList.add("search-no-result");
        noResultsDiv.innerText = "No results found";
        searchResults.appendChild(noResultsDiv);
      } else {
        const resultsList = document.createElement("ul");
        resultsList.classList.add("search-results-list");
        searchResults.appendChild(resultsList);

        addResults(resultsList, results, 0, 10, 100, currentSearchIndex);
      }

      function addResults(
        resultsList,
        results,
        start,
        batchSize,
        batchMillis,
        searchIndex
      ) {
        if (searchIndex != currentSearchIndex) {
          return;
        }
        for (let i = start; i < start + batchSize; i++) {
          if (i == results.length) {
            return;
          }
          addResult(resultsList, results[i]);
        }
        setTimeout(function () {
          addResults(
            resultsList,
            results,
            start + batchSize,
            batchSize,
            batchMillis,
            searchIndex
          );
        }, batchMillis);
      }

      function addResult(resultsList, result) {
        const doc = docs[result.ref];
        const resultsListItem = document.createElement("li");
        resultsListItem.classList.add("search-results-list-item");
        resultsList.appendChild(resultsListItem);
        const resultLink = document.createElement("a");
        resultLink.classList.add("search-result");
        resultLink.setAttribute("href", `${window.unnest}/${doc.url}`);
        resultsListItem.appendChild(resultLink);
        const resultTitle = document.createElement("div");
        resultTitle.classList.add("search-result-title");
        resultLink.appendChild(resultTitle);
        const resultDoc = document.createElement("div");
        resultDoc.classList.add("search-result-doc");
        resultDoc.innerHTML =
          '<svg viewBox="0 0 24 24" class="search-result-icon"><use xlink:href="#icon-svg-doc"></use></svg>';
        resultTitle.appendChild(resultDoc);
        const resultDocTitle = document.createElement("div");
        resultDocTitle.classList.add("search-result-doc-title");
        resultDocTitle.innerHTML = doc.doc;
        resultDoc.appendChild(resultDocTitle);
        let resultDocOrSection = resultDocTitle;
        if (doc.doc != doc.title) {
          resultDoc.classList.add("search-result-doc-parent");
          const resultSection = document.createElement("div");
          resultSection.classList.add("search-result-section");
          resultSection.innerHTML = doc.title;
          resultTitle.appendChild(resultSection);
          resultDocOrSection = resultSection;
        }
        const metadata = result.matchData.metadata;
        const titlePositions = [];
        const contentPositions = [];
        for (let j in metadata) {
          const meta = metadata[j];
          if (meta.title) {
            const positions = meta.title.position;
            for (let k in positions) {
              titlePositions.push(positions[k]);
            }
          }
          if (meta.content) {
            const positions = meta.content.position;
            for (let k in positions) {
              const position = positions[k];
              let previewStart = position[0];
              let previewEnd = position[0] + position[1];
              let ellipsesBefore = true;
              let ellipsesAfter = true;
              for (let k = 0; k < preview_words_before; k++) {
                const nextSpace = doc.content.lastIndexOf(
                  " ",
                  previewStart - 2
                );
                const nextDot = doc.content.lastIndexOf(". ", previewStart - 2);
                if (nextDot >= 0 && nextDot > nextSpace) {
                  previewStart = nextDot + 1;
                  ellipsesBefore = false;
                  break;
                }
                if (nextSpace < 0) {
                  previewStart = 0;
                  ellipsesBefore = false;
                  break;
                }
                previewStart = nextSpace + 1;
              }
              for (let k = 0; k < preview_words_after; k++) {
                const nextSpace = doc.content.indexOf(" ", previewEnd + 1);
                const nextDot = doc.content.indexOf(". ", previewEnd + 1);
                if (nextDot >= 0 && nextDot < nextSpace) {
                  previewEnd = nextDot;
                  ellipsesAfter = false;
                  break;
                }
                if (nextSpace < 0) {
                  previewEnd = doc.content.length;
                  ellipsesAfter = false;
                  break;
                }
                previewEnd = nextSpace;
              }
              contentPositions.push({
                highlight: position,
                previewStart: previewStart,
                previewEnd: previewEnd,
                ellipsesBefore: ellipsesBefore,
                ellipsesAfter: ellipsesAfter,
              });
            }
          }
        }
        if (titlePositions.length > 0) {
          titlePositions.sort(function (p1, p2) {
            return p1[0] - p2[0];
          });
          resultDocOrSection.innerHTML = "";
          addHighlightedText(
            resultDocOrSection,
            doc.title,
            0,
            doc.title.length,
            titlePositions
          );
        }
        if (contentPositions.length > 0) {
          contentPositions.sort(function (p1, p2) {
            return p1.highlight[0] - p2.highlight[0];
          });
          let contentPosition = contentPositions[0];
          let previewPosition = {
            highlight: [contentPosition.highlight],
            previewStart: contentPosition.previewStart,
            previewEnd: contentPosition.previewEnd,
            ellipsesBefore: contentPosition.ellipsesBefore,
            ellipsesAfter: contentPosition.ellipsesAfter,
          };
          const previewPositions = [previewPosition];
          for (let j = 1; j < contentPositions.length; j++) {
            contentPosition = contentPositions[j];
            if (previewPosition.previewEnd < contentPosition.previewStart) {
              previewPosition = {
                highlight: [contentPosition.highlight],
                previewStart: contentPosition.previewStart,
                previewEnd: contentPosition.previewEnd,
                ellipsesBefore: contentPosition.ellipsesBefore,
                ellipsesAfter: contentPosition.ellipsesAfter,
              };
              previewPositions.push(previewPosition);
            } else {
              previewPosition.highlight.push(contentPosition.highlight);
              previewPosition.previewEnd = contentPosition.previewEnd;
              previewPosition.ellipsesAfter = contentPosition.ellipsesAfter;
            }
          }
          const resultPreviews = document.createElement("div");
          resultPreviews.classList.add("search-result-previews");
          resultLink.appendChild(resultPreviews);
          const content = doc.content;
          for (
            let j = 0;
            j < Math.min(previewPositions.length, previews);
            j++
          ) {
            const position = previewPositions[j];
            const resultPreview = document.createElement("div");
            resultPreview.classList.add("search-result-preview");
            resultPreviews.appendChild(resultPreview);
            if (position.ellipsesBefore) {
              resultPreview.appendChild(document.createTextNode("... "));
            }
            addHighlightedText(
              resultPreview,
              content,
              position.previewStart,
              position.previewEnd,
              position.highlight
            );
            if (position.ellipsesAfter) {
              resultPreview.appendChild(document.createTextNode(" ..."));
            }
          }
        }
        const resultRelUrl = document.createElement("span");
        resultRelUrl.classList.add("search-result-rel-url");
        resultRelUrl.innerText = doc.url;
        resultTitle.appendChild(resultRelUrl);
      }

      function addHighlightedText(parent, text, start, end, positions) {
        let index = start;
        for (let i in positions) {
          const position = positions[i];
          const span = document.createElement("span");
          span.innerHTML = text.substring(index, position[0]);
          parent.appendChild(span);
          index = position[0] + position[1];
          const highlight = document.createElement("span");
          highlight.classList.add("search-result-highlight");
          highlight.innerHTML = text.substring(position[0], index);
          parent.appendChild(highlight);
        }
        const span = document.createElement("span");
        span.innerHTML = text.substring(index, end);
        parent.appendChild(span);
      }
    }

    addEvent(searchInput, "focus", function () {
      setTimeout(update, 0);
    });

    addEvent(searchInput, "keyup", function (e) {
      switch (e.keyCode) {
        case 27: // When esc key is pressed, hide the results and clear the field
          searchInput.value = "";
          break;
        case 38: // arrow up
        case 40: // arrow down
        case 13: // enter
          e.preventDefault();
          return;
      }
      update();
    });

    addEvent(searchInput, "keydown", function (e) {
      let active;
      switch (e.keyCode) {
        case 38: // arrow up
          e.preventDefault();
          active = document.querySelector(".search-result.active");
          if (active) {
            active.classList.remove("active");
            if (active.parentElement.previousSibling) {
              const previous =
                active.parentElement.previousSibling.querySelector(
                  ".search-result"
                );
              previous.classList.add("active");
            }
          }
          return;
        case 40: // arrow down
          e.preventDefault();
          active = document.querySelector(".search-result.active");
          if (active) {
            if (active.parentElement.nextSibling) {
              const next =
                active.parentElement.nextSibling.querySelector(
                  ".search-result"
                );
              active.classList.remove("active");
              next.classList.add("active");
            }
          } else {
            const next = document.querySelector(".search-result");
            if (next) {
              next.classList.add("active");
            }
          }
          return;
        case 13: // enter
          e.preventDefault();
          active = document.querySelector(".search-result.active");
          if (active) {
            active.click();
          } else {
            const first = document.querySelector(".search-result");
            if (first) {
              first.click();
            }
          }
          return;
      }
    });

    addEvent(document, "click", function (e) {
      if (e.target != searchInput) {
        hideSearch();
      }
    });
  };

  self.initSearch = function initSeach(docs) {
    // enable support for hyphenated search words
    lunr.tokenizer.separator = /[\s/]+/;

    const index = lunr(function () {
      this.ref("id");
      this.field("title", { boost: 200 });
      this.field("content", { boost: 2 });
      this.field("url");
      this.metadataWhitelist = ["position"];

      for (let [i, entry] of docs.entries()) {
        this.add({
          id: i,
          title: entry.title,
          content: entry.content,
          url: `${window.unnest}/${entry.url}`,
        });
      }
    });

    searchLoaded(index, docs);
  };

  const init = function () {
    for (let property in gleamConfig) {
      initProperty(property);
      const toggle = document.querySelector(`#${property}-toggle`);
      toggle.addEventListener("click", function (event) {
        setProperty(event, property);
      });
    }

    sidebarToggles.forEach(function (sidebarToggle) {
      sidebarToggle.addEventListener("click", function (event) {
        event.preventDefault();
        self.toggleSidebar();
      });
    });

    setHashOffset();
    window.addEventListener("load", function (_event) {
      self.scrollToHash();
    });
    window.addEventListener("hashchange", function (_event) {
      self.scrollToHash();
    });

    document
      .querySelectorAll(
        `
      .module-name > a,
      .member-name a[href^='#']
    `
      )
      .forEach(function (title) {
        title.innerHTML = title.innerHTML.replace(
          /([A-Z])|([_/])/g,
          "$2<wbr>$1"
        );
      });
  };

  /* Initialise */

  init();

  return self;
})();
