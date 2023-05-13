/* globals Shiny,$,CustomEvent */

const epoxyOutputBinding = new Shiny.OutputBinding()

$.extend(epoxyOutputBinding, {
  find: function (scope) {
    return $(scope).find('.epoxy-html')
  },
  _is_empty: function (x) {
    if (x === undefined || x === null) return true
    if (typeof x === 'number') return false
    if (typeof x === 'string') return false
    if (typeof x === 'boolean') return false
    if (Array.isArray(x) && x.length) return false
    if (x instanceof Object && Object.keys(x).length) return false
    return true
  },
  _deepEqual (x, y) {
    if (x === y) {
      return true
    }

    if (
      typeof x !== 'object' ||
      typeof y !== 'object' ||
      x === null ||
      y === null
    ) {
      return false
    }

    const keysX = Object.keys(x)
    const keysY = Object.keys(y)

    if (keysX.length !== keysY.length) {
      return false
    }

    for (const key of keysX) {
      if (!keysY.includes(key) || !this._deepEqual(x[key], y[key])) {
        return false
      }
    }

    return true
  },
  _last: null,
  renderValue: function (el, data) {
    // remove copies of epoxyItem (the first item is the pattern)
    el.querySelectorAll('[data-epoxy-copy]').forEach(e =>
      e.parentElement.removeChild(e)
    )

    const outputId = el.id

    const items = el.querySelectorAll('[data-epoxy-item]')
    items.forEach(item => {
      item.classList.remove('epoxy-item__placeholder')
      const itemName = item.dataset.epoxyItem
      const asHTML = item.dataset.epoxyAsHtml === 'true'
      const replaceContents = (el, contents) => {
        asHTML ? (el.innerHTML = contents) : (el.textContent = contents)
      }

      let itemData = data[itemName]

      if (this._last && this._deepEqual(itemData, this._last[itemName])) {
        // don't do anything, the value hasn't changed
        return
      }

      if (this._is_empty(itemData)) {
        item.style.display = 'none'
        return
      } else {
        item.style.removeProperty('display')
      }

      const evData = { output: outputId, name: itemName }

      if (itemData instanceof Array) {
        replaceContents(item, itemData[0])
        const itemParent = item.parentElement
        itemData = itemData.slice(1)
        item.dispatchEvent(
          new CustomEvent('epoxy-update', {
            bubbles: true,
            detail: { ...evData, data: itemData[0] }
          })
        )

        for (const itemDataThis of itemData) {
          const itemNew = item.cloneNode()
          itemNew.removeAttribute('data-epoxy-item')
          itemNew.dataset.epoxyCopy = itemName
          replaceContents(itemNew, itemDataThis)
          itemParent.insertAdjacentElement('beforeend', itemNew)
          itemNew.dispatchEvent(
            new CustomEvent('epoxy-update', {
              bubbles: true,
              detail: { ...evData, data: itemDataThis }
            })
          )
        }
      } else {
        replaceContents(item, itemData)
        item.dispatchEvent(
          new CustomEvent('epoxy-update', {
            bubbles: true,
            detail: { ...evData, data: itemData }
          })
        )
      }
    })

    this._last = data
    el.classList.remove('epoxy-init')
  },
  renderError: function (el, err) {
    this.clearError(el)
    if (err.message !== '') {
      console.error('[epoxy] ' + err.message)
      el.classList.add('epoxy-error')
    }
  },
  clearError: function (el) {
    el.classList.remove('epoxy-error')
  }
})

Shiny.outputBindings.register(epoxyOutputBinding, 'shiny.ui_epoxy_html')

/* Add epoxy styles to the document */
const epoxyStyle = document.createElement('style')

epoxyStyle.innerHTML = `.epoxy-html.recalculating { opacity: 1; }

.epoxy-html.recalculating [data-epoxy-item],
.epoxy-html.recalculating [data-epoxy-copy] {
  animation-name: epoxy-pulse;
  animation-direction: alternate;
  animation-iteration-count: infinite;
  animation-duration: 1s;
  animation-delay: 1s;
}

@keyframes epoxy-pulse {
  0% { opacity: 1; }
  100% { opacity: 0.3; }
}`

document.head.appendChild(epoxyStyle)
