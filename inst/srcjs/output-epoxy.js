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
    const outputId = el.id

    const items = el.querySelectorAll('[data-epoxy-item]')
    items.forEach(item => {
      item.classList.remove('epoxy-item__placeholder')
      const itemName = item.dataset.epoxyItem
      const asHTML = item.dataset.epoxyAsHtml === 'true'

      const evData = { output: outputId, name: itemName, outputType: 'html' }

      const updateContents = (el, contents) => {
        asHTML ? (el.innerHTML = contents) : (el.textContent = contents)
        el.dispatchEvent(
          new CustomEvent('epoxy-update', {
            bubbles: true,
            detail: { ...evData, value: contents }
          })
        )
        return el
      }

      // remove copies of epoxyItem (the first item is the pattern)
      const removeCopies = () => {
        el
          .querySelectorAll(`[data-epoxy-copy="${itemName}"]`)
          .forEach(item => item.parentElement.removeChild(item))
      }

      let itemData = data[itemName]

      const errorClasses = ['epoxy-item__error', 'hint--top-right', 'hint--error']

      if (data.__errors__ && data.__errors__.includes(itemName)) {
        errorClasses.forEach(c => item.classList.add(c))
        removeCopies()
        updateContents(item, item.dataset.epoxyPlaceholder || '')
        item.style.removeProperty('display')
        item.setAttribute('aria-label', itemData)
        item.dispatchEvent(
          new CustomEvent('epoxy-error', {
            bubbles: true,
            detail: {
              output: el.id,
              key: itemName,
              message: itemData,
              outputType: 'html'
            }
          })
        )
        return
      } else {
        errorClasses.forEach(c => item.classList.remove(c))
        item.removeAttribute('aria-label')
      }

      if (this._last && this._deepEqual(itemData, this._last[itemName])) {
        // don't do anything, the value hasn't changed
        return
      }

      removeCopies()

      if (this._is_empty(itemData)) {
        item.style.display = 'none'
        return
      } else {
        item.style.removeProperty('display')
      }

      if (!(itemData instanceof Array)) {
        updateContents(item, itemData)
        return
      }

      // If an array, use the initial item as a pattern
      updateContents(item, itemData[0])
      const itemParent = item.parentElement
      itemData = itemData.slice(1)

      for (const itemDataThis of itemData) {
        const itemNew = item.cloneNode()
        itemNew.removeAttribute('data-epoxy-item')
        itemNew.dataset.epoxyCopy = itemName
        itemParent.insertAdjacentElement('beforeend', itemNew)
        updateContents(itemNew, itemDataThis)
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
