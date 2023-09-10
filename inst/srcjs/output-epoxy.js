/* globals Shiny,$,CustomEvent */

class EpoxyHTML extends HTMLElement {
  static is_set_global_event_listener = false

  last = null

  constructor () {
    super()

    if (EpoxyHTML.is_set_global_event_listener) return

    window.addEventListener('epoxy-message', ev => {
      // {example: {thing: "dolphin", color: "blue", height: 5}}
      for (const [key, value] of Object.entries(ev.detail)) {
        const el = document.getElementById(key)
        if (!el) {
          console.warn(`[epoxy] No element with id "${key}"`, { [key]: value })
          continue
        }
        el.updateEpoxyValues(value)
      }
    })
    EpoxyHTML.is_set_global_event_listener = true
  }

  static update (data) {
    // { [id]: { [itemKey]: value }}
    // { example: { thing: "dolphin", color: "blue", height: 5 }}
    const event = new CustomEvent('epoxy-message', { detail: data })
    window.dispatchEvent(event)
  }

  /* ---- Private methods ---- */
  _is_empty (x) {
    if (x === undefined || x === null) return true
    if (typeof x === 'number') return false
    if (typeof x === 'string') return false
    if (typeof x === 'boolean') return false
    if (Array.isArray(x) && x.length) return false
    if (x instanceof Object && Object.keys(x).length) return false
    return true
  }

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
  }

  _remove_item_copies (item) {
    const itemKey = item.dataset.epoxyItem
    this.querySelectorAll(`[data-epoxy-copy="${itemKey}"]`).forEach(item =>
      item.parentElement.removeChild(item)
    )
  }

  _eventUpdated (key, value) {
    return new CustomEvent('epoxy-updated', {
      bubbles: true,
      detail: { output: this.id, key, value, outputType: 'html' }
    })
  }

  _eventErrored (key, data) {
    return new CustomEvent('epoxy-errored', {
      bubbles: true,
      detail: {
        output: this.id,
        key,
        message: data,
        outputType: 'html'
      }
    })
  }

  errorClasses = ['epoxy-item__error', 'hint--top-right', 'hint--error']

  _clearError (item) {
    this.errorClasses.forEach(c => item.classList.remove(c))
    item.removeAttribute('aria-label')
  }

  _showError (item, data) {
    const itemKey = item.dataset.epoxyItem
    this.errorClasses.forEach(c => item.classList.add(c))
    this._remove_item_copies(item)

    updateContents(item, item.dataset.epoxyPlaceholder || '')
    item.style.removeProperty('display')
    item.setAttribute('aria-label', data)
    item.dispatchEvent(this._eventErrored(itemKey, data))
  }

  updateEpoxyValues (data) {
    const items = this.querySelectorAll('[data-epoxy-item]')

    items.forEach(item => {
      item.classList.remove('epoxy-item__placeholder')
      const itemName = item.dataset.epoxyItem
      const asHTML = item.dataset.epoxyAsHtml === 'true'

      const updateContents = (el, contents) => {
        asHTML ? (el.innerHTML = contents) : (el.textContent = contents)
        el.dispatchEvent(this._eventUpdated(itemName, contents))
        return el
      }

      let itemData = data[itemName]

      if (data.__errors__ && data.__errors__.includes(itemName)) {
        this._showError(item, itemData)
        return
      } else {
        this._clearError(item)
      }

      if (this._last && this._deepEqual(itemData, this._last[itemName])) {
        // don't do anything, the value hasn't changed
        return
      }

      this._remove_item_copies(item)

      if (this._is_empty(itemData)) {
        if (data.__partial) {
          // This is partial update, so the empty value is ignored.
          data[itemName] = this._last[itemName]
          return
        }
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
    this.classList.remove('epoxy-init')
  }
}

customElements.define('epoxy-html', EpoxyHTML)

if (window.Shiny) {
  const epoxyOutputBinding = new Shiny.OutputBinding()

  $.extend(epoxyOutputBinding, {
    find: function (scope) {
      return $(scope).find('epoxy-html')
    },
    renderValue: function (el, data) {
      el.updateEpoxyValues(data)
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
}
