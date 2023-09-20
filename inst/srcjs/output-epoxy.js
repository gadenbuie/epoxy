/* globals Shiny,$,CustomEvent */

class EpoxyHTML extends window.HTMLElement {
  static is_set_global_event_listener = false

  last = null

  constructor () {
    super()

    if (EpoxyHTML.is_set_global_event_listener) return

    window.addEventListener('epoxy-message.html', ev => {
      // {example: {thing: "dolphin", color: "blue", height: 5}}
      EpoxyHTML.update_all(ev.detail)
    })
    EpoxyHTML.is_set_global_event_listener = true
  }

  static update_all (data, partial = false) {
    // { [id]: { [itemKey]: value }}
    // { example: { thing: "dolphin", color: "blue", height: 5 }}
    if (partial) {
      for (const key of Object.keys(data)) {
        data[key].__partial = true
      }
    }

    for (const [key, value] of Object.entries(data)) {
      const el = document.getElementById(key)
      if (!el) {
        console.warn(
          `[epoxy-html] [${key}] No element found with id`, { [key]: value }
        )
        continue
      }
      el.update(value)
    }
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

  _deep_equal (x, y) {
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
      if (!keysY.includes(key) || !this._deep_equal(x[key], y[key])) {
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

  _event_updated (key, data) {
    return new CustomEvent('epoxy-updated', {
      bubbles: true,
      detail: { output: this.id, key, data, outputType: 'html' }
    })
  }

  _event_errored (key, data) {
    console.error(`[epoxy-html] [${this.id}]: ${data}`)

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

  error_classes = ['epoxy-item__error', 'hint--top-right', 'hint--error']

  _item_clear_error (item) {
    this.error_classes.forEach(c => item.classList.remove(c))
    item.removeAttribute('aria-label')
  }

  _item_show_error (item, data) {
    const itemKey = item.dataset.epoxyItem
    this.error_classes.forEach(c => item.classList.add(c))
    this._remove_item_copies(item)

    this._item_update_contents(item, item.dataset.epoxyPlaceholder || '')
    item.style.removeProperty('display')
    item.setAttribute('aria-label', data)
    item.dispatchEvent(this._event_errored(itemKey, data))
  }

  _item_update_contents (item, contents) {
    const asHTML = item.dataset.epoxyAsHtml === 'true'

    asHTML ? (item.innerHTML = contents) : (item.textContent = contents)
    return item
  }

  update (data) {
    const items = this.querySelectorAll('[data-epoxy-item]')

    items.forEach(item => {
      item.classList.remove('epoxy-item__placeholder')
      const itemKey = item.dataset.epoxyItem

      let itemData = data[itemKey]

      if (data.__errors__ && data.__errors__.includes(itemKey)) {
        this._item_show_error(item, itemData)
        return
      } else {
        this._item_clear_error(item)
      }

      if (this._last && this._deep_equal(itemData, this._last[itemKey])) {
        // don't do anything, the value hasn't changed
        return
      }

      this._remove_item_copies(item)

      if (this._is_empty(itemData)) {
        if (data.__partial) {
          // This is partial update, so the empty value is ignored.
          data[itemKey] = this._last[itemKey]
          return
        }
        item.style.display = 'none'
        return
      } else {
        item.style.removeProperty('display')
      }

      if (!(itemData instanceof Array)) {
        this._item_update_contents(item, itemData)
        item.dispatchEvent(this._event_updated(itemKey, itemData))
        return
      }

      // If an array, use the initial item as a pattern
      const itemEventUpdated = this._event_updated(itemKey, itemData)
      itemEventUpdated.detail.copies = []
      const itemParent = item.parentElement

      this._item_update_contents(item, itemData[0])
      itemData = itemData.slice(1)

      for (const itemDataThis of itemData) {
        const itemNew = item.cloneNode()
        itemNew.removeAttribute('data-epoxy-item')
        itemNew.dataset.epoxyCopy = itemKey
        itemParent.insertAdjacentElement('beforeend', itemNew)
        this._item_update_contents(itemNew, itemDataThis)
        itemEventUpdated.detail.copies.push(itemNew)
      }

      item.dispatchEvent(itemEventUpdated)
    })

    this._last = data
    this.classList.remove('epoxy-init')
  }
}

window.customElements.define('epoxy-html', EpoxyHTML)

if (window.Shiny) {
  const epoxyOutputBinding = new Shiny.OutputBinding()

  $.extend(epoxyOutputBinding, {
    find: function (scope) {
      return $(scope).find('epoxy-html')
    },
    renderValue: function (el, data) {
      el.update(data)
    },
    renderError: function (el, err) {
      this.clearError(el)
      if (err.message !== '') {
        console.error(`[epoxy-html] [${el.id}] ${err.message}`)
        el.classList.add('epoxy-error')
      }
    },
    clearError: function (el) {
      el.classList.remove('epoxy-error')
    }
  })

  Shiny.outputBindings.register(epoxyOutputBinding, 'shiny.ui_epoxy_html')
}
