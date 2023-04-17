/* globals Shiny,$ */

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

    if (typeof x !== 'object' || typeof y !== 'object' || x === null || y === null) {
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
    // remove copies of epoxyItem
    const elCopies = el.querySelectorAll('[data-epoxy-copy]')
    elCopies.forEach(e => e.parentElement.removeChild(e))

    const items = el.querySelectorAll('[data-epoxy-item]')
    items.forEach(item => {
      item.classList.remove('epoxy-item__placeholder')
      const itemName = item.dataset.epoxyItem

      let itemData = data[itemName]

      if (
        this._last &&
        this._deepEqual(itemData, this._last[itemName])
      ) {
        // don't do anything, the value hasn't changed
        return
      }

      if (this._is_empty(itemData)) {
        item.style.display = 'none'
        return
      } else {
        item.style.removeProperty('display')
      }

      if (itemData instanceof Array) {
        let lastItem = item
        item.innerHTML = itemData[0]
        const itemParent = item.parentElement
        itemData = itemData.slice(1)
        for (const itemDataThis of itemData) {
          const itemNew = item.cloneNode()
          itemNew.removeAttribute('data-epoxy-item')
          itemNew.dataset.epoxyCopy = itemName
          itemNew.innerHTML = itemDataThis
          itemParent.insertBefore(itemNew, lastItem.nextSibling)
          lastItem = itemNew
        }
      } else {
        item.innerHTML = itemData
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
