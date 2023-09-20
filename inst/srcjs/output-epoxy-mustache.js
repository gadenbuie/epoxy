/* globals Shiny,$,Mustache,CustomEvent */
class EpoxyMustache extends window.HTMLElement {
  static is_set_global_event_listener = false

  constructor () {
    super()

    if (EpoxyMustache.is_set_global_event_listener) return
    window.addEventListener('epoxy-message.mustache', ev => {
      // {example: {thing: "dolphin", color: "blue", height: 5}}
      EpoxyMustache.update_all(ev.detail)
    })
    EpoxyMustache.is_set_global_event_listener = true
  }

  static update_all (data) {
    // { [id]: template_data }
    for (const [key, value] of Object.entries(data)) {
      const el = document.getElementById(key)
      if (!el) {
        console.warn(
          `[epoxy-mustache] No element with id "${key}"`, { [key]: value }
        )
        continue
      }
      el.update(value)
    }
  }

  connectedCallback () {
    // store template in DOM element and clean up visible markup
    this.mustache_template = this.dataset.epoxyTemplate
    this.removeAttribute('data-epoxy-template')
  }

  _emit_errors (data) {
    const errors = data.__errors__
    if (!errors) return
    if (errors.length === 0) return

    errors.forEach(key => {
      console.error(`[epoxy-mustache] [${this.id}]: ${data[key]}`)
      this.dispatchEvent(
        new CustomEvent('epoxy-errored', {
          bubbles: true,
          detail: {
            output: this.id,
            key,
            message: data[key],
            outputType: 'mustache'
          }
        })
      )
      data[key] = ''
    })
  }

  _emit_updated (data) {
    this.dispatchEvent(
      new CustomEvent('epoxy-updated', {
        bubbles: true,
        detail: { output: this.id, data, outputType: 'mustache' }
      })
    )
  }

  update (data) {
    this._emit_errors(data)

    this.innerHTML = Mustache.render(this.mustache_template, data)
    this._emit_updated(data)
  }
}

window.customElements.define('epoxy-mustache', EpoxyMustache)

if (window.Shiny) {
  const epoxyMustacheOutputBinding = new Shiny.OutputBinding()

  $.extend(epoxyMustacheOutputBinding, {
    find: function (scope) {
      return $(scope).find('.epoxy-mustache')
    },
    renderValue: function (el, data) {
      el.update(data)
    },
    renderError: function (el, err) {
      this.clearError(el)
      if (err.message !== '') {
        console.error(`[epoxy-mustache] [${el.id}] ${err.message}`)
        el.classList.add('epoxy-error')
      }
    },
    clearError: function (el) {
      el.classList.remove('epoxy-error')
    }
  })

  Shiny.outputBindings.register(
    epoxyMustacheOutputBinding,
    'shiny.ui_epoxy_mustache'
  )
}
