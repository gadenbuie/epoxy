/* globals Shiny,$,Mustache,CustomEvent */

const epoxyMustacheOutputBinding = new Shiny.OutputBinding()

$.extend(epoxyMustacheOutputBinding, {
  find: function (scope) {
    return $(scope).find('.epoxy-mustache')
  },
  renderValue: function (el, data) {
    if (!el.epoxyTemplate) {
      // store template in DOM element and clean up visible markup
      el.epoxyTemplate = el.dataset.epoxyTemplate
      el.removeAttribute('data-epoxy-template')
    }

    const errors = data.__errors__
    if (errors && errors.length > 0) {
      errors.forEach(key => {
        console.error(`[epoxy] [${el.id}]: ${data[key]}`)
        el.dispatchEvent(
          new CustomEvent('epoxy-error', {
            bubbles: true,
            detail: {
              output: el.id,
              key,
              message: data[key],
              outputType: 'mustache'
            }
          })
        )
        data[key] = ''
      })
    }

    el.innerHTML = Mustache.render(el.epoxyTemplate, data)
    el.dispatchEvent(
      new CustomEvent('epoxy-update', {
        bubbles: true,
        detail: { output: el.id, data, outputType: 'mustache' }
      })
    )
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

Shiny.outputBindings.register(
  epoxyMustacheOutputBinding,
  'shiny.ui_epoxy_mustache'
)
