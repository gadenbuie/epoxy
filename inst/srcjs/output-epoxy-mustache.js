var epoxyMustacheOutputBinding = new Shiny.OutputBinding();
$.extend(epoxyMustacheOutputBinding, {
  find: function(scope) {
    return $(scope).find('.epoxy-mustache');
  },
  renderValue: function(el, data) {
    if (!el.epoxyTemplate) {
      // store template in DOM element and clean up visible markup
      el.epoxyTemplate = el.dataset.epoxyTemplate;
      el.removeAttribute('data-epoxy-template');
    }
    el.innerHTML = Mustache.render(el.epoxyTemplate, data);
  },
  renderError: function(el, err) {
    this.clearError(el);
    if (err.message !== '') {
      console.error('[epoxy] ' + err.message);
      el.classList.add('epoxy-error');
    }
    return;
  },
  clearError: function(el) {
    el.classList.remove('epoxy-error');
  }
});
Shiny.outputBindings.register(epoxyMustacheOutputBinding, 'shiny.epoxyMustache');
