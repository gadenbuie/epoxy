var epoxyMustacheOutputBinding = new Shiny.OutputBinding();
$.extend(epoxyMustacheOutputBinding, {
  find: function(scope) {
    return $(scope).find('.epoxy-mustache[data-epoxy-template]');
  },
  renderValue: function(el, data) {
    var template = el.dataset.epoxyTemplate;
    el.innerHTML = Mustache.render(template, data);
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
