var epoxyOutputBinding = new Shiny.OutputBinding();
$.extend(epoxyOutputBinding, {
  find: function(scope) {
    return $(scope).find('.epoxy-html');
  },
  renderValue: function(el, data) {
    let items = el.querySelectorAll('[data-epoxy-item]');
    [...items].forEach(item => {
      item.classList.remove('epoxy-item__placeholder');
      item.innerHTML = data[item.dataset.epoxyItem];
    });
    el.classList.remove('epoxy-init');
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
Shiny.outputBindings.register(epoxyOutputBinding, 'shiny.epoxyHTML');
