var epoxyOutputBinding = new Shiny.OutputBinding();
$.extend(epoxyOutputBinding, {
  find: function(scope) {
    return $(scope).find('.epoxy-html');
  },
  renderValue: function(el, data) {
    let items = el.querySelectorAll('[data-epoxy-item]');
    [...items].forEach(item => {
      item.classList.remove('epoxy-item__placeholder');
      item.innerHTML = data[item.dataset.epoxyItem]
    });
  }
});
Shiny.outputBindings.register(epoxyOutputBinding, 'shiny.epoxyHTML');
