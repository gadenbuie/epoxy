$(document).on('click', '[data-epoxy-input-click]', function(ev) {
  let el = ev.target;
  let inputName = el.dataset.epoxyItem;
  let elId = el.closest('.epoxy-html[id]').id;
  let value = +el.dataset.epoxyClickedValue || 0;
  value += 1;
  el.dataset.epoxyClickedValue = value;
  Shiny.setInputValue(`${elId}_${inputName}_clicked`, value);
});

document
  .querySelectorAll('[data-epoxy-input-click]')
  .forEach(el => {
    let name = el.dataset.epoxyItem;
    let inputId = el.closest('.epoxy-html').id;
    value = +el.dataset.epoxyClickedValue || 0;
    Shiny.setInputValue(`${inputId}_${name}_clicked`, 0);
  });
