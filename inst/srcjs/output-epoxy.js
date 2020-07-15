var epoxyOutputBinding = new Shiny.OutputBinding();
$.extend(epoxyOutputBinding, {
  find: function(scope) {
    return $(scope).find('.epoxy-html');
  },
  _is_empty: function(x) {
    if (x === undefined || x === null) return true;
    if (typeof x === 'number') return false;
    if (typeof x === 'string') return false;
    if (typeof x === 'boolean') return false;
    if (Array.isArray(x) && x.length) return false;
    if (x instanceof Object && Object.keys(x).length) return false;
    return true;
  },
  renderValue: function(el, data) {
    // remove copies of epoxyItem
    let elCopies = el.querySelectorAll('[data-epoxy-copy]')
    elCopies.forEach(e => e.parentElement.removeChild(e))

    let items = el.querySelectorAll('[data-epoxy-item]');
    items.forEach(item => {
      item.classList.remove('epoxy-item__placeholder');
      let itemName = item.dataset.epoxyItem;

      let itemData = data[itemName];
      if (this._is_empty(itemData)) {
        item.style.display = 'none';
        return;
      } else {
        item.style.removeProperty('display')
      }
      if (itemData instanceof Array) {
        let lastItem = item;
        item.innerHTML = itemData[0];
        itemParent = item.parentElement;
        itemData = itemData.slice(1);
        for (let itemDataThis of itemData) {
          const itemNew = item.cloneNode();
          itemNew.removeAttribute('data-epoxy-item');
          itemNew.dataset.epoxyCopy = itemName;
          itemNew.innerHTML = itemDataThis;
          itemParent.insertBefore(itemNew, lastItem.nextSibling);
          lastItem = itemNew;
        }
      } else {
        item.innerHTML = itemData;
      }
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
