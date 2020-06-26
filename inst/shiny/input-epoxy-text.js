// Ref: https://shiny.rstudio.com/articles/building-inputs.html
// Ref: https://github.com/rstudio/shiny/blob/master/srcjs/input_binding.js

const epoxyInlineText = new Shiny.InputBinding();

$.extend(epoxyInlineText, {
  find: function(scope) {
    // Specify the selector that identifies your input. `scope` is a general
    // parent of your input elements. This function should return the nodes of
    // ALL of the inputs that are inside `scope`. These elements should all
    // have IDs that are used as the inputId on the server side.
    return scope.querySelectorAll('.epoxy-inline-text-input input[type="text"]');
  },
  getValue: function(el) {
    // For a particular input, this function is given the element containing
    // your input. In this function, find or construct the value that will be
    // returned to Shiny. The ID of `el` is used for the inputId.

    // e.g: return el.value
    return el.value;
  },
  setValue: function(el, value) {
    // This method is used for restoring the bookmarked state of your input
    // and allows you to set the input's state without triggering reactivity.
    // Basically, reverses .getValue()

    // e.g.; el.value = value
    el.value = value
    this._resizeElement(el)
  },
  receiveMessage: function(el, data) {
    // Given the input's container and data, update the input
    // and its elements to reflect the given data.
    // The messages are sent from R/Shiny via
    // R> session$sendInputMessage(inputId, data)
    this.setValue(el, data)

    // If you want the update to trigger reactivity, trigger a subscribed event
    $(el).trigger("change")
  },
  _measureWidth: function(el, text) {
    let ruler = document.createElement('span')
    ruler.innerHTML = text
    ruler.style.visibility = 'hidden'
    ruler.style.whiteSpace = 'nowrap'
    el.parentElement.appendChild(ruler)
    width = ruler.offsetWidth
    el.parentElement.removeChild(ruler)
    return(width)
  },
  _resizeElement: function(el) {
    el.style.width = this._measureWidth(el, el.value) + 'px'
  },
  subscribe: function(el, callback) {
    // Prepare input
    this._resizeElement(el)

    // Listen to events on your input element. The following block listens to
    // the change event, but you might want to listen to another event.
    // Repeat the block for each event type you want to subscribe to.
    el.addEventListener('keyup', () => {
      let w = this._measureWidth(el, el.value.replace(' ', '&nbsp;'))
      el.style.width = w + 20 + 'px'
    })

    el.addEventListener('blur', () => this._resizeElement(el))

    $(el).on("change.epoxyInlineText", function(e) {
      // Use callback() or callback(true).
      // If using callback(true) the rate policy applies,
      // for example if you need to throttle or debounce
      // the values being sent back to the server.
      callback();
    });
  },
  getRatePolicy: function() {
    return {
      policy: 'debounce', // 'debounce', 'throttle' or 'direct' (default)
      delay: 100 // milliseconds for debounce or throttle
    };
  },
  unsubscribe: function(el) {
    $(el).off(".epoxyInlineText");
  }
});

Shiny.inputBindings.register(epoxyInlineText, 'epoxy.epoxyInlineText');
