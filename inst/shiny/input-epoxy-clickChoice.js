// Ref: https://shiny.rstudio.com/articles/building-inputs.html
// Ref: https://github.com/rstudio/shiny/blob/master/srcjs/input_binding.js

const epoxyInlineClickChoice = new Shiny.InputBinding();

$.extend(epoxyInlineClickChoice, {
  find: function(scope) {
    // Specify the selector that identifies your input. `scope` is a general
    // parent of your input elements. This function should return the nodes of
    // ALL of the inputs that are inside `scope`. These elements should all
    // have IDs that are used as the inputId on the server side.
    return scope.querySelectorAll('.epoxy-inline-clickChoice-input');
  },
  getValue: function(el) {
    // For a particular input, this function is given the element containing
    // your input. In this function, find or construct the value that will be
    // returned to Shiny. The ID of `el` is used for the inputId.

    // e.g: return el.value
    return el.textContent;
  },
  setValue: function(el, value) {
    // This method is used for restoring the bookmarked state of your input
    // and allows you to set the input's state without triggering reactivity.
    // Basically, reverses .getValue()

    // e.g.; el.value = value
    el.textContent = value;
  },
  receiveMessage: function(el, data) {
    // Given the input's container and data, update the input
    // and its elements to reflect the given data.
    // The messages are sent from R/Shiny via
    // R> session$sendInputMessage(inputId, data)
    if (data.value) {
      this.setValue(el, data.value);
    }

    if (data.choices && Array.isArray(data.choices) && data.choices.length) {
      el.dataset.choices = JSON.stringify(data.choices);
    }

    // If you want the update to trigger reactivity, trigger a subscribed event
    $(el).trigger("change");
  },
  subscribe: function(el, callback) {
    function nextChoice() {
      let choices = JSON.parse(el.dataset.choices);
      let sel = el.textContent;
      let idx = choices.findIndex((i) => i === sel);
      if (++idx >= choices.length) idx = 0;
      el.textContent = choices[idx];
      $(el).trigger("change");
    }

    // Listen to events on your input element. The following block listens to
    // the change event, but you might want to listen to another event.
    // Repeat the block for each event type you want to subscribe to.
    el.addEventListener('click', nextChoice);

    el.addEventListener('keydown', function(ev) {
      if (ev.code === 'Enter' ||  ev.code === "Space") nextChoice();
    });

    $(el).on("change.epoxyInlineClickChoice", function(e) {
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
    $(el).off(".epoxyInlineClickChoice");
  }
});

Shiny.inputBindings.register(epoxyInlineClickChoice, 'epoxy.epoxyInlineClickChoice');
