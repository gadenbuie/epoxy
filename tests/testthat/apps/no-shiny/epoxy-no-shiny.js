/* global EpoxyHTML */
function sendUpdatesToEpoxy (id) {
  return (event, value) => {
    if (typeof value === 'undefined') {
      value = { [event.target.id]: event.target.value }
    }
    if (typeof value === 'function') {
      value = value(event)
    }

    const data = { [id]: value }
    EpoxyHTML.update(data, true)
  }
}

function initApp () {
  const initData = {
    hello: {
      first: document.getElementById('first').value,
      last: document.getElementById('last').value
    }
  }
  EpoxyHTML.update(initData)
  ;['first', 'last'].forEach(inputId => {
    document
      .getElementById(inputId)
      .addEventListener('input', sendUpdatesToEpoxy('hello'))
  })
}

initApp()
