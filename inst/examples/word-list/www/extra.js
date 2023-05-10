document.addEventListener('epoxy-update', (ev) => {
  ev.target.classList.add('animate', 'blur')
})

document
  .getElementById('word_list')
  .addEventListener('animationend', ev => ev.target.classList.remove('animate'))
