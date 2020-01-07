const toggleElement = document.createElement('div');

const main = () => {
  document
    .querySelector('body')
    .appendChild(toggleElement)
};

window.addEventListener('load', main);
