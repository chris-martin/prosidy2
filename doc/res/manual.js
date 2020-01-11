class CalculateViewing {
    constructor() {
        this.main = document.querySelector('main');
        this.sections = document.querySelectorAll('main section');
        this.currentLinks = document.querySelectorAll('nav li.current a')
        this.update();
    }

    update() {
        if (!this.currentLinks) return;
        const currentTop = window.scrollY
        let viewing;
        for (const section of this.sections) {
            if (section.offsetTop > currentTop) break;
            viewing = section.id;
        }
        for (const link of this.currentLinks) {
            if (viewing === link.dataset.target) {
                link.classList.add('viewing');
            } else {
                link.classList.remove('viewing');
            }
        }
    }
}

const main = () => {
    const calc = new CalculateViewing;
    const update = () => calc.update();
    window.addEventListener('scroll', update);
    window.addEventListener('resize', update);
}

window.addEventListener('load', main);