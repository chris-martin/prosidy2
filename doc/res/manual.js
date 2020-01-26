class CalculateViewing {
    constructor() {
        this.main = document.querySelector('main');
        this.currentLinks = document.querySelectorAll('nav > ol > li.current a')
        this.parents = document.querySelectorAll('nav > ol > li, nav > ol > li > ol > li')
        this.sections = document.querySelectorAll('main > section, main > section > section')
        this.update();
        window.currentLinks = this.currentLinks
    }


    update() {
        if (!this.currentLinks) return;
        const currentTop = window.scrollY + window.innerHeight / 2;
        let viewing;
        for (const section of this.sections) {
            if (section.offsetTop > currentTop) break;
            viewing = section.id;
        }
        for (const link of this.currentLinks) {
            if (viewing === link.dataset.target) {
                for (const li of this.parents) {
                    if (li.contains(link)) {
                        li.classList.add('viewing-child');
                    } else {
                        li.classList.remove('viewing-child');
                    }
                }
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