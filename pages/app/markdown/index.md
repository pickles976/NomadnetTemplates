### Installation  

To get Macron running on your system:  

**Clone the repository**
   - git clone https://github.com/pickles976/Macro.git
   - cd Macro

**Install Chicken Scheme**
   - On Debian/Ubuntu: `sudo apt-get install chicken-bin`
   - On Arch: `sudo pacman -S chicken`
   - On macOS: `brew install chicken`

**Install required Chicken Scheme packages**
   - sudo chicken-install sql-de-lite srfi-1 srfi-13 srfi-19

**Deploy to Nomadnet**
    - # Copy pages to your Nomadnet storage directory
    - cp -r pages/* ~/.nomadnetwork/storage/pages/

    - # Make the main page executable
    - chmod +x ~/.nomadnetwork/storage/pages/index.mu

**Generate the database tables**
    - cd ~/.nomadnetwork/storage/pages
    - csi -s framework/manage.scm --generate

Your Macron site is now live on your Nomadnet node! Access it through the Nomadnet interface.

### Quick Reference

Here is some documentation to get you started:

**[Chicken Scheme Basics](./subpages/chicken-scheme-basics.mu)** - Learn the fundamentals of Scheme programming  

**[Micron DSL](./subpages/micron-dsl.mu)** - Generate micron with scheme  

**[Markdown Converter](./subpages/markdown-converter.mu)** - Write content efficiently with markdown  

**[ORM Guide](./subpages/orm.mu)** - Build data-driven applications with our simple ORM  

---

If this section of the page looks weird, that's because it was translated from markdown into micron when the page loaded! Load markdown directly into your nomadnet page!

---


