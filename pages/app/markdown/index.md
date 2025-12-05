## Get Started

### Installation

To get Macron running on your system:

1. **Clone the repository**
   ```bash
   git clone https://github.com/pickles976/Macro.git
   cd Macro
   ```

2. **Install Chicken Scheme**
   - On Debian/Ubuntu: `sudo apt-get install chicken-bin`
   - On Arch: `sudo pacman -S chicken`
   - On macOS: `brew install chicken`

3. **Install required Chicken Scheme packages**
   ```bash
   sudo chicken-install sql-de-lite srfi-1 srfi-13 srfi-19
   ```

4. **Deploy to Nomadnet**
   ```bash
   # Copy pages to your Nomadnet storage directory
   cp -r pages/* ~/.nomadnetwork/storage/pages/

   # Make the main page executable
   chmod +x ~/.nomadnetwork/storage/pages/index.mu
   ```

5. **Generate the database tables**
   ```bash
   cd ~/.nomadnetwork/storage/pages
   csi -s framework/manage.scm --generate
   ```

Your Macron site is now live on your Nomadnet node! Access it through the Nomadnet interface.

### Quick Reference

Here is some documentation to get you started:

**[Chicken Scheme Basics](./subpages/chicken_scheme_basics.mu)** - Learn the fundamentals of Scheme programming

**[Micron DSL](./subpages/micron_dsl.mu)** - Generate micron with scheme

**[Markdown Converter](./subpages/markdown_converter.mu)** - Write content efficiently with markdown

**[ORM Guide](./subpages/orm.mu)** - Build data-driven applications with our simple ORM

---

