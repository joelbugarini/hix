# Real-World Use Case: Full Stack Model Automation

Hix was designed to solve a very common problem in modern development: **reducing boilerplate when creating new screens or entities across a full stack application**.

---

## The Problem

When you add a new feature like a `Customer` or `Product`, you often have to:

- Create a backend model class
- Write a database table or ORM definition
- Build a REST API handler
- Create an HTML form
- Add JS for validation or interactivity
- Define styles, test files, etc.

That's **5–10 files per model**, most of which follow a repeatable structure.

---

## The Hix Solution

Hix lets you define **templates for each type of file** — HTML, Python, TypeScript, SQL, etc. Then, with a single command, it can generate all those files for a given model.

### Example:

Given:
- `Customer.json` (model)
- Templates:
  - `customer.model.py.hix`
  - `customer.form.html.hix`
  - `customer.api.js.hix`
  - `customer.sql.hix`

You can run:
```bash
hix customer.model.py.hix Customer.json > src/models/customer.py
hix customer.form.html.hix Customer.json > frontend/forms/customer.html
hix customer.api.js.hix Customer.json > api/customer.js
hix customer.sql.hix Customer.json > migrations/create_customer.sql
```

Or automate it with a script.

---

## Bash Script Example

```bash
#!/bin/bash
MODEL=$1

hix templates/model.py.hix models/$MODEL.json > output/$MODEL.py
hix templates/form.html.hix models/$MODEL.json > output/$MODEL.html
hix templates/api.js.hix models/$MODEL.json > output/$MODEL.js
hix templates/schema.sql.hix models/$MODEL.json > output/$MODEL.sql
```

Usage:
```bash
./generate.sh Customer
```

---

## PowerShell Example

```powershell
param([string]$Model)

hix templates\model.py.hix models\$Model.json > output\$Model.py
hix templates\form.html.hix models\$Model.json > output\$Model.html
hix templates\api.js.hix models\$Model.json > output\$Model.js
hix templates\schema.sql.hix models\$Model.json > output\$Model.sql
```

Usage:
```powershell
.\generate.ps1 -Model Customer
```

---

## Benefits

- 🔁 **Consistent code structure** across files and teams
- 🧠 **Fewer manual errors** when creating boilerplate
- ⚡ **Fast prototyping** of new modules or features
- 🧩 **Decouples templates from data** so you can focus on the logic

---

With Hix, you design your code scaffolding once — and reuse it forever.

