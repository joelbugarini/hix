# Hix: The Missing Link Between AI and Human-Driven Software Design

*Why Hix is more than just a code generator — it's the foundation for a new kind of human-AI collaboration.*

---

AI has come a long way in helping developers write code. It can generate functions, explain errors, suggest changes, and even write small applications. But when it comes to building real-world software — with consistent architecture, opinionated structure, and long-term maintainability — AI still needs guidance.

That guidance is human intuition, design, and intention.

**Hix is the layer that bridges this gap.**

---

## The Problem: AI Is Great at Code, But Not at Structure

You can ask an AI to build a `Customer` model, and it will likely give you a class. Maybe even a basic form or a handler. But it won’t:

- Place the files where your team expects them
- Follow your naming conventions
- Generate tests in the right style
- Match your framework, architecture, or language patterns

That’s where Hix shines.

---

## The Solution: Humans Define the Architecture, AI Fills the Gaps

With Hix, humans define the **templates** — a set of reusable, structured patterns that represent how your team wants code to be written.

Then, AI can generate or receive a **model** (like a `User`, `Order`, or `Product`) and use those templates to generate all the code you need:

```bash
hix model.py.hix Product.json > backend/models/product.py
hix form.html.hix Product.json > frontend/forms/product.html
hix api.ts.hix Product.json > frontend/api/product.ts
```

One model → multiple consistent files across your stack.

---

## Real-World Collaboration Loop

1. **Developer** defines templates once using Hix
2. **AI** creates or receives model definitions from a prompt or spec
3. **Hix** generates consistent code across multiple layers
4. **Developer** reviews, adjusts, and commits — fast, confidently

It’s the best of both worlds:
- AI brings speed and iteration
- Humans bring standards and strategy

---

## Example: Describe a Model to AI

> _“Create a `SubscriptionPlan` model with `Id`, `Name`, `Price`, and `IsRecurring`. Use existing templates.”_

AI writes `SubscriptionPlan.json`, then:

```bash
hix backend.hix SubscriptionPlan.json > models/subscription_plan.py
hix frontend-form.hix SubscriptionPlan.json > templates/subscription_form.html
```

All files follow the structure your team expects — ready to ship or iterate.

---

## Why This Matters

- 🧠 **Human intent** stays encoded in templates
- ⚡ **AI accelerates** model-driven generation
- 🧱 **Architecture becomes reusable**, reliable, and team-aligned
- 🔄 **Collaboration scales**, not chaos

> Hix doesn’t compete with AI — it empowers it.
> It brings the design layer AI has been missing.

---

## Call to Action

Try it in your own stack. Build templates once. Feed in models with AI. And let Hix do the rest.

👉 [Download Hix](https://github.com/joelbugarini/hix/releases/latest)  
📖 [Read the Docs](https://joelbugarini.github.io/hix/)  
⭐️ [Star it on GitHub](https://github.com/joelbugarini/hix)

_“Build once. Template forever. Let Hix do the repetition.”_

