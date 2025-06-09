# Template Syntax

Hix templates support a rich tag syntax for rendering model-based structures. Tags are enclosed in double square brackets `[[ ]]`.

---

## Model Tags

- `[[model.className]]`: Injects the model's class name.

## Property Tags (inside `[[prop]]`)

- `[[prop.name]]`: Injects the name of the property
- `[[prop.type]]`: Injects the property type

---

## Control Flow

### Loops

```hix
[[prop]]
  [[prop.type]] [[prop.name]];
[[/prop]]
```

### Optional filters:

```hix
[[prop type=bool]]
[[prop ignore=Id]]
```

### Conditionals

```hix
[[if prop.type=bool]]
  // do something
[[else]]
  // do something else
[[/if]]
```

---

## Functions

Apply transformations to values:

```hix
[[upper prop.name]]     => MYFIELD
[[lower prop.name]]     => myfield
[[snake_case prop.name]] => my_field
[[kebab_case prop.name]] => my-field
[[lowerFirst prop.name]] => myField
```

---

## Comments

Hix doesn't support comments in template syntax directly yet, but you can use:
```hix
// [[prop.name]] is the name of the field
```

---

## Example Template

```hix
public class [[model.className]] {
[[prop]]
  [[if prop.type=bool]]
    public bool [[prop.name]];
  [[else]]
    public [[prop.type]] [[prop.name]];
  [[/if]]
[[/prop]]
}
```