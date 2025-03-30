# Advanced Examples

This page includes more advanced usage scenarios of Hix templates, showing how you can structure templates and models for more realistic code generation tasks.

---

## 1. Generating a DTO with Attributes

### model.json
```json
{
  "className": "ProductDto",
  "properties": [
    { "name": "Id", "type": "int" },
    { "name": "Name", "type": "string" },
    { "name": "IsAvailable", "type": "bool" },
    { "name": "CreatedDate", "type": "DateTime" }
  ]
}
```

### template.hix
```hix
public class [[model.className]] {
[[prop]]
  [JsonProperty("[[snake_case prop.name]]")]
  [[if prop.type=bool]]
    public bool [[prop.name]] { get; set; }
  [[else]]
    public [[prop.type]] [[prop.name]] { get; set; }
  [[/if]]
[[/prop]]
}
```

### Output
```csharp
public class ProductDto {
  [JsonProperty("id")]
  public int Id { get; set; }

  [JsonProperty("name")]
  public string Name { get; set; }

  [JsonProperty("is_available")]
  public bool IsAvailable { get; set; }

  [JsonProperty("created_date")]
  public DateTime CreatedDate { get; set; }
}
```

---

## 2. Filtering by Property Type

### template.hix
```hix
// Booleans only
[[prop type=bool]]
  public bool [[prop.name]]Flag;
[[/prop]]
```

This will render only the properties in the model that have type `bool`.

---

## 3. Excluding a Property

You can use the `[[prop ignore=...]]` syntax to skip rendering specific fields, such as identifiers or audit fields.

### model.json
```json
{
  "className": "Customer",
  "properties": [
    { "name": "Id", "type": "Guid" },
    { "name": "FullName", "type": "string" },
    { "name": "Email", "type": "string" }
  ]
}
```

### template.hix
```hix
[[prop ignore=Id]]
  public [[prop.type]] [[prop.name]];
[[/prop]]
```

### Output
```csharp
public string FullName;
public string Email;
```

This renders all properties except `Id`. This is useful when you want to skip certain fields during generation (e.g., primary keys or audit fields).

---

You can combine all these patterns to build flexible templates for generating DTOs, API contracts, form models, or any other structured output.

