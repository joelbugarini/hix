using Microsoft.AspNetCore.Mvc;
using System.Collections.Generic;
using System.Threading.Tasks;

public class UserController
{
    private readonly IUserService _service;

    public UserController(IUserService service)
    {
        _service = service;
    }

    public async Task<IActionResult> Get()
    {
        var items = await _service.GetAllAsync();
        return Ok(items);
    }

    public async Task<IActionResult> Get(int id)
    {
        var item = await _service.GetByIdAsync(id);
        if (item == null)
            return NotFound();
        return Ok(item);
    }

    public async Task<IActionResult> Create(User model)
    {
        var result = await _service.CreateAsync(model);
        return CreatedAtAction(nameof(Get), new { id = result.Id }, result);
    }

    public async Task<IActionResult> Update(int id, User model)
    {
        if (id != model.Id)
            return BadRequest();

        await _service.UpdateAsync(model);
        return NoContent();
    }

    public async Task<IActionResult> Delete(int id)
    {
        await _service.DeleteAsync(id);
        return NoContent();
    }
} 