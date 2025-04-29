@model Person

<div class="card">
    <div class="card-header">
        <h3>Person Details</h3>
    </div>
    <div class="card-body">
        <form asp-action="Edit">
            
            <div class="form-group">
                <label asp-for="Id" class="control-label"></label>
                
                <input asp-for="Id" class="form-control" />
                
                <span asp-validation-for="Id" class="text-danger"></span>
            </div>
            
            <div class="form-group">
                <label asp-for="TitleName" class="control-label"></label>
                
                <input asp-for="TitleName" class="form-control" />
                
                <span asp-validation-for="TitleName" class="text-danger"></span>
            </div>
            
            <div class="form-group">
                <label asp-for="IsActive" class="control-label"></label>
                
                <input asp-for="IsActive" class="form-check-input" />
                
                <span asp-validation-for="IsActive" class="text-danger"></span>
            </div>
            
            <div class="form-group mt-3">
                <input type="submit" value="Save" class="btn btn-primary" />
                <a asp-action="Index" class="btn btn-secondary">Back to List</a>
            </div>
        </form>
    </div>
</div>

@section Scripts {
    @{await Html.RenderPartialAsync("_ValidationScriptsPartial");}
} 