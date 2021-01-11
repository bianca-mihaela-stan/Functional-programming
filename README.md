# Examen DAW
### Controller
- redenumirea actiunilor:
    ````C++
        [ActionName("about")]
        public ActionResult AboutPage()
        {
            return View();
        }
        //----> acum pagina se acceseaza prin /Pages/about
    ````
- `[NonAction]` - cand vrem ca metoda publica sa nu fie accesata prin intermediul unei rute
- GET, POST - crearea unei resurse, PUT- modificare, DELETE, PATCH, HEAD, OPTIONS
- PUT, PATCH si DELETE nu pot fi folosite ca method in View
    - ca sa trimit formular de edit din View:
    ````C++
    <form action = "/Students/Edit/{id}" method="post">
        @Html.HttpMethodOverride(HttpVerbs.Put)
        . . .
    <form />
    ````
- BINDING
    - `public ActionResult New([Bind(Include = "Name, Email")] Student student)` face bind doar pe Name si Email
    - `public ActionResult New([Bind(Exclude = "CNP")] Student student)` nu face bind la CNP
- model controller NEW
    ````C++
    [HttpPost]
    public ActionResult New(Student student)
    {
        try
        {
            if (ModelState.IsValid)
            {
                db.Students.Add(student);
                db.SaveChanges();
                return RedirectToAction("Index")
            }
            else
                return View(student)
        }
        catch (Exception e)
        {
            return View();
        }
    }
    ````
- model controller EDIT
    ````
    [HttpPut]
    public ActionResult Edit(int id, Student requestStudent)
    {
        try
        {
            if (ModelState.IsValid)
            {
                requestStudent student = db.Students.Find(id);
                if (TryUpdateModel(student))
                {
                    student.Name = requestStudent.Name;
                    student.Email = requestStudent.Email;
                    student.CNP = requestStudent.CNP;
                    db.SaveChanges();
                }
                return RedirectToAction("Index");
            }
            else
            {
                return View(requestStudent);
            }
        }
        catch (Exception e)
        {
            return View(requestStudent);
        }
    }
    ````
- model controller DELETE
    ````C++
    [HttpDelete]
    public ActionResult Delete(int id)
    {
        Student student = db.Students.Find(id);
        db.Students.Remove(student);
        db.SaveChanges();
        return RedirectToAction("Index");
    }
    ````
- model adaugare comentariu la o postare
    ````C++
    [HttpPost]
    public ActionResult AddMark(Mark mark)
    {
        try
        {
            db.Marks.Add(mark);
            db.SaveChanges();
            return Redirect("/Students/Show/" + mark.StudentId);
        }
        catch (Exception e)
        {
            ViewBag.StudentId = mark.StudentId;
            return View();
        }
    }
    ````
- cu View-ul asociat:
    ````HTML
    <form method = "post" action="/Students/AddMark">
    <input type = "hidden" name="StudentId" value="@ViewBag.StudentId" />
    <br />
    <label>Nota:</label>
    <br />
    <input type = "text" name="Value" />
    <button type = "submit" > Adauga nota</button>
    </form>
    ````
- adaugarea migratiilor
    - se adauga modelele
    - se adauga proprietatile modelelor
    - se adauga contextul in AppContext
        ````C++
        public class AppContext(): base ("DBConnectionString") {}
            public DbSet<Student> Students { get; set; }
        ````
    - se adauga Entity Framework
    - se adauga baza de date in App_Data
    - Package Manager Console `enable-migrations -EnableAutomaticMigrations:$true`
    - Migration -> Configuration
        ````
        AutomaticMigrationDataLossAllowed = true;
        ContextKey = "Curs4Partea2.AppContext";
        ````
    - in AppContext `Database.SetInitializer(new MigrateDatabaseToLatestVersion<AppContext,
          Curs4Partea2.Migrations.Configuration>("DBConnectionString"));`
    - in Package Console:
        - `Add-Migration Initial`
        - `Update-Database`
### Rutare
- rutare cu 2 parametri:
    ````C++
    routes.MapRoute(
    name: "Invite",
    url: "GroupMembers/Invite/{UserId}/{GrupId}",
    defaults: new
    {
        controller = "GroupMembers",
        action = "Invite",
        UserId = UrlParameter.Optional,
        GrupId = UrlParameter.Optional
    });
    ````
- rutare fara numele actiunii
    ````C++
    routes.MapRoute(
               name: "Users",
               url: "users/{user_id}",
               defaults: new
               {
                   controller = "Users",
                   action = "Show",
                   user_id = UrlParameter.Optional
               }
           );
    ````
- constrangeri
    ````C++
    routes.MapRoute(
       name: "Users",
       url: "users/{user_id}",
       defaults: new
       {
           controller = "Users",
           action = "Show",
           user_id = UrlParameter.Optional
       },
       constraints: new { user_id = @"\d+" } // user_id accepta doar cifre
    );
    ````
### View-uri
- formular pentru new doar cu HTML:
    ````HTML
    <form method="post" action="/Students/New">
    <label>Nume</label>
    <input type="text" name="Name" />
    <label>Adresa de e-mail</label>
    <input type="text" name="Email" />
    <label>CNP</label>
    <input type="text" name="CNP" />
    <button type="submit">Adauga student</button>
    </form>
    ````
- redirectionare doar cu HTML
    ````HTML
    <a href="/Students/Show/@student.StudentId"
       //prin formular ca mai sus
    //pentru delete
    <form method="post" action="/Students/Delete/@ViewBag.Student.StudentId">
        @Html.HttpMethodOverride(HttpVerbs.Delete)
        <button type="submit">Sterge studentul</button>
    </form>
    ````
- `@:` pentru excluderea textului
- `@model Curs6.Models.Student` ca sa fie inclus modelul
- `ViewBag` trimite cate un obiect, `ViewData` trimite un dictionar
- daca Actiunea1 redirectioneaza in Actiunea2 si trimite un mesaj prin `TempData` aceasta ve fi disponibila si in Actiunea2 (nu si in `ViewBag`)
- afisare mesaj din `TempData`:
    ````C++
    @if (ViewBag.Message != null)
    {
        <h3 class="alert alert-info" role="alert">@ViewBag.Message</h3>
    }
    ````
- helpere:
    - @Html.EditorFor(m => m.Name) <=> 
    - `@Html.Label("Name", "Nume Student")` <=> `<label for="Name">Nume Student</label>`
    - `@Html.TextBox("Name", null, new { @class = "form-control" })` <=> `<input class="form-control" id="Name" name="Name" type="text" value="" />`
    - `@Html.Editor("Email")` <=> `<input type="text" name="Email" value="@ViewBag.Student.Email" />`
- dropdown list:
    - cu HTML:
    ````
    <select name="CategoryId"> @foreach (var category in ViewBag.Categories)
    {
    <option value="@category.CategoryId">@category.CategoryName
    </option>
    }
    </select>
    ````
    - cu helper:
        - in Model se adauga IEnumerable<SelectListItem>
            ````
            @Html.DropDownListFor(m => m.CategoryId, new SelectList(Model.Categories, "Value", "Text"), "Selectati categoria", new { @class = "form-control" })
            ````
        - si se adauga metoda urmatoare in controller:
            ````
            [NonAction]
            public IEnumerable<SelectListItem> GetAllCategories()
            {
            // generam o lista goala
            var selectList = new List<SelectListItem>();
            // Extragem toate categoriile din baza de date
            var categories = from cat in db.Categories select cat;
            // iteram prin categorii
            foreach(var category in categories)
            {
            // Adaugam in lista elementele necesare pentru dropdown
            selectList.Add(new SelectListItem
            {
            Value = category.CategoryId.ToString(),
            Text = category.CategoryName.ToString()
            });
            }
            // returnam lista de categorii
            return selectList;
            }
            ````
- view-uri partiale: `@Html.Partial("StudentInfo", student);`
### Modele
- adaugarea Entity Framework in cadrul modelelor
    ````C++
    public class StudentDBContext : DbContext
    {
        public StudentDBContext() : base("DBConnectionString") { }
        //aici pun toate entitatile
        public DbSet<Student> Students { get; set; }
    }
    ````
- adaugarea bazei de date: App_Data -> Add -> New Item -> Sql Server Database
- adaugarea Connection String-ului in Web.config
    ````
    <connectionStrings>
        <add name = "DBConnectionString" providerName="System.Data.SqlClient" connectionString="Data Source=(LocalDB)\MSSQLLocalDB;AttachDbFilename='C:\...\App_Data\StudentDb.mdf';Integrated Security=True"/>
    </connectionStrings>
    ````
- validari
    - Required
        - `[Required(ErrorMessage = "Campul e-mail este obligatoriu")]`
    - StringLength
    - Range
    - RegularExpression
    - CreditCard
    - CustomValidation
    - EmailAdress
        - `[EmailAddress(ErrorMessage = "Adresa de e-mail nu este valida")]`
    - FileExtension
    - MaxLength
    - MinLength
    - Phone
    - DataType
- pentru afisarea validarilor in View se foloseste Html.ValidationMessageFor sau Html.ValidationMessage
    ````
    @Html.ValidationMessageFor(m => m.Name, "Numele este obligatoriu", new { @class = "text-danger " })
    ````
- pentru afisarea corecta a validarilor se adauga in Site.css liniile:
    ````
    .field-validation-valid {
    display: none;
    }
    .validation-summary-valid {
    display: none;
    }
    ````
- Validation summary `@Html.ValidationSummary(false, "", new { @class = "text-danger" })`
- relatie one to many: o postare are mai multe comentarii
    - comentariul are:
        ````C++
        public int PostId { get; set; }
        public virtual Post Post { get; set; }
        ````
    - postarea are:
        ````C++
        public virtual ICollection<Comment> Comments { get; set; }
        ````
- relatie many to many:
    - pentru a se adauga automat tabelul de legatura se adauga in Initial.cs metoda:
    ````C++
    public override void Up()
    {
        CreateTable(
            "dbo.Cities",
            c => new
                {
                    CityId = c.Int(nullable: false, identity: true),
                    CityName = c.String(),
                    CountryId = c.Int(nullable: false),
                    Country_CoutryId = c.Int(),
                })
            .PrimaryKey(t => t.CityId)
            .ForeignKey("dbo.Countries", t => t.Country_CoutryId)
            .Index(t => t.Country_CoutryId);
        
        CreateTable(
            "dbo.Countries",
            c => new
                {
                    CoutryId = c.Int(nullable: false, identity: true),
                    CountryName = c.String(),
                })
            .PrimaryKey(t => t.CoutryId);
        
        CreateTable(
            "dbo.Profiles",
            c => new
                {
                    ProfileId = c.Int(nullable: false, identity: true),
                    ProfileVisibility = c.Boolean(nullable: false),
                    Description = c.String(),
                    Gender = c.Boolean(nullable: false),
                    Birthday = c.DateTime(nullable: false),
                    CityId = c.Int(nullable: false),
                    CountryId = c.Int(nullable: false),
                    UserId = c.String(maxLength: 128),
                    Country_CoutryId = c.Int(),
                })
            .PrimaryKey(t => t.ProfileId)
            .ForeignKey("dbo.Cities", t => t.CityId, cascadeDelete: true)
            .ForeignKey("dbo.Countries", t => t.Country_CoutryId)
            .ForeignKey("dbo.AspNetUsers", t => t.UserId)
            .Index(t => t.CityId)
            .Index(t => t.UserId)
            .Index(t => t.Country_CoutryId);
        
    }
    ````
### LINQ
- exemplu:
    ````
    from stud in db.Students
    group stud by stud.Age into stud.Group
    where studGroup.Count() >= 1
    orderby studGroup.Key descending
    select studGroup;
    ````
- grupare studenti dupa varsta in View
    ````HTML
    @foreach (IGrouping <int, Test.Models.Student> group in ViewBag.Students)
    {
        foreach (var student in group)
        {
            ....
        }
    }
    ````
