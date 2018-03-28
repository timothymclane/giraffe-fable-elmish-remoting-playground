open System
open System.IO
open System.Threading.Tasks
open Microsoft.AspNetCore
open Microsoft.AspNetCore.Builder
open Microsoft.AspNetCore.Hosting
open Microsoft.AspNetCore.Authentication
open Microsoft.Extensions.DependencyInjection

open Giraffe

open Fable.Remoting.Giraffe

open Shared
open Microsoft.AspNetCore.Authentication.JwtBearer
open Microsoft.IdentityModel.Tokens
open System.Text
open System.IdentityModel.Tokens.Jwt
open System.Security.Claims
open Microsoft.Extensions.Logging

let mustBeLoggedIn = requiresAuthentication (challenge JwtBearerDefaults.AuthenticationScheme)

let clientPath = Path.Combine("..","Client") |> Path.GetFullPath
let port = 8085us

let getInitCounter () : Task<Counter> = task { return 21 }

let login req =
  let claims = [Claim(ClaimTypes.Name, req.username)]
  let key = SymmetricSecurityKey(Encoding.UTF8.GetBytes("secretsecretsecret"));
  let creds = SigningCredentials(key, SecurityAlgorithms.HmacSha256);

  let jwt = JwtSecurityToken(
                "yourdomain.com",
                "yourdomain.com",
                claims,
                System.Nullable(DateTime.Now),
                System.Nullable(DateTime.Now.AddMinutes(30.0)),
                creds)
  let handler = new JwtSecurityTokenHandler()
  let token = handler.WriteToken jwt
  token.ToString()

let webApp : HttpHandler =
  let counterProcotol = 
    { 
      getInitCounter = getInitCounter >> Async.AwaitTask;
    }
  let loginApi = {    
      login = (fun request -> 
                task {
                   match request with
                   | {username = "admin"; password = "testpass"} -> 
                      let token = login request
                      return LoginResult.Success token
                   | _ -> return LoginResult.PasswordIncorrect
                }) >> Async.AwaitTask;
  }  
  // creates a HttpHandler for the given implementation
  choose [
    FableGiraffeAdapter.httpHandlerWithBuilderFor loginApi Route.builder
    mustBeLoggedIn >=> FableGiraffeAdapter.httpHandlerWithBuilderFor counterProcotol Route.builder
  ]

let configureLogging (loggerBuilder : ILoggingBuilder) =
    loggerBuilder.AddFilter(fun lvl -> lvl.Equals LogLevel.Error)
                 .AddConsole()
                 .AddDebug() |> ignore

let errorHandler (ex : Exception) (logger : ILogger) =
    logger.LogError(EventId(), ex, "An unhandled exception has occurred while executing the request.")
    clearResponse >=> setStatusCode 500 >=> text ex.Message

let configureApp  (app : IApplicationBuilder) =
  app
      .UseAuthentication()
      .UseGiraffeErrorHandler(errorHandler)
      .UseStaticFiles()
      .UseGiraffe webApp

let configureServices (services : IServiceCollection) =
    services.AddAuthentication(JwtBearerDefaults.AuthenticationScheme).AddJwtBearer(fun options ->
        options.TokenValidationParameters <- 
          TokenValidationParameters(
            ValidateActor = false,
            ValidateAudience = false,
            ValidateLifetime = false,
            ValidateIssuerSigningKey = false,
            IssuerSigningKey = SymmetricSecurityKey(Encoding.UTF8.GetBytes("secret")))
        options.SaveToken <- true
    ) |> ignore
    services.AddGiraffe() |> ignore
    

WebHost
  .CreateDefaultBuilder()
  .UseWebRoot(clientPath)
  .UseContentRoot(clientPath)
  .Configure(Action<IApplicationBuilder> configureApp)
  .ConfigureServices(configureServices)
  .ConfigureLogging(configureLogging)
  .UseUrls("http://0.0.0.0:" + port.ToString() + "/")
  .Build()
  .Run()