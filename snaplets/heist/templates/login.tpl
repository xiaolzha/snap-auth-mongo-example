<html>
  <head>
    <title>Snap web server</title>
    <link rel="stylesheet" type="text/css" href="/screen.css"/>
  </head>
  <body>
    <div id="content">
      <message/>
      <form action="/login" method="post">
        <fieldset name="login">
          <label>User name:</label> <input name="user" type="text"/><br/>
          <label>Password: &nbsp; </label> <input name="password" type="password"/><br/>
        </fieldset>
        <button type="submit" name="submit">Login</button> <br/>
      </form>
      <a href="/register">Register</a>
    </div>
  </body>
</html>