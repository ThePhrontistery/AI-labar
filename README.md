# AI-labar
AI-labar is a fast and convenient ways to get the opinion of people on various topics, implemented in Angular/Java

## Installation

### Tool Installation (Recommended)

- It is highly recommended to use Visual Studio Code for the frontend development and IntelliJ IDEA for the backend development.
- Ensure that you have Node.js installed, which you can download from [nodejs.org](https://nodejs.org/).
- Globally install Angular CLI by running `npm install -g @angular/cli`.
- Ensure you have Java 11 installed on your system. You can check the Java version by running `java -version` in the command line.
- Install Maven 3.5.4 or a compatible version. You can download it from the official Apache Maven website ([https://maven.apache.org/download.cgi](https://maven.apache.org/download.cgi)).
- Download and install Visual Studio Code from [Visual Studio Code](https://code.visualstudio.com/). (Recommended)
- Download and install IntelliJ IDEA from [IntelliJ IDEA](https://www.jetbrains.com/idea/download/). (Recommended)

### 1. Project Download

1. Clone or download this repository to your local development environment.
#### To clone:
  1. Open a terminal and navigate to the location where you want to clone the repository for your application.
  2. Execute the following command to clone the repository from your version control server (e.g., GitHub):
```
git clone <Repository_URL>
```
### 2. Configure the Database

1. Open the `Backend/ai-labar/src/main/resources/application.properties` file.
2. Configure the database connection properties. Here's an example for an embedded H2 database:
```properties
# postgres configuration
spring.datasource.url=jdbc:postgresql://localhost:5432/ai_labar
spring.datasource.username=postgres
spring.datasource.password=password
spring.datasource.driver-class-name=org.postgresql.Driver
```
For the creation of the database in Postgres, please review the file [Create Tables](/UML/create_tables_Ai-labar.sql)

### 4. Build and Run the Backend

#### With Command
1. Open a terminal and navigate to the root directory of your Spring Boot project.
2.  Execute the following command to build and run the backend:
   ```
mvn clean install -Dmaven.test.skip=true
mvn spring-boot:run -Dmaven.test.skip=true
 ```
The backend should now be up and running at http://localhost:8080.

#### With IntelliJ IDEA

1. Open IntelliJ IDEA and navigate to the backend project folder.
2. Import the project as a Maven project.
3. Ensure that the application server configuration is correctly set up (e.g., Tomcat).
4. Configure the database connection if needed in the `application.properties` file.
5. Run the Spring Boot application from IntelliJ IDEA.

### 5. Build and Run the Frontend

#### With Command
1. Open a terminal in the Angular project folder.
2. Run the following commands to install dependencies and build the frontend:
```
npm install
ng serve
```
The frontend will be accessible at http://localhost:4200.

#### With Visual Studio Code
1. Open Visual Studio Code and navigate to the frontend project folder.
2. Open an integrated terminal in Visual Studio Code.
3. Run `npm install` to install project dependencies.
4. Edit the API configuration in the code to point to the Spring Boot backend if necessary.
5. Run `ng serve` to start the Angular development server. The application will be available at `http://localhost:4200`.



## Usage
TODO

## Contributing

Pull requests are welcome. For major changes, please open an issue first
to discuss what you would like to change.

Please make sure to update tests as appropriate.

## Copyright and license

Copyright © 2023 The Phrontistery

Licensed under the MIT License 
[MIT](https://choosealicense.com/licenses/mit/)
