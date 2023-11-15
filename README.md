# Thoth: A Domain-Specific Language for Multitier Web Development

> DISCLAIMER
> 
> This project is just a proof-of-concept, it was never intended to be used in production and it is unmaintained. USE ON YOUR OWN RISK.

Thoth is a multitier statically typed domain-specific language (DSL) that introduces a new way for developers to build web applications. By combining the functionality of the database, server, and client tiers into a single, mono-linguistic program, Thoth eliminates the need for excessive boilerplate code and simplifies the entire development process. It was created as part of my master's dissertation under <a href="https://www.birmingham.ac.uk/staff/profiles/computer-science/academic-staff/rahli-vincent.aspx">Dr. Vincent Rahli's</a> supervision.

- [Thoth: A Domain-Specific Language for Multitier Web Development](#thoth-a-domain-specific-language-for-multitier-web-development)
  - [The Need for Multitier Development](#the-need-for-multitier-development)
  - [Introducing Thoth the DSL](#introducing-thoth-the-dsl)
  - [Human-readable Typescript Code](#human-readable-typescript-code)
  - [Framework-Agnostic](#framework-agnostic)
  - [Leveraging the JavaScript Ecosystem and Customization](#leveraging-the-javascript-ecosystem-and-customization)
  - [Real-Time Applications by Default](#real-time-applications-by-default)
  - [Exploring Thoth's Features](#exploring-thoths-features)
  - [Get Started](#get-started)
    - [Prerequisites](#prerequisites)
    - [Building](#building)
    - [Compiling an Application](#compiling-an-application)
      - [Command Line Options](#command-line-options)

## The Need for Multitier Development

Traditional web application development often involves using different tools and programming languages for different parts of the application, leading to the well-known impedance mismatch problem. This separation creates complexities and inefficiencies that hinder developers' productivity. Thoth addresses this challenge by providing a holistic approach to web development.

## Introducing Thoth the DSL

Thoth offers a set of powerful declarations that abstract most of the low-level details of web development. Our DSL enables developers to define data models, queries, interactive user interfaces, and implement common web features like authentication and authorization seamlessly. What sets Thoth apart is its unique declarative approach, which hasn't been implemented by other multitier languages before.

## Human-readable Typescript Code

Thoth's design philosophy emphasizes generating human-readable code as part of the compilation process. This approach enables developers to comprehend the generated code easily, making it simpler to debug, modify, and enhance the application even after it has been compiled from Thoth DSL. By producing human-readable code, Thoth eliminates the barriers that may arise when relying solely on a specialized DSL. Developers can seamlessly transition between working with the DSL and directly modifying the compiled code, allowing for greater flexibility and customization.

## Framework-Agnostic

Thoth is designed to be framework-agnostic, offering developers the freedom to choose their preferred web application framework. While Thoth's default compilation targets a NodeJS server and a ReactJS client, the language itself is not bound to any specific framework. This flexibility allows developers to adapt Thoth to their existing tech stack or explore alternative frameworks that better suit their project requirements.

## Leveraging the JavaScript Ecosystem and Customization

Thoth seamlessly integrates with TypeScript, leveraging the vast JavaScript/TypeScript ecosystem and empowering developers with familiar tools and libraries. This compatibility ensures that developers can make the most of existing resources while benefiting from Thoth's declarative nature.

## Real-Time Applications by Default

Thoth takes web application development a step further by compiling applications into real-time experiences by default. Leveraging the power of server-sent events (SSE), Thoth enables developers to create dynamic and interactive applications that provide real-time updates to users. With Thoth, you can seamlessly incorporate real-time features into your web applications without the need for complex setup or additional libraries. By default, Thoth generates a NodeJS server and a ReactJS client that facilitate real-time communication between the server and the client.

## Exploring Thoth's Features

To know more about Thoth you can read the full dissertation found [here](https://github.com/abdllahdev/thoth-dissertation/blob/main/output/dissertation.pdf "A DSL for Multitier web development").

## Get Started

### Prerequisites

Before you can build and run this code, you will need to have the following installed on your system:

- OCaml <= ^4.0
- Dune <= ^3.6
- NodeJS <= ^16
- PostgreSQL <= ^14.7

### Building

To build the compiler, please follow the instructions below:

```
dune build bin/main.exe
```

### Compiling an Application

```
dune exec -- bin/main.exe PATH_TO_APP DATABASE_NAME
```

#### Command Line Options

The following command line options are available for this project:

- *output_dir*: The path to the directory that will contain the compiled code. Default value: “.out”

- *server_port*: The port used to run the server on. Default value = 4000

To use these options, simply include them when compiling the app from the command line, as follows:

```
dune exec -- bin/main.exe PATH_TO_APP DATABASE_NAME --output_dir=PATH_TO_DIR --server_port=PORT_NUMBER
```

The compiler creates two different directories, one for the server and another for the client. To run the compiled app, first, ensure that the PostgreSQL server is running on the machine. Next, navigate to the server directory and use the following command to create the database:

```
yarn prisma migrate dev
```

After that, you can run the server using the following command:

```
yarn dev
```

To run the client, navigate to the client directory and use the same command that was used to run the server.
