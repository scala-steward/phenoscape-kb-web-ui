# phenoscape-kb-web-ui
Redesigned Phenoscape KB web user interface — in development

## Development

In the `sbt` console, enter `~runDev` for to run the server and continually recompile to JavaScript. The site will be available at http://localhost:8080.

## Publishing to Docker

First update the version number in `build.sbt`, commit changes, and create a tag. Then, in the `sbt` console, enter `publishDocker`.

## Running in Docker

The application can be run via the image `phenoscape/phenoscape-kb-web-ui`. The `KB_ENDPOINT` environment variable should be set to provide the location of the Phenoscape KB web service API, e.g., `https://kb.phenoscape.org/api/v2-beta`.
