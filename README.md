# haskell-rolodex [![Stories in Ready](https://badge.waffle.io/lexi-lambda/haskell-rolodex.svg?label=ready&title=Ready)](http://waffle.io/lexi-lambda/haskell-rolodex)

This is a tiny Haskell web service that provides a REST API to manage contacts in a SQLite database. It is built using [servant][servant].

## Installing and running the server

This project uses [stack][stack] for build management. Clone this repository, then run `stack build` to install and compile the server.

The `SQLITE_FILENAME` environment variable must be set in order to run the server, and it will be used to determine where the SQLite database should be on the filesystem. If the file does not exist, it will be created.

```
$ stack build
$ env SQLITE_FILENAME=data.sqlite stack exec haskell-rolodex
```

## API

The server provides an interface for managing a single resource, *contacts*. Standard CRUD operations are supported, plus a full-text search for looking up contacts by their fields.

Each contact *must* contain at least valid `email` and `phoneNumber` fields, where `email` is a valid email address and `phoneNumber` is a valid U.S. phone number. In addition, each contact supports an arbitrary number of custom metadata fields that can be listed under the `details` key. Here is an example of a valid contact:

```json
{
  "email": "user@example.com",
  "phoneNumber": "(555) 555-5555",
  "details": {
    "firstName": "Jane",
    "lastName": "Smith"
  }
}
```

### Endpoints

#### `GET /contacts`

Returns all contacts in a list. Each contact contains an additional `id` field, which is a unique integral id used for referring to that contact.

#### `POST /contacts`

Creates a new contact. The body of the request must be a valid JSON representation of a contact.

#### `GET /contacts/:id`

Gets the contact with the specified id, where `id` is an integer to use for lookup.

#### `PUT /contacts/:id`

Updates the contact with the specified id, where `id` is an integer to use for lookup. The body must be a *complete* JSON representation of a contact—the contact will be completely overwritten with the new value. All fields not present in the new value will be removed.

#### `DELETE /contacts/:id`

Removes the contact with the specified id, where `id` is an integer to use for lookup.

#### `GET /contacts/search?query`

Performs a full-text, case-insensitive search for contacts given a textual `query` to look for. All fields of each contact are searched to see if they contain the query string. Only exact matches are considered, but the match may be partial. An array of all matching contacts is returned.

## Running the test suite

This project includes a small, incomplete test suite. To run it, just run `stack test`.

[servant]: https://github.com/haskell-servant/servant
[stack]: https://github.com/commercialhaskell/stack
