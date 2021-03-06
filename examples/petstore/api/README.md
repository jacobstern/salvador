# Overview

This is a loose port of the [OpenAPI Petstore
example](https://github.com/OAI/OpenAPI-Specification/blob/master/examples/v3.0/petstore.yaml)
to the Salvador spec format.

It demonstrates some of the nice features of Dhall and Salvador
for specifying and documenting APIs.

# Pets

Modify and access pets in the database.

## GET /pets

List all pets.

**Query Parameters**

| Parameter | Type    | Required | Description                            |
| --------- | ------- | -------- | -------------------------------------- |
| limit     | Natural | Optional | Maximum number of items to return.     |
| offset    | Natural | Optional | Offset from the beginning of the list. |

**Response Status Code**

200

**Response Content**

List of [Pet](#pet)

**Example Response Content**

```json
[
    {
        "id": 1,
        "name": "Fido",
        "tag": null
    },
    {
        "id": 2,
        "name": "Jefferson",
        "tag": "dog"
    }
]
```

## POST /pets

Create a pet.

**Request Body**

| Field | Type | Required | Description                            |
| ----- | ---- | -------- | -------------------------------------- |
| name  | Text | Yes      | The name of the pet.                   |
| tag   | Text | Optional | An optional tag to categorize the pet. |

**Response Status Code**

201

**Response Content**

No content

## GET /pets/{petId}

Access a single pet by its unique identifier.

**URL Parameters**

| Parameter | Type    | Description                          |
| --------- | ------- | ------------------------------------ |
| petId     | Natural | The identifier of a pet to retrieve. |

**Response Status Code**

200

**Response Content**

[Pet](#pet)

**Example Response Content**

```json
{
    "id": 1,
    "name": "Fido",
    "tag": null
}
```

## Definitions

### Pet

A pet data structure returned from the API.

| Field | Type    | Required | Description                            |
| ----- | ------- | -------- | -------------------------------------- |
| id    | Natural | Yes      | Unique identifier for the pet.         |
| name  | Text    | Yes      | The name of the pet.                   |
| tag   | Text    | Optional | An optional tag to categorize the pet. |

