# Petstore

A loose port of the [Swagger Petstore
example](https://github.com/OAI/OpenAPI-Specification/blob/master/examples/v3.0/petstore.yaml)
to the Salvador spec format.

# Pets

Modify and access pets in the database.

## GET /pets

List all pets.

### Query Parameters

| Parameter | Type    | Required | Description                            |
| --------- | ------- | -------- | -------------------------------------- |
| limit     | Natural | Optional | Maximum number of items to return.     |
| offset    | Natural | Optional | Offset from the beginning of the list. |

### Responses

| Status Code | Content             |
| ----------- | ------------------- |
| 200         | List of [Pet](#pet) |

### Example Response

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

### Request Body

| Field | Type | Required | Description                            |
| ----- | ---- | -------- | -------------------------------------- |
| name  | Text | Required | The name of the pet.                   |
| tag   | Text | Optional | An optional tag to categorize the pet. |

### Responses

| Status Code | Content    |
| ----------- | ---------- |
| 201         | No content |

## GET /pets/{petId}

Access a single pet by its unique identifier.

### URL Parameters

| Parameter | Type    | Description                          |
| --------- | ------- | ------------------------------------ |
| petId     | Natural | The identifier of a pet to retrieve. |

### Responses

| Status Code | Content     |
| ----------- | ----------- |
| 200         | [Pet](#pet) |

### Example Response

```json
{
    "id": 1,
    "name": "Fido",
    "tag": null
}
```
## Definitions

### Pet

| Field | Type    | Required | Description                            |
| ----- | ------- | -------- | -------------------------------------- |
| id    | Natural | Required | Unique identifier for the pet.         |
| name  | Text    | Required | The name of the pet.                   |
| tag   | Text    | Optional | An optional tag to categorize the pet. |

