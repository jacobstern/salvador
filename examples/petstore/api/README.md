# Petstore

A loose port of the [Swagger Petstore
example](https://github.com/OAI/OpenAPI-Specification/blob/master/examples/v3.0/petstore.yaml)
to the Salvador spec format.

# Pets

Modify and access pets in the database.

## List Pets

List all pets.

### HTTP Request

`GET /pets`

### Query Parameters

| Parameter | Type    | Required | Description                            |
| --------- | ------- | -------- | -------------------------------------- |
| limit     | Natural | Optional | Maximum number of items to return.     |
| start     | Natural | Optional | Offset from the beginning of the list. |

### Response Status

200

### Response Content

List of [Pet](#pet)

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
## Create Pet

Create a pet.

### HTTP Request

`POST /pets`

### Request Body

| Field | Type    | Required | Description                            |
| ----- | ------- | -------- | -------------------------------------- |
| name  | Natural | Required | The name of the pet.                   |
| tag   | Natural | Optional | An optional tag to categorize the pet. |

### Response Status

201

### Response Content

No content.

## Get a Specific Pet

Access a single pet by its unique identifier.

### HTTP Request

`GET /pets/{petId}`

### URL Parameters

| Parameter | Type    | Description                          |
| --------- | ------- | ------------------------------------ |
| petId     | Natural | The identifier of a pet to retrieve. |

### Response Status

200

### Response Content

[Pet](#pet)

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
| name  | Natural | Required | The name of the pet.                   |
| tag   | Natural | Optional | An optional tag to categorize the pet. |

