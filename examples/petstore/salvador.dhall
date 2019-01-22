let C = ../../dhall/constructors.dhall

let T = ../../dhall/types.dhall

let paginationQueryParameters =
      [ { name =
            "limit"
        , type =
            C.ValueParameter { valueType = C.NaturalValue }
        , required =
            False
        , description =
            "Maximum number of items to return."
        }
      , { name =
            "start"
        , type =
            C.ValueParameter { valueType = C.NaturalValue }
        , required =
            False
        , description =
            "Offset from the beginning of the list."
        }
      ]

let petsSegment = C.LiteralPathSegment { segment = "pets" }

let spec
    : T.Spec
    = { title =
          "Petstore"
      , description =
          ''
          A loose port of the [Swagger Petstore
          example](https://github.com/OAI/OpenAPI-Specification/blob/master/examples/v3.0/petstore.yaml)
          to the Salvador spec format.
          ''
      , validation =
          { acceptOptional =
              C.AllowNullUndefinedMissing
          , returnOptional =
              C.AllowNull
          }
      , modules =
          [ { name =
                "Pets"
            , description =
                "Modify and access pets in the database."
            , paths =
                [ { location =
                      [ petsSegment ]
                  , endpoints =
                      [ { description =
                            "List all pets."
                        , request =
                            C.Get { parameters = paginationQueryParameters }
                        , response =
                            { statusCode =
                                200
                            , content =
                                C.JSONResponse
                                { type = C.ReferenceListJSON { name = "Pet" } }
                            }
                        }
                      ]
                  }
                ]
            , definitions =
                [ C.Record
                  { name =
                      "Pet"
                  , fields =
                      [ { name =
                            "id"
                        , required =
                            True
                        , description =
                            "Unique identifier for the pet."
                        , type =
                            C.ValueJSON { valueType = C.NaturalValue }
                        }
                      , { name =
                            "name"
                        , required =
                            True
                        , description =
                            "The name of the pet."
                        , type =
                            C.ValueJSON { valueType = C.TextValue }
                        }
                      , { name =
                            "tag"
                        , required =
                            False
                        , description =
                            "An optional tag to categorize the pet."
                        , type =
                            C.ValueJSON { valueType = C.TextValue }
                        }
                      ]
                  }
                ]
            }
          ]
      }

in  spec
