let C = ../../../dhall/constructors.dhall

let T = ../../../dhall/types.dhall

let List/concat =
      https://raw.githubusercontent.com/dhall-lang/dhall-lang/0a7f596d03b3ea760a96a8e03935f4baa64274e1/Prelude/List/concat sha256:43ef75a328d312c7fed8fbaf25d945b244d1b96505e3f1c291567ecee972449c

let paginationQueryParameters =
      [ { name =
            "limit"
        , type =
            C.ValueParameter { type = C.NaturalValue }
        , required =
            False
        , description =
            "Maximum number of items to return."
        }
      , { name =
            "start"
        , type =
            C.ValueParameter { type = C.NaturalValue }
        , required =
            False
        , description =
            "Offset from the beginning of the list."
        }
      ]

let petsBaseSegment = C.LiteralPathSegment { segment = "pets" }

let petsUserSuppliedFields =
      [ { name =
            "name"
        , required =
            True
        , description =
            "The name of the pet."
        , type =
            C.ValueJSON { type = C.TextValue }
        }
      , { name =
            "tag"
        , required =
            False
        , description =
            "An optional tag to categorize the pet."
        , type =
            C.ValueJSON { type = C.TextValue }
        }
      ]

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
          [ { title =
                "Pets"
            , description =
                "Modify and access pets in the database."
            , paths =
                [ { location =
                      [ petsBaseSegment ]
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
                                { type =
                                    C.ReferenceListJSON { name = "Pet" }
                                , example =
                                    ''
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
                                    ''
                                }
                            }
                        }
                      , { description =
                            "Create a pet."
                        , request =
                            C.Post
                            { body =
                                C.RecordRequestBody
                                { fields = petsUserSuppliedFields }
                            }
                        , response =
                            { statusCode = 201, content = C.NoContentResponse }
                        }
                      ]
                  }
                , { location =
                      [ petsBaseSegment
                      , C.CapturePathSegment
                        { name =
                            "petId"
                        , description =
                            "The identifier of a pet to retrieve."
                        , type =
                            C.NaturalValue
                        }
                      ]
                  , endpoints =
                      [ { description =
                            "Access a single pet by its unique identifier."
                        , request =
                            C.Get { parameters = [] : List T.QueryParameter }
                        , response =
                            { statusCode =
                                200
                            , content =
                                C.JSONResponse
                                { type =
                                    C.ReferenceJSON { name = "Pet" }
                                , example =
                                    ''
                                    {
                                        "id": 1,
                                        "name": "Fido",
                                        "tag": null
                                    }
                                    ''
                                }
                            }
                        }
                      ]
                  }
                ]
            , definitions =
                [ { name =
                      "Pet"
                  , fields =
                      List/concat
                      T.Field
                      [ [ { name =
                              "id"
                          , required =
                              True
                          , description =
                              "Unique identifier for the pet."
                          , type =
                              C.ValueJSON { type = C.NaturalValue }
                          }
                        ]
                      , petsUserSuppliedFields
                      ]
                  }
                ]
            }
          ]
      }

in  spec
