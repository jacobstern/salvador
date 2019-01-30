let C = ../../dhall/constructors.dhall

let T = ../../dhall/types.dhall

let List/concat =
      https://raw.githubusercontent.com/dhall-lang/dhall-lang/0a7f596d03b3ea760a96a8e03935f4baa64274e1/Prelude/List/concat sha256:43ef75a328d312c7fed8fbaf25d945b244d1b96505e3f1c291567ecee972449c

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
            "offset"
        , type =
            C.ValueParameter { valueType = C.NaturalValue }
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

let spec
    : T.Spec
    = { title =
          "Petstore"
      , validation =
          { acceptOptional =
              C.AllowNullUndefinedMissing
          , returnOptional =
              C.AllowNull
          }
      , modules =
          [ { title =
                "Overview"
            , description =
                ''
                This is a loose port of the [OpenAPI Petstore
                example](https://github.com/OAI/OpenAPI-Specification/blob/master/examples/v3.0/petstore.yaml)
                to the Salvador spec format.
                
                It demonstrates some of the nice features of Dhall and Salvador
                for specifying and documenting APIs.
                ''
            , paths =
                [] : List T.Path
            , definitions =
                [] : List T.NamedRecord
            }
          , { title =
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
                        , valueType =
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
                  , description =
                      ''
                      A pet data structure returned from the API.
                      ''
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
                              C.ValueJSON { valueType = C.NaturalValue }
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
