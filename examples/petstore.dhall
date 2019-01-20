let C = ../dhall/constructors.dhall

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

in  { title =
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
    , sections =
        [ { name =
              "Pets"
          , description =
              "Modify and access pets in the database."
          , paths =
              [ { location =
                    "/pets"
                , endpoints =
                    [ { description =
                          "List all pets."
                      , request =
                          C.Get { parameters = paginationQueryParameters }
                      , response =
                          { statusCode = 200 }
                      }
                    ]
                }
              ]
          }
        ]
    }
