{
    "$schema": "https://vega.github.io/schema/vega-lite/v5.json",
    "description": "the population per state, engineers per state, and hurricanes per state",
    "repeat": {"row": ["population", "engineers", "hurricanes"]},
    "resolve": {
      "scale": {
        "color": "independent"
      }
    },
    "spec": {
      "width": 500,
      "height": 300,
      "data": {
        "url": "data/population_engineers_hurricanes.csv"
      },
      "transform": [
        {
          "lookup": "id",
          "from": {
            "data": {
              "url": "data/us-10m.json",
              "format": {
                "type": "topojson",
                "feature": "states"
              }
            },
            "key": "id"
          },
          "as": "geo"
        }
      ],
      "projection": {"type": "albersUsa"},
      "mark": "geoshape",
      "encoding": {
        "shape": {
          "field": "geo",
          "type": "geojson"
        },
        "color": {
          "field": {"repeat": "row"},
          "type": "quantitative"
        }
      }
    }
  }