use iref::IriBuf;
use linked_data::{to_quads, Deserialize, Serialize};
use rdf_types::{generator, RdfDisplay};

// Define the schema.org Observation type
#[derive(Serialize, Deserialize)]
#[ld(prefix("schema" = "https://schema.org/"))]
#[ld(prefix("xsd" = "http://www.w3.org/2001/XMLSchema#"))]
#[ld(type = "https://schema.org/Observation")]
struct Observation {
    // The identifier for this observation
    #[ld(id)]
    id: Option<IriBuf>,

    // When the observation was made
    #[ld("schema:observationDate")]
    observation_date: String, // ISO 8601 date string like "2023-03-15T14:30:00Z"

    // The thing that was measured or observed
    #[ld("schema:variableMeasured")]
    variable_measured: String,

    // The value of the observation
    #[ld("schema:value")]
    value: u32,

    // The unit of measurement
    #[ld("schema:unitCode")]
    unit_code: String,

    // A description of the observation
    #[ld("schema:description")]
    description: Option<String>,
}

// A collection of observations
#[derive(Serialize, Deserialize)]
#[ld(prefix("schema" = "https://schema.org/"))]
struct ObservationCollection {
    #[ld(id)]
    id: IriBuf,

    #[ld("schema:name")]
    name: String,

    #[ld("schema:hasPart")]
    observations: Vec<Observation>,
}

fn main() {
    // Create an example Observation
    let observation = Observation {
        id: Some(IriBuf::new("https://example.org/observation/12345".to_string()).unwrap()),
        observation_date: "2023-03-15T14:30:00Z".to_string(),
        variable_measured: "Temperature".to_string(),
        value: 22,
        unit_code: "CEL".to_string(), // Celsius
        description: Some("Room temperature measurement".to_string()),
    };

    // Generate quads (RDF triples with an optional graph component)
    let quads = to_quads(generator::Blank::new(), &observation).expect("RDF serialization failed");

    println!("Generated RDF quads from the Observation:");
    for quad in quads {
        println!("{} .", quad.rdf_display())
    }

    // Create a collection of observations
    let collection = ObservationCollection {
        id: IriBuf::new("https://example.org/collections/temperature-log".to_string()).unwrap(),
        name: "Daily Temperature Log".to_string(),
        observations: vec![
            observation,
            Observation {
                id: Some(IriBuf::new("https://example.org/observation/12346".to_string()).unwrap()),
                observation_date: "2023-03-15T15:30:00Z".to_string(),
                variable_measured: "Temperature".to_string(),
                value: 23,
                unit_code: "CEL".to_string(),
                description: Some("Room temperature measurement after increasing thermostat".to_string()),
            },
        ],
    };

    // Generate quads for the collection
    let collection_quads = to_quads(generator::Blank::new(), &collection).expect("RDF serialization failed");

    println!("\nGenerated RDF quads from the ObservationCollection:");
    for quad in collection_quads {
        println!("{} .", quad.rdf_display())
    }
}