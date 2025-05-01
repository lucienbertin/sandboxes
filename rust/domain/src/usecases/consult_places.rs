use crate::models::Agent;

#[derive(PartialEq, Debug)]
pub enum ConsultPlacesResult {
    ConsultAllPlaces,
}
pub fn consult_places(agent: &Agent) -> ConsultPlacesResult {
    use ConsultPlacesResult::*;
    match agent {
        _ => ConsultAllPlaces, // open bar for now
    }
}

#[cfg(test)]
mod test {
    // not sure its relevant to write a UT for it
}
