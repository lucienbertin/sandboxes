use crate::models::{Agent, Place};

#[derive(PartialEq, Debug)]
pub enum CreatePlaceResult {
    DoCreate(Place),
    CantCreateAsUser,
}

pub fn create_place(agent: &Agent, place: &Place) -> CreatePlaceResult {
    use CreatePlaceResult::*;
    match agent {
        Agent::Worker => DoCreate(place.clone()),
        _ => CantCreateAsUser,
    }
}
#[cfg(test)]
mod test {
    use crate::models::{Role, User};

    use super::*;

    #[test]
    fn user_cant_create_place() {
        let admin = User {
            id: 1,
            first_name: "test".to_string(),
            last_name: "test".to_string(),
            email: "test@te.st".to_string(),
            role: Role::Admin,
        };
        let place = Place {
            id: 1,
            name: "test".to_string(),
            geom: vec![0f64, 0f64],
        };
        let agent = Agent::User(admin);

        let result = create_place(&agent, &place);

        assert!(matches!(result, CreatePlaceResult::CantCreateAsUser));
    }
    #[test]
    fn worker_can_create_place() {
        let place = Place {
            id: 1,
            name: "test".to_string(),
            geom: vec![0f64, 0f64],
        };
        let agent = Agent::Worker;

        let result = create_place(&agent, &place);

        assert!(matches!(result, CreatePlaceResult::DoCreate(_)));

        if let CreatePlaceResult::DoCreate(p) = result {
            assert_eq!(p, place);
        }
    }
}
