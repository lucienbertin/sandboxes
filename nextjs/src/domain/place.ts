import { Feature, FeatureCollection, Point } from "geojson";
import { ForbiddenError, UnauthorizedError } from "./error";
import { AgentDelegate, UserRole } from "./user";


export type Place = {
  id: number;
  name: string;
};

type GetPlacesGeoJSONDelegate = () => Promise<FeatureCollection<Point, Place>>;
export async function getPlacesAsGeoJSON(
  // agentDelegate: AgentDelegate,
  getPlacesDelegate: GetPlacesGeoJSONDelegate,
): Promise<FeatureCollection<Point, Place>> {
  // const agent = await agentDelegate(); // IO - injected

  const places = await getPlacesDelegate(); // IO - injected
  // should the transformation to geojson happen here or in another layer ?

  return places;
}

type CreatePlaceDelegate = (
  place: Feature<Point, Partial<Place>>,
) => Promise<void>;
export async function createPlace(
  place: Feature<Point, Partial<Place>>,
  agentDelegate: AgentDelegate,
  createPlaceDelegate: CreatePlaceDelegate,
) {
  const agent = await agentDelegate(); // IO - injected

  // Domain logic
  if (!agent) {
    return Promise.reject(new UnauthorizedError());
  } else if (agent.role == UserRole.Reader) {
    return Promise.reject(new ForbiddenError());
  }

  await createPlaceDelegate(place); // IO - injected
}
