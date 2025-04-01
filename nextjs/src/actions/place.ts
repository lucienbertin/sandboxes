"use server";
import { Place, UserRole } from "@/domain";
import { Feature, FeatureCollection, Point } from "geojson";
import * as infra from "@/infrastructure";

export async function getPlacesGeoJSON(): Promise<
  FeatureCollection<Point, Place>
> {
  // const agent = await infra.resolveAgent(); // IO
  const places = await infra.getPlacesGeoJSON(); // IO
  // should the transformation to geojson happen here or in infra layer ?

  return places;
}

export async function createPlace(place: Feature<Point, Partial<Place>>) {
  const agent = await infra.resolveAgent(); // IO

  if (!agent || agent.role == UserRole.Reader) {
    // Domain logic
    return Promise.reject(new Error("insufficient rights"));
  }

  await infra.createPlace(place); // IO
}
