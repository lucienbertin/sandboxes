"use server";
import { Place } from "@/domain";
import { Feature, FeatureCollection, Point } from "geojson";
import { auth, db } from "@/infrastructure";
import * as domain from "@/domain";

export async function getPlacesGeoJSON(): Promise<
  FeatureCollection<Point, Place>
> {
  return domain.getPlacesAsGeoJSON(
    // auth.resolveAgent,
    db.getPlacesGeoJSON,
  );
}

export async function createPlace(place: Feature<Point, Partial<Place>>) {
  return domain.createPlace(place, auth.resolveAgent, db.createPlace);
}
