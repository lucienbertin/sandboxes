"use server";
import { Place } from "@/domain";
import { Feature, FeatureCollection, Point } from "geojson";
import * as infra from "@/infrastructure";
import * as domain from "@/domain";

export async function getPlacesGeoJSON(): Promise<
  FeatureCollection<Point, Place>
> {
  return domain.getPlacesAsGeoJSON(
    // infra.resolveAgent,
    infra.getPlacesGeoJSON,
  );
}

export async function createPlace(place: Feature<Point, Partial<Place>>) {
  return domain.createPlace(place, infra.resolveAgent, infra.createPlace);
}
