"use server";

import { Place } from "@/domain";
import { datasource, isInitialized, ORMPlace } from "./datasource";
import { Feature, FeatureCollection, Point } from "geojson";
import { DeepPartial } from "typeorm";

export async function getPlaces(): Promise<Place[]> {
  await isInitialized;
  const places = await datasource.getRepository(ORMPlace).find();
  return places.map((p) => p.asRecord());
}

export async function getPlacesGeoJSON(): Promise<
  FeatureCollection<Point, Place>
> {
  await isInitialized;
  const places = await datasource.getRepository(ORMPlace).find();
  return {
    type: "FeatureCollection",
    features: places.map((p) => p.asGeoJSON()),
  };
}

export async function createPlace(
  newPlace: Feature<Point, DeepPartial<Place>>,
) {
  await isInitialized;

  const repo = datasource.getRepository(ORMPlace);
  const place = repo.create({ ...newPlace.properties });
  place.geometry = newPlace.geometry;

  await repo.save(place);
}