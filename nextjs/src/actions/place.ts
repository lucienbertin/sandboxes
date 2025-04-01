"use server";
import { Place, UserRole } from "@/domain";
import { Feature, FeatureCollection, Point } from "geojson";
import * as infra from "@/infrastructure";
import { getServerSession } from "next-auth";

export async function getPlacesGeoJSON(): Promise<
  FeatureCollection<Point, Place>
> {
//   const session = await getServerSession();
//   let me = null;
//   if (session?.user?.email) {
//     me = await infra.getUserByEmail(session?.user?.email); // IO
//   }

  const places = await infra.getPlacesGeoJSON(); // IO
  // should the transformation to geojson happen here or in infra layer ?

  return places;
}

export async function createPlace(place: Feature<Point, Partial<Place>>) {
  const session = await getServerSession();
  let me = null;
  if (session?.user?.email) {
    me = await infra.getUserByEmail(session?.user?.email); // IO
  }

  if (!me || me.role == UserRole.Reader) { // Domain logic
    return Promise.reject(new Error("insufficient rights"));
  }

  await infra.createPlace(place); // IO
}

