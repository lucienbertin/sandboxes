"use server";
import * as infra from "@/infrastructure";
import * as domain from "@/domain";

export const getPlacesAsGeoJSON = domain.getPlacesAsGeoJSON(/*infra.resolveAgent, */infra.getPlacesGeoJSON);
export const createPlace = domain.createPlace(infra.resolveAgent, infra.createPlace);