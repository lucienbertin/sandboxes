import PlacesMap from "./map";
import { getPlacesGeoJSON, isInitialized } from "@/datasource";
import StaticMap from "./staticMap";
import { Suspense } from "react";

export default async function Page() {
    await isInitialized;
    const places$ = getPlacesGeoJSON();

    return (
        <>
            <Suspense fallback={<StaticMap />}>
                <PlacesMap places$={places$} />
            </Suspense>
        </>
    )
  }