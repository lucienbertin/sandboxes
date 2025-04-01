import PlacesMap from "./map";
import { getPlacesGeoJSON } from "@/actions";
import StaticMap from "./staticMap";
import { Suspense } from "react";

export default async function Page() {
  const places$ = getPlacesGeoJSON();

  return (
    <>
      <Suspense fallback={<StaticMap />}>
        <PlacesMap places$={places$} />
      </Suspense>
    </>
  );
}
