import PlacesMap from "./map";
import { getPlacesGeoJSON } from "@/actions";
import StaticMap from "./staticMap";
import { Suspense } from "react";
import Link from "next/link";

export default async function Page() {
  const places$ = getPlacesGeoJSON();

  return (
    <>
      <Link href="/places/create">Create</Link>
      <Suspense fallback={<StaticMap />}>
        <PlacesMap places$={places$} />
      </Suspense>
    </>
  );
}
