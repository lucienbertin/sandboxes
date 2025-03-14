import PlacesMap from "./map";
import { getPlacesGeoJSON, isInitialized } from "@/datasource";

export default async function Page() {
    await isInitialized;
    const places$ = getPlacesGeoJSON();

    return (
        <PlacesMap places$={places$} />
    )
  }