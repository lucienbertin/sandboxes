"use client";

import { useState, useCallback, useRef } from "react";
import Map from "react-map-gl/mapbox";
import { createPlace } from "@/infrastructure";

import MapboxDraw from "@mapbox/mapbox-gl-draw";
import { useControl } from "react-map-gl/mapbox";

import type { MapRef, ControlPosition } from "react-map-gl/mapbox";
import { Feature, Point } from "geojson";
import { Place } from "@/domain";
import { useSession } from "next-auth/react";

type DrawControlProps = ConstructorParameters<typeof MapboxDraw>[0] & {
  position?: ControlPosition;

  onCreate?: (evt: { features: Feature<Point>[] }) => void;
  onUpdate?: (evt: { features: Feature<Point>[]; action: string }) => void;
  onDelete?: (evt: { features: Feature<Point>[] }) => void;
};

function DrawControl(props: DrawControlProps) {
  useControl<MapboxDraw>(
    () => new MapboxDraw(props),
    ({ map }: { map: MapRef }) => {
      map.on("draw.create", props.onCreate as any); // eslint-disable-line @typescript-eslint/no-explicit-any
      map.on("draw.update", props.onUpdate as any); // eslint-disable-line @typescript-eslint/no-explicit-any
      map.on("draw.delete", props.onDelete as any); // eslint-disable-line @typescript-eslint/no-explicit-any
    },
    ({ map }: { map: MapRef }) => {
      map.off("draw.create", props.onCreate as any); // eslint-disable-line @typescript-eslint/no-explicit-any
      map.off("draw.update", props.onUpdate as any); // eslint-disable-line @typescript-eslint/no-explicit-any
      map.off("draw.delete", props.onDelete as any); // eslint-disable-line @typescript-eslint/no-explicit-any
    },
    {
      position: props.position,
    },
  );

  return null;
}

DrawControl.defaultProps = {
  onCreate: () => {},
  onUpdate: () => {},
  onDelete: () => {},
};

const MAPBOX_TOKEN =
  "pk.eyJ1IjoibHVjaWVuYmVydGluIiwiYSI6ImNsMHJ4cW9idjAyNG4zYnBndXZkeXVuNjEifQ.8gkDcSKIddxBKkwucdo3JA";
const INITIAL_CENTER: [number, number] = [-1.5536, 47.2184]; // nantes
const INITIAL_ZOOM = 11.5;

export default function Form() {
  const mapRef = useRef<MapRef>(null);
  const { data: session } = useSession();
  const me = session?.user;
  console.log(me);

  const [feature, setFeature] = useState<Feature<Point> | null>();

  const onUpdate = useCallback((evt: { features: Feature<Point>[] }) => {
    console.log(evt);

    setFeature(evt.features[0]);
  }, []);

  const onDelete = useCallback(() => {
    setFeature(null);
  }, []);

  const handler = async (formData: FormData) => {
    console.log(feature);
    console.log(formData);

    if (!!feature && !!formData.get("name")) {
      const place: Feature<Point, Partial<Place>> = {
        type: "Feature",
        geometry: feature.geometry,
        properties: {
          name: formData.get("name") as string,
        },
      };

      await createPlace(place);
    }
  };

  return (
    <>
      <form action={handler} className="w-240 h-20 bg-white">
        <input type="text" name="name" />
        <button type="submit" disabled={!feature}>
          Create
        </button>
      </form>
      <div className="w-240 h-160 bg-white">
        <Map
          initialViewState={{
            longitude: INITIAL_CENTER[0],
            latitude: INITIAL_CENTER[1],
            zoom: INITIAL_ZOOM,
          }}
          mapStyle="mapbox://styles/mapbox/outdoors-v12"
          mapboxAccessToken={MAPBOX_TOKEN}
          ref={mapRef}
        >
          <DrawControl
            position="top-left"
            displayControlsDefault={false}
            controls={{
              polygon: true,
              trash: true,
            }}
            defaultMode="draw_point"
            onCreate={onUpdate}
            onUpdate={onUpdate}
            onDelete={onDelete}
          />
        </Map>
      </div>
    </>
  );
}
