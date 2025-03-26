"use client"
import { useRef, use } from 'react'

import 'mapbox-gl/dist/mapbox-gl.css';
import { FeatureCollection, Point } from 'geojson';
import { Place } from '@/domain';
import Map, { Layer, MapRef, Source } from 'react-map-gl/mapbox';

const INITIAL_CENTER: [
	number,
	number
] = [
    -1.5536,
    47.2184
]; // nantes
const INITIAL_ZOOM = 11.5;
const MAPBOX_TOKEN = 'pk.eyJ1IjoibHVjaWVuYmVydGluIiwiYSI6ImNsMHJ4cW9idjAyNG4zYnBndXZkeXVuNjEifQ.8gkDcSKIddxBKkwucdo3JA';
export default function PlacesMap({
  places$,
}: {
  places$: Promise<FeatureCollection<Point, Place>>
}) {
    const mapRef = useRef<MapRef>(null)
    const places = use(places$);

    return (
        <div className="w-240 h-160 bg-white">
            <Map
                initialViewState={{
                    longitude: INITIAL_CENTER[0],
                    latitude: INITIAL_CENTER[1],
                    zoom: INITIAL_ZOOM
                }}
                mapStyle="mapbox://styles/mapbox/outdoors-v12"
                mapboxAccessToken={MAPBOX_TOKEN}
                ref={mapRef}
            >
                <Source id="places" type="geojson" data={places} />
                <Layer
                    id="places-marker"
                    type="circle"
                    source="places"
                    paint={{'circle-color': '#f00', 'circle-radius': 6}}
                />
            </Map>
        </div>
    )
}