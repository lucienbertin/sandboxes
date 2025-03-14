"use client"
import { useRef, useEffect, useState, use } from 'react'
import mapboxgl from 'mapbox-gl'

import 'mapbox-gl/dist/mapbox-gl.css';
import { Feature, FeatureCollection, Point } from 'geojson';
import { IPlace } from '@/place.entity';

const INITIAL_CENTER: [
	number,
	number
] = [
    -1.5536,
    47.2184
]; // nantes
const INITIAL_ZOOM = 11.5;

export default function PlacesMap({
  places$,
}: {
  places$: Promise<FeatureCollection<Point, IPlace>>
}) {
    const mapRef = useRef<any>(null)
    const mapContainerRef = useRef<HTMLDivElement>(null)

    const places = use(places$);

    const [center, setCenter] = useState<[
        number,
        number
    ]>(INITIAL_CENTER)
    const [zoom, setZoom] = useState(INITIAL_ZOOM)
    const [selectedFeature, setSelectedFeature] = useState<Feature<Point, IPlace> | null>();

    const deselectPlace = () => {
        setSelectedFeature(null);
        if (selectedFeature) {
            console.log(selectedFeature);
            mapRef.current.setFeatureState({
                source: 'places',
                id: selectedFeature.id,
            }, { selected: false });
        }
    }

    useEffect(() => {
        mapboxgl.accessToken = 'pk.eyJ1IjoibHVjaWVuYmVydGluIiwiYSI6ImNsMHJ4cW9idjAyNG4zYnBndXZkeXVuNjEifQ.8gkDcSKIddxBKkwucdo3JA';
        mapRef.current = new mapboxgl.Map({
            container: mapContainerRef.current as HTMLDivElement,
            center: center,
            zoom: zoom,
        });

        mapRef.current.on('move', () => {
            // get the current center coordinates and zoom level from the map
            const mapCenter = mapRef.current.getCenter()
            const mapZoom = mapRef.current.getZoom()
      
            // update state
            setCenter([ mapCenter.lng, mapCenter.lat ])
            setZoom(mapZoom)
        })

        mapRef.current.on('load', () => {
            mapRef.current.addSource('places', {
                type: 'geojson',
                // Use a URL for the value for the `data` property.
                data: places
            });

            mapRef.current.addLayer({
                'id': 'places-layer',
                'type': 'circle',
                'source': 'places',
                'paint': {
                    'circle-color': [
                        'case',
                        ['boolean', ['feature-state', 'selected'], false],
                        '#f00',
                        '#4264fb'
                    ],
                    'circle-radius': [
                        'case',
                        ['boolean', ['feature-state', 'selected'], false],
                        6,
                        ['boolean', ['feature-state', 'highlight'], false],
                        6,
                        4
                    ],
                }
            });

            let selectedFeatureId: number | null = null;
            mapRef.current.addInteraction('click', {
                type: 'click',
                target: { layerId: 'places-layer' },
                handler: ({ feature }: any) => {
                    if (selectedFeatureId) {
                        console.log(selectedFeature);
                        mapRef.current.setFeatureState({
                            source: 'places',
                            id: selectedFeatureId,
                        }, { selected: false });
                    }
    
                    setSelectedFeature(feature);
                    mapRef.current.setFeatureState(feature, { selected: true });
                    selectedFeatureId = feature.id;
                }
            });
            // Hovering over a feature will highlight it
            mapRef.current.addInteraction('mouseenter', {
                type: 'mouseenter',
                target: { layerId: 'places-layer' },
                handler: ({ feature }: any) => {
                    mapRef.current.setFeatureState(feature, { highlight: true });
                    mapRef.current.getCanvas().style.cursor = 'pointer';
                }
            });

            // Moving the mouse away from a feature will remove the highlight
            mapRef.current.addInteraction('mouseleave', {
                type: 'mouseleave',
                target: { layerId: 'places-layer' },
                handler: ({ feature }: any) => {
                    mapRef.current.setFeatureState(feature, { highlight: false });
                    mapRef.current.getCanvas().style.cursor = '';
                    return false;
                }
            });
        })

    
        return () => {
            mapRef.current.remove()
        }
    }, [])
    return (
        <>
            <div className="sidebar">
                Longitude: {center[0].toFixed(4)} | Latitude: {center[1].toFixed(4)} | Zoom: {zoom.toFixed(2)} | Selected feature : {selectedFeature?.properties.name}
            </div>
            <div id='map-container' className="w-240 h-160 bg-white"  ref={mapContainerRef}/>

            {selectedFeature ? (
                <div className="">
                    <div>
                        <code>{selectedFeature?.properties.name}</code>
                        <span onClick={deselectPlace}> x</span>
                    </div>
                </div>
                ) : (<></>)}

        </>
    )
}