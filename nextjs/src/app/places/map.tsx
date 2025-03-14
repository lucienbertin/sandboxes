"use client"
import { useRef, useEffect, useState } from 'react'
import mapboxgl from 'mapbox-gl'

import 'mapbox-gl/dist/mapbox-gl.css';

const INITIAL_CENTER: [
	number,
	number
] = [
    -1.5536,
    47.2184
]; // nantes
const INITIAL_ZOOM = 11.5;

export default function PlacesMap() {
    const mapRef = useRef<any>(null)
    const mapContainerRef = useRef<HTMLDivElement>(null)

    const [center, setCenter] = useState<[
        number,
        number
    ]>(INITIAL_CENTER)
    const [zoom, setZoom] = useState(INITIAL_ZOOM)

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
    
        return () => {
            mapRef.current.remove()
        }
    }, [])
    return (
        <>
            <div className="sidebar">
                Longitude: {center[0].toFixed(4)} | Latitude: {center[1].toFixed(4)} | Zoom: {zoom.toFixed(2)}
            </div>
            <div id='map-container' className="w-240 h-160 bg-white"  ref={mapContainerRef}/>
        </>
    )
}