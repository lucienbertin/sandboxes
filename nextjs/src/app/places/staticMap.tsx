import Image from "next/image";
import mapboxClient from "@mapbox/mapbox-sdk/services/static";

export default function StaticMap() {
  const width = 960;
  const height = 640;
  const token = "pk.eyJ1IjoibHVjaWVuYmVydGluIiwiYSI6ImNsMHJ4cW9idjAyNG4zYnBndXZkeXVuNjEifQ.8gkDcSKIddxBKkwucdo3JA";
  
  const client = mapboxClient({accessToken: token});
  const request = client.getStaticImage({
    ownerId: 'mapbox',
    styleId: 'outdoors-v12',
    width: width,
    height: height,
    position: {
      coordinates: [-1.5536, 47.2184], // nantes
      zoom: 11.5
    }, 
  });
  const src = request.url();
  return (
    <Image
    width={width}
    height={height}
    alt="a static map"
    src={src} />
  )
}