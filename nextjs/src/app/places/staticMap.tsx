import Image from "next/image";

export default function StaticMap() {
    const host = "https://api.mapbox.com/styles/v1";
    const style = "mapbox/streets-v12";
    const position = "-1.5536,47.2184,11.5";
    const width = 960;
    const height = 640;
    const token = "pk.eyJ1IjoibHVjaWVuYmVydGluIiwiYSI6ImNsMHJ4cW9idjAyNG4zYnBndXZkeXVuNjEifQ.8gkDcSKIddxBKkwucdo3JA";
    const src = `${host}/${style}/static/${position}/${width}x${height}?access_token=${token}`;
    return (
        <Image
                width={width}
                height={height}
                alt="a static map"
                src={src} />
    )
}