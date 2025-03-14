import prisma from "@/lib/prisma";
import PlacesMap from "./map";

export default async function Page() {
    const places = await prisma.place.findMany();

    console.log(places)

    return (
        <PlacesMap />
    )
  }