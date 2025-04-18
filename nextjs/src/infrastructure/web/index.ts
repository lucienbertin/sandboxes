import { Post } from "@/domain";

const token = "eyJhbGciOiJIUzI1NiJ9.eyJzdWIiOiJsdWNpZW5AYmVydC5pbiJ9.MQ2AtPRuMhZuu84jFpjbnZF3tMREpSi51YEU6yq8KBI"; // hardcoded osef

export async function getPosts(): Promise<Post[]> {
    const response = await fetch("http://localhost:8001/api/posts", {
        headers: {
            "Authorization": `Bearer ${token}`,
        }
    });

    const data = await response.json();

    return data;
}
