"use server"
export async function createDeployment(formData) {
    const name = formData.get('name');
    const image = formData.get('image');
    const replicas = parseInt(formData.get('replicas'), 10);
    const body = JSON.stringify({
        name: name,
        image: image,
        replicas: replicas
    });
    await fetch(`http://127.0.0.1:8000/deployments`, {
        method: "POST",
        headers: {
            "Content-Type": "application/json",
        },
        body: body
    });
}
export async function getDeployments() {
    const response = await fetch("http://127.0.0.1:8000/deployments");
    const data = await response.json();
    return data;
}
export async function getDeployment(name: string) {
    const response = await fetch(`http://127.0.0.1:8000/deployment/${name}`);
    const data = await response.json();
    return data;
}

export async function deleteDeployment(name: string) {
    const response = await fetch(`http://127.0.0.1:8000/deployment/${name}`, {
        method: "DELETE"
    });
}