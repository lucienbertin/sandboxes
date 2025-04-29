from kubernetes import client, config
from fastapi import FastAPI, HTTPException
from pydantic import BaseModel

NAMESPACE = "default"
config.load_kube_config()
core_v1 = client.CoreV1Api()
apps_v1 = client.AppsV1Api()

app = FastAPI()

class Deployment(BaseModel):
    name: str
    image: str
    replicas: int = 1

@app.get("/deployments")
def list_deployments() -> list[Deployment]:
    ret = apps_v1.list_deployment_for_all_namespaces(watch=False)
    public_deps = filter(lambda p : p.metadata.namespace == NAMESPACE, ret.items)
    deps = map(to_deployment, public_deps)
    return deps

@app.get("/deployment/{name}")
def get_deployment(name:str) -> Deployment | None:
    ret = apps_v1.list_deployment_for_all_namespaces(watch=False)
    public_deps = filter(lambda p : p.metadata.namespace == NAMESPACE, ret.items)
    first_or_default = next((i for i in public_deps if i.metadata.name == name), None)
    if first_or_default == None:
        raise HTTPException(status_code=404)
    return to_deployment(first_or_default)

@app.post("/deployments", status_code=201)
def create_deployment(deployment: Deployment):
    body = create_deployment_object(deployment)
    try:
        apps_v1.create_namespaced_deployment(
            body=body, namespace=NAMESPACE
        )
    except:
        raise HTTPException(status_code=500)

@app.delete("/deployment/{name}")
def delete_deployment(name:str):
    try:
        apps_v1.delete_namespaced_deployment(
            name=name,
            namespace=NAMESPACE,
        )
    except:
        raise HTTPException(status_code=410)


def to_deployment(item):
    containers = item.spec.template.spec.containers
    image = containers[0].image
    return Deployment(name=item.metadata.name, image=image, replicas=item.spec.replicas)

def create_deployment_object(dep):
    # Configureate Pod template container
    container = client.V1Container(
        name=dep.name,
        image=dep.image,
        ports=[client.V1ContainerPort(container_port=80)],
        resources=client.V1ResourceRequirements(
            requests={"cpu": "100m", "memory": "200Mi"},
            limits={"cpu": "500m", "memory": "500Mi"},
        ),
    )

    # Create and configure a spec section
    template = client.V1PodTemplateSpec(
        metadata=client.V1ObjectMeta(labels={"app": "nginx"}),
        spec=client.V1PodSpec(containers=[container]),
    )

    # Create the specification of deployment
    spec = client.V1DeploymentSpec(
        replicas=dep.replicas, template=template, selector={
            "matchLabels":
            {"app": "nginx"}})

    # Instantiate the deployment object
    deployment = client.V1Deployment(
        api_version="apps/v1",
        kind="Deployment",
        metadata=client.V1ObjectMeta(name=dep.name),
        spec=spec,
    )

    return deployment