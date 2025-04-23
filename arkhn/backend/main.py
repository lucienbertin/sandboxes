from typing import Union
from kubernetes import client, config
from fastapi import FastAPI
config.load_kube_config()
app = FastAPI()

v1 = client.CoreV1Api()

@app.get("/")
def read_root():
    return {"Hello": "World"}


@app.get("/items/{item_id}")
def read_item(item_id: int, q: Union[str, None] = None):
    return {"item_id": item_id, "q": q}

@app.get("/pods")
def get_pods():
    ret = v1.list_pod_for_all_namespaces(watch=False)
    public_pods = filter(lambda p : p.metadata.namespace == "default", ret.items)
    for pod in public_pods:
        return { "ip": pod.status.pod_ip, "name": pod.metadata.name, "phase": pod.status.phase }