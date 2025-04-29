"use server"

import * as k8s from '@kubernetes/client-node';

const NAMESPACE = "default";
const kc = new k8s.KubeConfig();
kc.loadFromDefault();

const k8sApi = kc.makeApiClient(k8s.CoreV1Api);


export async function getPods() {
    const res = await k8sApi.listNamespacedPod({ namespace: NAMESPACE });
    const pods = res.items.map(i => ({ 
        ip: i.status?.podIP,
        name: i.metadata?.name,
        phase: i.status?.phase,
    }));

    return pods;
}


export async function getPodLogs(podName) {
    const res = await k8sApi.readNamespacedPodLog({name: podName, namespace: NAMESPACE});

    return res;
}
