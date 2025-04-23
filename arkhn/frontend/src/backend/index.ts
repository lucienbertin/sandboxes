"use server"

import * as k8s from '@kubernetes/client-node';

const kc = new k8s.KubeConfig();
kc.loadFromDefault();

const k8sApi = kc.makeApiClient(k8s.CoreV1Api);

export async function getPods() {
    const res = await k8sApi.listNamespacedPod({ namespace: 'default' });
    const pods = res.items.map(i => ({ 
        ip: i.status?.podIP,
        name: i.metadata?.name,
        phase: i.status?.phase,
    }));

    return pods;
}
