using System.Collections;
using System.Collections.Generic;
using UnityEngine;

[System.Serializable]
class SpawnReplication {
    public string type;
    public int actorId;
    public string className;
    public float locX;
    public float locY;
    public float locZ;
    public float bias;
}

[System.Serializable]
class ComponentUpdate {
    public int id;
    public string className;
    public float newLocX;
    public float newLocY;
    public float newLocZ;
    public float newRotX;
    public float newRotY;
    public float newRotZ;
    public float bias;
    public float rotLimit;
}

[System.Serializable]
class UpdateReplication {
    public string type;
    public int actorId;
    public List<ComponentUpdate> components;
}

[System.Serializable]
class DestroyReplication {
    public string type;
    public int actorId;
}

[System.Serializable]
class Frame {
    public float time;
    public float delta;
    public List<SpawnReplication> spawned;
    public List<UpdateReplication> updated;
    public List<DestroyReplication> destroyed;
}

[System.Serializable]
class Replay {
    public List<Frame> frames;

    public static Replay CreateFromJSON(string jsonString)
    {
        return JsonUtility.FromJson<Replay>(jsonString);
    }
}

public class ReplayManager : MonoBehaviour {
    public TextAsset json;
    public GameObject ballPrefab;
    public GameObject carPrefab;

    Replay replay;
    Queue<Frame> queuedFrames;
    Dictionary<int, GameObject> actors;

    void Restart() {
        actors = new Dictionary<int, GameObject>();
        replay = Replay.CreateFromJSON(json.text);
        queuedFrames = new Queue<Frame>(replay.frames);

        // Make sure there are things to actually spawn
        Debug.Assert(replay.frames.FindAll(f => f.spawned.Count > 0).Count > 0);
        Debug.Log(string.Format("Replay loaded. {0} frames.", queuedFrames.Count.ToString()));
    }

    void Start() {
        Restart();
    }

    void HandleSpawns(List<SpawnReplication> spawns) {
        foreach(SpawnReplication spawn in spawns) {
            if (!actors.ContainsKey(spawn.actorId))
            {
                Vector3 pos = new Vector3(spawn.locX, spawn.locZ, spawn.locY) / 4096f * 10f;
                GameObject actor = null;

                switch (spawn.className)
                {
                    case "TAGame.Ball_TA":
                        actor = Instantiate(ballPrefab, pos, Quaternion.identity);
                        break;
                    case "TAGame.Car_TA":
                        actor = Instantiate(carPrefab, pos, Quaternion.identity);
                        break;
                    default:
                        Debug.Log(string.Format("Attempted to spawn unknown class: {0}", name));
                        break;
                }

                actors.Add(spawn.actorId, actor);
                Debug.Log("Spawned: " + spawn.actorId.ToString());
            }
        }
    }

    void HandleUpdates(List<UpdateReplication> udpated) {
        foreach (UpdateReplication update in udpated) {
            GameObject actor;
            actors.TryGetValue(update.actorId, out actor);

            if (actor != null)
            {
                foreach (ComponentUpdate c in update.components)
                {
                    Vector3 newPos = new Vector3(c.newLocX, c.newLocZ, c.newLocY) / 4096f * 10f;
                    actor.transform.position = newPos;

                    Vector3 euler = new Vector3(c.newRotX, c.newRotY, c.newRotZ) / c.rotLimit * 360f;
                    actor.transform.eulerAngles = euler;
                }
            }
        }
    }

    void HandleDestruction(List<DestroyReplication> destroyed) {
        foreach (DestroyReplication dr in destroyed) {
            GameObject actor;
            actors.TryGetValue(dr.actorId, out actor);

            if (actor != null) {
                actors.Remove(dr.actorId);
                Destroy(actor);
                Debug.Log("Destroyed: " + dr.actorId.ToString());
            }
        }
    }

    void Update() {
        if (replay != null && queuedFrames.Count > 0) {
            Frame currentFrame = queuedFrames.Dequeue();
            // Debug.Log(currentFrame.time.ToString());

            if (currentFrame.destroyed.Count > 0) {
                Debug.Log(currentFrame.destroyed.ToString());
            }

            HandleSpawns(currentFrame.spawned);
            HandleUpdates(currentFrame.updated);
            HandleDestruction(currentFrame.destroyed);
        }
    }
}
