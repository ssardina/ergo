using UnityEngine;

// This script is attached to the RearBumper component of the JoyRide car.
// It detects contact with the Patrol car and ends the game. 

public class BumperScript : MonoBehaviour {
    Light overhead;   

    void Start() {
        overhead = GameObject.Find("OverheadLight").GetComponent<Light>();
    }
    void OnTriggerEnter(Collider col) {
        overhead.intensity = 0.0f;
        Application.Quit();
    }
}
