using UnityEngine;

// This script is attached to the LookAhead component of the JoyRide car.
// It detects collisions and signals them with an exogenous action.

public class LookAheadScript : MonoBehaviour {
    ErgoServer server;
    Transform myTrans;  

    void Start() {
        myTrans = transform;
        server = transform.parent.GetComponent<JoyRideCar>().server;
    }
    void OnTriggerEnter(Collider col) {
        Vector3 myPos = myTrans.position;
        Vector3 itsPos = col.ClosestPointOnBounds(myPos);
        float dist = Vector3.Distance(myPos,itsPos);
        server.signalExogenous("(object-detect! "+dist+")");
    }
    void OnTriggerExit(Collider col) {
        server.signalExogenous("(object-detect! 0)");
    }
}
