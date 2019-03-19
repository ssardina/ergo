using UnityEngine;

// This is the script to be attached to the "JoyRide" car in the scene.
// It accepts actions via an ErgoServer and moves the car accordingly.

public class JoyRideCar : MonoBehaviour {

    private float forwardSpeed = 35f;
    private float turnSpeed = 0.6f;
    private Rigidbody myCarBody;
    private Transform myCar;

    private int turnDir = 0;
    private int forwardDir = 0;
    public ErgoServer server = new ErgoServer();

    void Start () {
         myCarBody = GetComponent<Rigidbody>();
         myCar = GetComponent<Transform>();
         server.Start();
    }

    void Update () {
         server.Update();
         if (server.hasData) {
            string act = server.getEndogenousAct();
            switch(act) {
              case "right-turn!": turnDir = 1; break;
              case "left-turn!":  turnDir = -1; break;
              case "straight!":   turnDir = 0; break;
              case "stop!":       forwardDir = 0; break;
              case "go!":         forwardDir = 1; break;                  
              case "reverse!":    forwardDir = -1; break;
            }
         }
         myCar.Rotate(0,turnDir*turnSpeed,0);
         myCarBody.AddRelativeForce(0,0,forwardDir*forwardSpeed);
    }
}
