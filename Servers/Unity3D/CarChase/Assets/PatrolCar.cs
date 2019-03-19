using UnityEngine;

// This is the script to be attached to the car "Patrol" in the scene.
// It reads arrow keys from the terminal and moves the car accordingly.

public class PatrolCar : MonoBehaviour {
    private float forwardSpeed = 80f;
    private float turnSpeed = 0.4f;
    private Rigidbody myCarBody;
    private Transform myCar;

    void Start () {
         myCarBody = GetComponent<Rigidbody>();
         myCar = GetComponent<Transform>();
    }

    void Update () {
         float forwardAmount = Input.GetAxis("Vertical")*forwardSpeed;
         float turnAmount = Input.GetAxis("Horizontal")*turnSpeed;
         myCar.Rotate(0,turnAmount,0);
         myCarBody.AddRelativeForce(0,0,forwardAmount);
    }
}
