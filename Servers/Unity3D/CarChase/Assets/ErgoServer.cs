using UnityEngine;
using System;
using System.IO;
using System.Net;
using System.Net.Sockets;
using System.Collections;

public class ErgoServer {
	
	public int tcpPort = 8123;
	public string tcpHost = "127.0.0.1";
	
    private TcpListener listener;
    private Socket client;
    private NetworkStream stream;
    private StreamReader reader;
	private StreamWriter writer;

	private void TryConnect() {
    	if (!listener.Pending()) return;
 	    client = listener.AcceptSocket();
        stream = new NetworkStream(client);
        reader = new StreamReader(stream);
        writer = new StreamWriter(stream);
        writer.AutoFlush = true;
        Debug.Log("Ergo client connected");
	}
	
	//------------------  External Interface  ----------------------------

	public Boolean hasData  = false;
	
	public void signalExogenous(String str) {
	    if (writer != null) { writer.WriteLine(str); }
	}

	public string getEndogenousAct() {
		hasData = false;
		return reader.ReadLine();
	}

	public void Start() {
		listener = new TcpListener(IPAddress.Parse(tcpHost),tcpPort);
		listener.Start();
    	Debug.Log("Ready for Ergo connection on port "+tcpPort);
	}

	public void Stop() {
		Debug.Log("Ending Ergo connection");
		if (reader != null) reader.Close();
		if (writer != null) writer.Close();
		if (stream != null) stream.Close();
    	if (listener != null) listener.Stop();
    	listener = null;
 	}

	public void Update() {
		if (listener != null && client == null) {
			TryConnect();
		} else if (!client.Connected) {
			return;
		} else if (!hasData) {
			if (!stream.DataAvailable) return;
			hasData = true;
		} 
	}
}
