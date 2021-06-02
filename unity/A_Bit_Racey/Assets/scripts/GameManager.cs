using UnityEngine;
using UnityEngine.SceneManagement;

public class GameManager : MonoBehaviour {

	bool gameon = false;
	public float restartdelay = 1f;
	public GameObject completelevelui;

	public void completelevel(){
		completelevelui.SetActive(true);
	}
	public void EndGame(){
		if(gameon == false){
			gameon = true;
			Debug.Log("gameover");
			Restart();
		}
	}
	void Restart(){
		SceneManager.LoadScene(SceneManager.GetActiveScene().buildIndex);
	}

}