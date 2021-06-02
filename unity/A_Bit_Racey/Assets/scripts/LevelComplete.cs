using UnityEngine;
using UnityEngine.SceneManagement;
public class LevelComplete : MonoBehaviour {

	public void load(){
		SceneManager.LoadScene(SceneManager.GetActiveScene().buildIndex + 1);
	}
}
