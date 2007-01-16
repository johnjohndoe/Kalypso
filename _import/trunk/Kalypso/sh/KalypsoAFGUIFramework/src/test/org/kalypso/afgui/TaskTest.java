package test.org.kalypso.afgui;

import java.util.List;

import org.kalypso.afgui.model.IActivity;
import org.kalypso.afgui.model.impl.ActivitySeq;
import org.kalypso.afgui.model.impl.Task;

import com.hp.hpl.jena.rdf.model.Model;
import com.hp.hpl.jena.rdf.model.Resource;

import junit.framework.TestCase;

public class TaskTest extends TestCase
{
	private Model model=TestRDFModel.getInstance().getWorkflowModel();
	
	private Resource resource= 
			model.getResource(TestRDFModel.TASK1);
	
	public void testTask()
	{
		Task task1= new Task(resource);
		List<IActivity> activity12=task1.getActivities();
		ActivitySeq aSeq= new ActivitySeq(model.getResource(TestRDFModel.ACTIVITIES_1_2_URI));
		assertEquals(activity12, aSeq);
		
	}
}
