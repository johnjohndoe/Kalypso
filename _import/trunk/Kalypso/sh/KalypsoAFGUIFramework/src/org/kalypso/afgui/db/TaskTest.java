package org.kalypso.afgui.db;

import java.util.List;

import junit.framework.TestCase;

import org.kalypso.afgui.model.IActivity;
import org.kalypso.afgui.model.internal.Task;

import test.org.kalypso.afgui.TestRDFModel;

import com.hp.hpl.jena.rdf.model.Model;
import com.hp.hpl.jena.rdf.model.Resource;

public class TaskTest extends TestCase {
	private Model model = TestRDFModel.getInstance().getWorkflowModel();

	private Resource resource = model.getResource(TestRDFModel.TASK1);

	public void testTask() {
		Task task1 = new Task(resource);
		List<IActivity> activity12 = task1.getActivities();
		ActivitySeq aSeq = new ActivitySeq(model
				.getResource(TestRDFModel.ACTIVITIES_1_2_URI));
		assertEquals(activity12, aSeq);

	}
}
