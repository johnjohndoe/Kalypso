package org.kalypso.afgui.db;

import junit.framework.TestCase;

import org.kalypso.afgui.model.internal.Task;

import test.org.kalypso.afgui.TestRDFModel;

import com.hp.hpl.jena.rdf.model.Model;
import com.hp.hpl.jena.rdf.model.Seq;

/**
 * @author Patrice Congo
 * 
 */
public class TestTaskSeq extends TestCase {
	private Model model = TestRDFModel.getInstance().getWorkflowModel();

	private Seq resTask12 = model.getSeq(TestRDFModel.TASK_1_2);

	public void testTaskSeq() {
		TaskSeq taskSeq = new TaskSeq(resTask12);
		assertEquals(2, taskSeq.size());
		System.out.println("Tastseq[0]=" + taskSeq.get(0));
		assertEquals(new Task(model.getResource(TestRDFModel.TASK1)), taskSeq
				.get(0));
		assertEquals(new Task(model.getResource(TestRDFModel.TASK2)), taskSeq
				.get(1));
	}
}
