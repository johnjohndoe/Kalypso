package test.org.kalypso.afgui;

import org.kalypso.afgui.model.impl.TaskGroup;
import org.kalypso.afgui.model.impl.TaskGroupSeq;

import com.hp.hpl.jena.rdf.model.Model;
import com.hp.hpl.jena.rdf.model.Resource;
import com.hp.hpl.jena.rdf.model.Seq;

import junit.framework.TestCase;

/**
 * @author Patrice Congo
 */
public class TaskGroupSeqTest extends TestCase
{
	private Model model=
			TestRDFModel.getInstance().getWorkflowModel();
	
	private Seq tgRes=
			model.getSeq(TestRDFModel.TASKGROUP_1_2);
	
	public void testTaskGroup()
	{
		TaskGroupSeq tgSeq= new TaskGroupSeq(tgRes); 
		assertEquals(2,tgSeq.size());
		assertEquals(
				new TaskGroup(
						model.getResource(TestRDFModel.TASKGROUP1)),
				tgSeq.get(0));
		assertEquals(
				new TaskGroup(
						model.getResource(TestRDFModel.TASKGROUP2)),
				tgSeq.get(1));
	}
	
}
