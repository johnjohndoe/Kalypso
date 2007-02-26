package test.org.kalypso.afgui;

import org.kalypso.afgui.model.impl.SubTaskGroup;
import org.kalypso.afgui.model.impl.TaskSeq;

import com.hp.hpl.jena.rdf.model.Model;
import com.hp.hpl.jena.rdf.model.Resource;

import junit.framework.TestCase;

/**
 * @author Patrice Congo
 *
 */
public class SubTaskGroupTest extends TestCase
{
	private Model model=TestRDFModel.getInstance().getWorkflowModel();
	private Resource taskGroup1=model.getSeq(TestRDFModel.SUBTASKGROUP1);
	
	public void testTaskGroup()
	{
		SubTaskGroup tg1= new SubTaskGroup(taskGroup1);		
		assertEquals( 
				new TaskSeq(model.getSeq(TestRDFModel.TASK_1_2)),
				tg1.getTasks());
	}
}
