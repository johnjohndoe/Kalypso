package test.org.kalypso.afgui;


import org.kalypso.afgui.model.impl.SubTaskGroupSeq;
import org.kalypso.afgui.model.impl.TaskGroup;

import com.hp.hpl.jena.rdf.model.Model;
import com.hp.hpl.jena.rdf.model.Resource;

import junit.framework.TestCase;

/**
 *
 * @author Patrice Congo
 */
public class TaskGroupTest extends TestCase
{
	private Model model=TestRDFModel.getInstance().getWorkflowModel();
	private Resource tgRes=model.getSeq(TestRDFModel.TASKGROUP1);
	
	public void testTaskGroup()
	{
		TaskGroup tg= new TaskGroup(tgRes);
		assertEquals( 
				new SubTaskGroupSeq(
						model.getSeq(TestRDFModel.SUBTASKGROUP_1_2)),
				tg.getSubTaskGroups());		
	}
}
