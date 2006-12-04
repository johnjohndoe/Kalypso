package test.org.kalypso.afgui;

import org.kalypso.afgui.model.impl.WorkflowData;
import org.kalypso.afgui.model.impl.WorkflowImpl;
import org.kalypso.afgui.model.impl.WorkflowPartRTContext;

import com.hp.hpl.jena.rdf.model.Model;
import com.hp.hpl.jena.rdf.model.Resource;
import com.hp.hpl.jena.rdf.model.Seq;

import junit.framework.TestCase;

/**
 * @author Patrice Congo
 *
 */
public class WorkflowPartRTContextTest extends TestCase
{
private Model model=TestRDFModel.getInstance().getWorkflowModel();
	
	private Resource resource= 
			model.getResource(TestRDFModel.WORKFLOW_PART_RT_CONTEX_1);
	
	public void testWorkflowPartRTContext()
	{
		WorkflowPartRTContext rtContext=
			new WorkflowPartRTContext(resource);
		assertEquals(
				new WorkflowData(
						model.getResource(TestRDFModel.DATA_MOD_1)),
				rtContext.getProcessedWorkflowData());
		assertEquals(
				new WorkflowImpl(
						model.getResource(TestRDFModel.WORKFLOW1)), 
				rtContext.getExecutingWorkflowPart());
				
	}
}
