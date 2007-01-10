package test.org.kalypso.afgui;

import junit.framework.TestCase;

import org.kalypso.afgui.model.internal.WorkflowData;
import org.kalypso.afgui.model.internal.WorkflowImpl;
import org.kalypso.afgui.model.internal.WorkflowPartRTContext;

import com.hp.hpl.jena.rdf.model.Model;
import com.hp.hpl.jena.rdf.model.Resource;

/**
 * @author Patrice Congo
 * 
 */
public class WorkflowPartRTContextTest extends TestCase {
	private Model model = TestRDFModel.getInstance().getWorkflowModel();

	private Resource resource = model
			.getResource(TestRDFModel.WORKFLOW_PART_RT_CONTEX_1);

	public void testWorkflowPartRTContext() {
		WorkflowPartRTContext rtContext = new WorkflowPartRTContext(resource);
		assertEquals(new WorkflowData(model
				.getResource(TestRDFModel.DATA_MOD_1)), rtContext
				.getProcessedWorkflowData());
		assertEquals(
				new WorkflowImpl(model.getResource(TestRDFModel.WORKFLOW1)),
				rtContext.getExecutingWorkflowPart());

	}
}
