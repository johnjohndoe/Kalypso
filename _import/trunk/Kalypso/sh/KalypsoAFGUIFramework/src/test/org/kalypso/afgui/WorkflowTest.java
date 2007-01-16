package test.org.kalypso.afgui;

import java.util.List;

import org.kalypso.afgui.model.IPhase;
import org.kalypso.afgui.model.impl.Phase;
import org.kalypso.afgui.model.impl.WorkflowImpl;

import com.hp.hpl.jena.rdf.model.Model;
import com.hp.hpl.jena.rdf.model.Resource;

import junit.framework.TestCase;

/**
 * @author Patrice Congo
 *
 */
public class WorkflowTest extends TestCase
{
	private Model model=TestRDFModel.getInstance().getWorkflowModel();
	private Resource wfRes=
		model.getSeq(TestRDFModel.WORKFLOW1);
	
	public void testWorkflow()
	{
		WorkflowImpl wf= new WorkflowImpl(wfRes);
		List<IPhase> phases= wf.getPhases();
		assertEquals(2,phases.size());
		assertEquals(
				new Phase(
						model.getResource(TestRDFModel.PHASE1)), 
				phases.get(0));
		assertEquals(
				new Phase(
						model.getResource(TestRDFModel.PHASE2)), 
				phases.get(1));
	}
}
