package test.org.kalypso.afgui;

import org.kalypso.afgui.model.impl.Phase;
import org.kalypso.afgui.model.impl.TaskGroupSeq;

import com.hp.hpl.jena.rdf.model.Model;
import com.hp.hpl.jena.rdf.model.Resource;

import junit.framework.TestCase;

/**
 * @author Patrice Congo
 *
 */
public class PhaseTest extends TestCase
{
	private Model model=
		TestRDFModel.getInstance().getWorkflowModel();

	private Resource pRes=
			model.getResource(TestRDFModel.PHASE1);
	
	public void testPhase()
	{
		Phase p= new Phase(pRes);
		assertEquals(
				new TaskGroupSeq(model.getSeq(TestRDFModel.TASKGROUP_1_2)), 
				p.getTaskGroups());
	}
}
