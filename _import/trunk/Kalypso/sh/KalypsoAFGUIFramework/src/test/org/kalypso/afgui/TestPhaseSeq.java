package test.org.kalypso.afgui;

import org.kalypso.afgui.model.IPhase;
import org.kalypso.afgui.model.impl.Phase;
import org.kalypso.afgui.model.impl.WorkflowPartSeq;

import com.hp.hpl.jena.rdf.model.Model;
import com.hp.hpl.jena.rdf.model.Seq;

import junit.framework.TestCase;

/**
 * @author Patrice Congo
 *
 */
public class TestPhaseSeq extends TestCase
{
	private Model model=
		TestRDFModel.getInstance().getWorkflowModel();

	private Seq pRes=
			model.getSeq(TestRDFModel.PHASE_1_2);
	
	public void testPhaseSeq()
	{
		WorkflowPartSeq<IPhase> pSeq=new WorkflowPartSeq<IPhase>(pRes,IPhase.class);
		assertEquals(2,pSeq.size());
		assertEquals(
				new Phase(model.getResource(TestRDFModel.PHASE1)), 
				pSeq.get(0));
		assertEquals(
				new Phase(model.getResource(TestRDFModel.PHASE2)), 
				pSeq.get(1));
				
	}
	
}
