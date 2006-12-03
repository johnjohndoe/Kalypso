package test.org.kalypso.afgui;

import org.kalypso.afgui.model.impl.SubTaskGroup;
import org.kalypso.afgui.model.impl.SubTaskGroupSeq;

import com.hp.hpl.jena.rdf.model.Model;
import com.hp.hpl.jena.rdf.model.Resource;
import com.hp.hpl.jena.rdf.model.Seq;

import junit.framework.TestCase;

/**
 * @author Patrice Congo
 *
 */
public class SubTaskGroupSeqTest extends TestCase
{
	private Model model=TestRDFModel.getInstance().getWorkflowModel();
	
	private Seq seq= 
			model.getSeq(TestRDFModel.SUBTASKGROUP_1_2);
	
	public void testSubTaskGroupSeq()
	{
		SubTaskGroupSeq stgSeq= new SubTaskGroupSeq(seq);
		assertEquals(2,stgSeq.size());
		assertEquals(
				new SubTaskGroup(model.getResource(TestRDFModel.SUBTASKGROUP1)),
				stgSeq.get(0));
		assertEquals(
				new SubTaskGroup(model.getResource(TestRDFModel.SUBTASKGROUP2)),
				stgSeq.get(1));
	}
}
