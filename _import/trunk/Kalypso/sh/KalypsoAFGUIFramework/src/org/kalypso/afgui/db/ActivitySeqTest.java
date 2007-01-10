package org.kalypso.afgui.db;

import junit.framework.TestCase;

import test.org.kalypso.afgui.TestRDFModel;

import com.hp.hpl.jena.rdf.model.Model;
import com.hp.hpl.jena.rdf.model.Seq;

/**
 * @author Patrice Congo
 * 
 */
public class ActivitySeqTest extends TestCase {
	Model model = TestRDFModel.getInstance().getWorkflowModel();

	Seq seqRes = model.getSeq(TestRDFModel.ACTIVITIES_1_2_URI);

	public void testCreation() {
		ActivitySeq activitySeq = new ActivitySeq((Seq) seqRes);
		assertEquals("Only activity 1 and 2 in sequence", 2, activitySeq.size());
		assertEquals("First activity in the sequence must be Activity1",
				TestRDFModel.ACTIVITY1_ID, activitySeq.get(0).getURI());
		assertEquals("Second activity in the sequence must be Activity2",
				TestRDFModel.ACTIVITY2_ID, activitySeq.get(1).getURI());
	}
}
