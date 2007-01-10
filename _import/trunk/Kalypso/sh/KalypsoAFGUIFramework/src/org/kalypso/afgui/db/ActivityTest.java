package org.kalypso.afgui.db;

import junit.framework.TestCase;

import org.kalypso.afgui.model.IActivity;
import org.kalypso.afgui.model.IHelp;
import org.kalypso.afgui.model.internal.Activity;

import test.org.kalypso.afgui.TestRDFModel;

import com.hp.hpl.jena.rdf.model.Model;
import com.hp.hpl.jena.rdf.model.Resource;

public class ActivityTest extends TestCase {

	private Model model = TestRDFModel.getInstance().getWorkflowModel();

	private Resource resource = model.getResource(TestRDFModel.ACTIVITY1_ID);

	public void testActivity() {
		assertNotNull(model.toString(), resource);
		IActivity a = new Activity(resource);
		assertEquals("Test for actity name fails", TestRDFModel.ACTIVITY1_NAME,
				a.getName());

		IHelp help = a.getHelp();
		assertEquals("Test for actity help test ", TestRDFModel.ACTIVITY1_HELP,
				help.getHelp());
	}
}
