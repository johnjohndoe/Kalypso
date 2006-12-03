package test.org.kalypso.afgui;

import java.util.Arrays;
import java.util.Collections;
import java.util.List;

import org.kalypso.afgui.db.EWorkflowProperty;
import org.kalypso.afgui.model.IWorkflowData;
import org.kalypso.afgui.model.impl.WorkflowData;

import com.hp.hpl.jena.query.util.CollectionUtils;
import com.hp.hpl.jena.rdf.model.Model;
import com.hp.hpl.jena.rdf.model.Resource;

import junit.framework.TestCase;

/**
 * @author Patrice Congo
 *
 */
public class WorkflowDataTest extends TestCase
{
	private Model model=TestRDFModel.getInstance().getWorkflowModel();
	
	
	private Resource resource= 
			model.getResource(TestRDFModel.DATA_MOD_1);
	
	public void testWorkflowData()
	{
		WorkflowData simMo1=new WorkflowData(resource);
		assertEquals(TestRDFModel.DATA_MOD_1_NAME, simMo1.getName());
		assertEquals("type_simulationmodel",simMo1.getType());
		assertEquals("file.xml#SimMo1",simMo1.getLocation());
		assertTrue(simMo1.hasLinkedWorkflowData(EWorkflowProperty.CONTAINS));
		List<IWorkflowData> subs=
				simMo1.getLinkedWorkflowData(EWorkflowProperty.CONTAINS);
		assertEquals("2 sub models in the list size=2",2,subs.size());
//		boolean sub1Vs0=subs.get(0).getURI().equals(TestRDFModel.DATA_SUB_MOD_1_1);
//		boolean sub1Vs1=subs.get(1).getURI().equals(TestRDFModel.DATA_SUB_MOD_1_1);
//		boolean sub2Vs0=subs.get(0).getURI().equals(TestRDFModel.DATA_SUB_MOD_1_2);
//		boolean sub2Vs1=subs.get(1).getURI().equals(TestRDFModel.DATA_SUB_MOD_1_2);
		
		assertTrue(
				"sub 1 1 mus be contained in model1"+subs,
				subs.contains(
						new WorkflowData(
								model.getResource(TestRDFModel.DATA_SUB_MOD_1_1))));
		assertTrue(
				"sub 1 1 mus be contained in model1"+subs,
				subs.contains(
						new WorkflowData(
								model.getResource(TestRDFModel.DATA_SUB_MOD_1_2))));
		Resource mod1Dev= model.getResource(TestRDFModel.DATA_MOD_1_DEV);
		WorkflowData mod1DevData= new WorkflowData(mod1Dev);
		List<IWorkflowData> devParents=
			mod1DevData.getLinkedWorkflowData(EWorkflowProperty.IS_DERIVED_FROM);
		assertEquals(1,devParents.size());
		assertEquals(
				"mod1 dev ist derived from md 1",
				simMo1,
				devParents.get(0));
	}
}
