package test.org.kalypso.afgui;

import java.io.InputStream;

import com.hp.hpl.jena.rdf.model.Model;
import com.hp.hpl.jena.rdf.model.ModelFactory;

public class TestRDFModel
{
	final static private String TEST_NS=
		"http://www.tu-harburg.de/wb/kalypso/kb/workflow/test#";
	
	final static public String ACTIVITY1_ID=TEST_NS+"Activity1";
	
	final static public String ACTIVITY1_NAME="Activity1";
	final static public String ACTIVITY1_HELP="ActivityHelp1";
	final static public String ACTIVITY2_ID=TEST_NS+"Activity2";
	
	final static public String ACTIVITIES_1_2_URI=TEST_NS+"Activities_1_2";
	
	final static public String DATA_MOD_1=TEST_NS+"SimMo1";
	final static public String DATA_MOD_1_NAME="SimulationModel1";
	final static public String DATA_MOD_1_DEV=TEST_NS+"SimMo1Dev";
	final static public String DATA_SUB_MOD_1_1=TEST_NS+"SubMo1_1";
	final static public String DATA_SUB_MOD_1_2=TEST_NS+"SubMo1_2";
	
	final static public String TEST_TASK_RDF="test_task_rdf.xml";
	final static public String TEST_TASK_RDF_SH="test_task_sh.xml";
	
	
	private static final TestRDFModel models=new TestRDFModel();

	public static final String TASK1 = TEST_NS+"Task1";
	public static final String TASK2 = TEST_NS+"Task2";
	public static final String TASK_1_2 = TEST_NS+"Task_1_2";
	
	public static final String SUBTASKGROUP1 = TEST_NS+"SubTaskGroup1";
	public static final String SUBTASKGROUP2 = TEST_NS+"SubTaskGroup2";
	
	public static final String SUBTASKGROUP_1_2 = TEST_NS+"SubTaskGroup_1_2";
	
	public static final String TASKGROUP1 = TEST_NS+"TaskGroup1";
	public static final String TASKGROUP2 = TEST_NS+"TaskGroup2";
	public static final String TASKGROUP_1_2 = TEST_NS+"TaskGroup_1_2";
	
	public static final String PHASE1 = TEST_NS+"Phase1";
	public static final String PHASE2 = TEST_NS+"Phase2";
	public static final String PHASE_1_2 = TEST_NS+"Phase_1_2";
	
	public static final String WORKFLOW1 = TEST_NS+"Workflow1";
	public static final String WORKFLOW_SH = TEST_NS+"WF_Kalypso1D2D";
	
	public static final String WORKFLOW_PART_RT_CONTEX_1 = TEST_NS+"RTSimMo1";
	
	private Model workflowModel;

	final  private Model shModel;
	
	 
	
	private TestRDFModel()
	{
		InputStream iStream=
				TestRDFModel.class.getResourceAsStream(TEST_TASK_RDF);
		workflowModel=ModelFactory.createDefaultModel();
		workflowModel.read(iStream, null);
		
		//
		iStream=
			TestRDFModel.class.getResourceAsStream(TEST_TASK_RDF_SH);
		shModel=ModelFactory.createDefaultModel();
		shModel.read(iStream, null);
	}
	
	static public TestRDFModel getInstance()
	{
		return models;
	}
	
	public Model getWorkflowModel()
	{
		return workflowModel;
	}
	
	public Model getShModel()
	{
		return shModel;
	}
	
	static public void main(String[] d)
	{
		System.out.println(models.getWorkflowModel());
	}
}
