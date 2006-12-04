package org.kalypso.afgui.schema;

import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.PrintStream;
import java.lang.reflect.Constructor;
import java.net.URI;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.EnumMap;
import java.util.HashMap;
import java.util.Hashtable;
import java.util.List;
import java.util.Map;

import org.apache.log4j.Logger;
import org.kalypso.afgui.db.EWorkflowProperty;
import org.kalypso.afgui.model.EActivityExeState;
import org.kalypso.afgui.model.EActivityRelationship;
import org.kalypso.afgui.model.IActivity;
import org.kalypso.afgui.model.IHelp;
import org.kalypso.afgui.model.IPhase;
import org.kalypso.afgui.model.ISubTaskGroup;
import org.kalypso.afgui.model.ITask;
import org.kalypso.afgui.model.ITaskGroup;
import org.kalypso.afgui.model.IWorkflow;
import org.kalypso.afgui.model.IWorkflowConcept;
import org.kalypso.afgui.model.IWorkflowData;
import org.kalypso.afgui.model.IWorkflowPart;
import org.kalypso.afgui.model.IHelp.HELP_TYPE;
import org.kalypso.afgui.model.impl.Activity;
import org.kalypso.afgui.model.impl.ActivitySeq;
import org.kalypso.afgui.model.impl.Help;
import org.kalypso.afgui.model.impl.Phase;
import org.kalypso.afgui.model.impl.SubTaskGroup;
import org.kalypso.afgui.model.impl.SubTaskGroupSeq;
import org.kalypso.afgui.model.impl.Task;
import org.kalypso.afgui.model.impl.TaskGroup;
import org.kalypso.afgui.model.impl.TaskGroupSeq;
import org.kalypso.afgui.model.impl.TaskSeq;
import org.kalypso.afgui.model.impl.WorkflowData;
import org.kalypso.afgui.model.impl.WorkflowImpl;
import org.kalypso.afgui.model.impl.WorkflowPartSeq;


import com.hp.hpl.jena.rdf.model.Literal;
import com.hp.hpl.jena.rdf.model.Model;
import com.hp.hpl.jena.rdf.model.ModelFactory;
import com.hp.hpl.jena.rdf.model.Property;
import com.hp.hpl.jena.rdf.model.RDFNode;
import com.hp.hpl.jena.rdf.model.ResIterator;
import com.hp.hpl.jena.rdf.model.Resource;
import com.hp.hpl.jena.rdf.model.Seq;
import com.hp.hpl.jena.rdf.model.Statement;
import com.hp.hpl.jena.vocabulary.RDF;

/**
 * 
 * A utility java representation of the workflow schema. 
 * It contains a jena model for the schema, and the element declared
 * in the schema.
 * 
 * The vocabulary elements, resources and properties, are all static final field.
 * If their initialization fails because of an exception, this exception is store in
 * {@link #PROBLEM_FLAG} are all element are consequently set to null.
 * 
 * @author Patrice Congo
 *
 */
final public class Schema
{
	
	interface IResourceWrapperConstructor
	{
		public IWorkflowConcept construct(Object resource);
	};
	
	static final private Logger logger=
						Logger.getLogger(Schema.class);
	public static final String SCHEMA_NS=
		"http://www.tu-harburg.de/wb/kalypso/schema/workflow#";
	
	final static public String WORKFLOW_SCHEMA_FOLDER="res";
	final static public String WORKFLOW_SCHEMA_FILE="res/workflow_schema.xml";
	final static public Model schemaModel=ModelFactory.createDefaultModel();

	final static public  Property PROP_HAS_NAME;
	final static public  String URI_PROP_HAS_NAME=SCHEMA_NS+"hasName";
	
	final static public  Property PROP_EXE_STATE;
	final static public  String URI_PROP_EXE_STATE=SCHEMA_NS+"exeState";
	
	final static public  Property PROP_PART_OF;
	final static public  String URI_PROP_PART_OF=SCHEMA_NS+"partOf";
	
	final static public  Property PROP_DEPENDS_ON;
	final static public  String URI_PROP_DEPENDS_ON=SCHEMA_NS+"dependsOn";
	
	final static public  Property PROP_HAS_ACTIVITY;
	final static public  String URI_PROP_HAS_ACTIVITY=SCHEMA_NS+"hasActivity";
	
	final static public String URI_PROP_IS_ROOT = SCHEMA_NS+"isRoot";
	final static public Property PROP_IS_ROOT;
	
	final static public String URI_PROP_HAS_A = SCHEMA_NS+"hasA";
	final static public Property PROP_HAS_A;
	
	final static public String URI_PROP_FOLLOWS = SCHEMA_NS+"follows";
	final static public Property PROP_FOLLOWS;
	
	final static public String URI_PROP_HAS_HELP = SCHEMA_NS+"hasHelp";
	final static public Property PROP_HAS_HELP; 
	
	final static public String URI_CLASS_ACTIVITY=SCHEMA_NS+"Activity";
	final static public Resource CLASS_ACTIVITY;
	final static public IResourceWrapperConstructor CONSTRUCTOR_ACTIVITY=
		new IResourceWrapperConstructor()
	{
		public IWorkflowConcept construct(Object resource)
		{
			return new Activity((Resource)resource);
		}
	};
	
	final static public String URI_CLASS_WORKFLOW=SCHEMA_NS+"Workflow";
	final static public Resource CLASS_WORKFLOW;
	final static public IResourceWrapperConstructor CONSTRUCTOR_WORKFLOW= 
		new IResourceWrapperConstructor()
	{

		public IWorkflowConcept construct(Object resource)
		{
			logger.info("WF_RES="+resource);
			return new WorkflowImpl((Resource)resource); 
		}	
		
		@Override
		public String toString()
		{
			return "Creator for Workflow";
		}
	};
	
	final static public String URI_CLASS_HELP=SCHEMA_NS+"Help";
	final static public Resource CLASS_HELP;
	final static public IResourceWrapperConstructor CONSTRUCTOR_HELP= 
		new IResourceWrapperConstructor()
	{

		public IWorkflowConcept construct(Object resource)
		{
			return null;//new Help(resource); 
		}	
	};
	
	final static public String URI_CLASS_TASK_SEQ=SCHEMA_NS+"TaskSeq";
	final static public Resource CLASS_TASK_SEQ;
	final static public IResourceWrapperConstructor CONSTRUCTOR_TASK_SEQ= 
		new IResourceWrapperConstructor()
	{

		public IWorkflowConcept construct(Object resource)
		{
			return new TaskSeq((Seq)resource);//new Help(resource); 
		}	
	};
	
	
	
	final static public String URI_CLASS_WORKFLOW_STATUS=SCHEMA_NS+"WorkflowStatus";
	final static public Resource CLASS_WORKFLOW_STATUS=null;
	
	final static public String URI_CLASS_ACTIVITY_STATUS=SCHEMA_NS+"ActivityStatus";
	final static public Resource CLASS_ACTIVITY_STATUS=null;
	
	//
	final static public String URI_CLASS_WORKFLOW_DATA=SCHEMA_NS+"WorkflowData";
	final static public Resource CLASS_WORKFLOW_DATA;
	final static public IResourceWrapperConstructor CONSTRUCTOR_WORKFLOW_DATA= 
		new IResourceWrapperConstructor()
	{

		public IWorkflowConcept construct(Object resource)
		{
			return new WorkflowData((Resource)resource); 
		}	
	};
	
	//Task
	final static public String URI_CLASS_TASK=SCHEMA_NS+"Task";
	final static public Resource CLASS_TASK;
	final static public IResourceWrapperConstructor CONSTRUCTOR_TASK= 
		new IResourceWrapperConstructor()
	{

		public IWorkflowConcept construct(Object resource)
		{
			return new Task((Resource)resource); 
		}	
	};
	
	//TaskGroup
	final static public String URI_CLASS_TASK_GROUP=SCHEMA_NS+"TaskGroup";
	final static public Resource CLASS_TASK_GROUP=null;
	final static public IResourceWrapperConstructor CONSTRUCTOR_TASK_GROUP= 
		new IResourceWrapperConstructor()
	{

		public IWorkflowConcept construct(Object resource)
		{
			return new TaskGroup((Resource)resource); 
		}	
	};

	//SubTaskGroup
	final static public String URI_CLASS_SUB_TASK_GROUP=SCHEMA_NS+"SubTaskGroup";
	final static public Resource CLASS_SUB_TASK_GROUP=null;
	final static public IResourceWrapperConstructor CONSTRUCTOR_SUB_TASK_GROUP= 
		new IResourceWrapperConstructor()
	{

		public IWorkflowConcept construct(Object resource)
		{
			return new SubTaskGroup((Resource)resource); 
		}	
	};
	
	//	SubTaskGroup
	final static public String URI_CLASS_PHASE=SCHEMA_NS+"Phase";
	final static public Resource CLASS_PHASE=null;
	final static public IResourceWrapperConstructor CONSTRUCTOR_PHASE= 
		new IResourceWrapperConstructor()
	{

		public IWorkflowConcept construct(Object resource)
		{
			return new Phase((Resource)resource); 
		}	
	};
	
	//hasType
	final static public String URI_PROP_HAS_TYPE = SCHEMA_NS+"hasType";
	final static public Property PROP_HAS_TYPE; 
	
	//ActivitySeq
	final static public String URI_CLASS_ACTIVITY_SEQ=SCHEMA_NS+"ActvitySeq";
	final static public Resource CLASS_ACTIVITY_SEQ;
	final static public IResourceWrapperConstructor CONSTRUCTOR_ACTIVITY_SEQ= 
		new IResourceWrapperConstructor()
	{

		public IWorkflowConcept construct(Object resource)
		{
			return new ActivitySeq((Resource)resource); 
		}	
	};
	
	
	//hasActivities
	final static public String URI_PROP_HAS_ACTIVITIES = SCHEMA_NS+"hasActivities";
	final static public Property PROP_HAS_ACTIVITIES; 
	
	//hasTasks
	final static public String URI_PROP_HAS_TASKS = SCHEMA_NS+"hasTasks";
	final static public Property PROP_HAS_TASKS; 
	 
	//hasSubTaskGroups PROP_HAS_SUB_TASK_GROUPS
	final static public String URI_PROP_HAS_SUB_TASK_GROUPS = SCHEMA_NS+"hasSubTaskGroups";
	final static public Property PROP_HAS_SUB_TASK_GROUPS;
	
	//hasTaskGroups
	final static public String URI_PROP_HAS_TASK_GROUPS = SCHEMA_NS+"hasTaskGroups";
	final static public Property PROP_HAS_TASK_GROUPS;
	
	//hasPhases PROP_HAS_PHASES
	final static public String URI_PROP_HAS_PHASES = SCHEMA_NS+"hasPhases";
	final static public Property PROP_HAS_PHASES;
	
	
	final static public List<Property> ACTIVITY_LINK_PROPS;
	
	final static public Map<Property,
							EActivityRelationship> REL_STATEMENT_PROP_MAP=null;
	
	final static public Map<Object, IResourceWrapperConstructor> RT_CLASS_MAP;
	
	final static private EnumMap<EWorkflowProperty,Property> toPropertyMap;
	
	final static public String URI_PROP_HAS_LOCATION = SCHEMA_NS+"hasLocation";
	
	final static public String URI_PROP_IS_DERIVED_FROM = SCHEMA_NS+"isDerivedFrom";
	final static public Property PROP_IS_DERIVED_FROM;
	//executedWorkflowPart
	final static public String URI_PROP_EXECUTING_WORKFLOW_PART = SCHEMA_NS+"executingWorkflowPart";
	final static public Property PROP_EXECUTING_WORKFLOW_PART;
	
	//processedResource
	final static public String URI_PROP_PROCESSED_RES = SCHEMA_NS+"processedResource";
	final static public Property PROP_PROCESSED_RES;
	
	//hasSubRTContext
	final static public String URI_PROP_HAS_SUB_RT_CONTEXT = 
											SCHEMA_NS+"hasSubRTContext";
	final static public Property PROP_HAS_SUB_RT_CONTEXT;
	
	final static public String URI_PROP_IS_WORKS_ON = SCHEMA_NS+"worksOn";
	
	final static public String URI_PROP_IS_CONTAINS = SCHEMA_NS+"contains";
	
	
	/**
	 * Holds the throwable which describt a failure while initialising 
	 * the class static fields.
	 * A value null signal an error free initialisation
	 */
	static final public Throwable PROBLEM_FLAG;
	final  static public String PJT_NS="pjtNS";

	
	private Schema()
	{
		//empty
	}
	
	static
	{
		Map<String, Property> propMap= new HashMap<String, Property>();
		Map<String, Resource> resMap=  new HashMap<String, Resource>();
		Throwable backupTh=null; 
		try{
			//load model
//			File resFolder=
//				new File(Schema.class.getResource(WORKFLOW_SCHEMA_FOLDER).getPath());
			
			InputStream iStream=
				Schema.class.getResourceAsStream(WORKFLOW_SCHEMA_FILE);//new FileInputStream(new File(resFolder,WORKFLOW_SCHEMA_FILE));
			schemaModel.read(iStream,"");
			String propUris[]={
					URI_PROP_IS_ROOT, URI_PROP_HAS_ACTIVITY,URI_PROP_HAS_NAME,
					URI_PROP_EXE_STATE, URI_PROP_PART_OF,URI_PROP_HAS_A,
					URI_PROP_DEPENDS_ON, URI_PROP_FOLLOWS,URI_PROP_HAS_HELP,
					URI_PROP_HAS_TYPE,URI_PROP_HAS_ACTIVITIES, 
					URI_PROP_EXECUTING_WORKFLOW_PART, URI_PROP_PROCESSED_RES,
					URI_PROP_HAS_SUB_RT_CONTEXT, URI_PROP_HAS_TASKS,
					URI_PROP_HAS_SUB_TASK_GROUPS, URI_PROP_HAS_TASK_GROUPS,
					URI_PROP_HAS_PHASES};
			
			String resUris[]={
					URI_CLASS_ACTIVITY,URI_CLASS_WORKFLOW,URI_CLASS_HELP,
					URI_CLASS_PHASE,URI_CLASS_WORKFLOW_STATUS,
					URI_CLASS_ACTIVITY_STATUS,URI_CLASS_WORKFLOW_DATA,
					URI_CLASS_ACTIVITY_SEQ,URI_CLASS_TASK,
					URI_CLASS_TASK_SEQ};
			
			//find vokabulary elements
			fillPropMap(propMap, propUris);
			fillResMap(resMap, resUris);
		}
		catch(Throwable th)
		{
			//remove all sothat vocabulary element will be init with null
			logger.error("Loading fails", th);
			propMap.clear();
			resMap.clear();
			backupTh=th;
		}
		finally
		{
			//init vokabulary element
			PROBLEM_FLAG=backupTh;
			PROP_HAS_NAME=propMap.get(URI_PROP_HAS_NAME);
			PROP_IS_ROOT=propMap.get(URI_PROP_IS_ROOT);
			PROP_HAS_ACTIVITY=propMap.get(URI_PROP_HAS_ACTIVITY);
			PROP_HAS_A=propMap.get(URI_PROP_HAS_A);
			PROP_PART_OF=propMap.get(URI_PROP_PART_OF);
			PROP_EXE_STATE=propMap.get(URI_PROP_EXE_STATE);
			PROP_DEPENDS_ON=propMap.get(URI_PROP_DEPENDS_ON);
			PROP_FOLLOWS=propMap.get(URI_PROP_FOLLOWS);
			PROP_HAS_HELP=propMap.get(URI_PROP_HAS_HELP);
			PROP_HAS_TYPE=propMap.get(URI_PROP_HAS_TYPE);
			PROP_IS_DERIVED_FROM=propMap.get(URI_PROP_IS_DERIVED_FROM);
			PROP_HAS_ACTIVITIES=propMap.get(URI_PROP_HAS_ACTIVITIES);
			PROP_EXECUTING_WORKFLOW_PART=propMap.get(URI_PROP_EXECUTING_WORKFLOW_PART);
			PROP_PROCESSED_RES=propMap.get(URI_PROP_PROCESSED_RES);
			PROP_HAS_SUB_RT_CONTEXT=propMap.get(URI_PROP_HAS_SUB_RT_CONTEXT);
			PROP_HAS_TASKS=propMap.get(URI_PROP_HAS_TASKS);
			PROP_HAS_SUB_TASK_GROUPS=propMap.get(URI_PROP_HAS_SUB_TASK_GROUPS);
			PROP_HAS_TASK_GROUPS=propMap.get(URI_PROP_HAS_TASK_GROUPS);
			PROP_HAS_PHASES=propMap.get(URI_PROP_HAS_PHASES);
			Property tempProps[]= {
									PROP_HAS_A, PROP_DEPENDS_ON, PROP_PART_OF};
			ACTIVITY_LINK_PROPS=
					Collections.unmodifiableList(
								Arrays.asList(tempProps));
			Object[][] propRelStatePair=
				{	{PROP_HAS_A,EActivityRelationship.HAS_A},
					{PROP_DEPENDS_ON,EActivityRelationship.DEPENDS_ON},
					{PROP_PART_OF,EActivityRelationship.PART_OF}};
			
			Map<Property, EActivityRelationship> tempRelPropMap= 
				new HashMap<Property, EActivityRelationship>(propRelStatePair.length);
			
			for(Object[] pair:propRelStatePair)
			{
				tempRelPropMap.put(
						(Property)pair[0], 
						(EActivityRelationship)pair[1]);
			}
			
			
			toPropertyMap= computeEWorkflowPropToJena();
			
			
			
//			REL_STATEMENT_PROP_MAP=Collections.unmodifiableMap(tempRelPropMap);
//			logger.info(REL_STATEMENT_PROP_MAP);
//			
//			CLASS_WORKFLOW=resMap.get(URI_CLASS_WORKFLOW);
//			CLASS_PHASE=resMap.get(URI_CLASS_PHASE);
//			CLASS_ACTIVITY_STATUS= resMap.get(URI_CLASS_ACTIVITY_STATUS);
			CLASS_WORKFLOW_DATA=resMap.get(URI_CLASS_WORKFLOW_DATA);
			CLASS_ACTIVITY_SEQ=resMap.get(URI_CLASS_ACTIVITY_SEQ);
			CLASS_TASK=resMap.get(URI_CLASS_TASK);
			CLASS_ACTIVITY=resMap.get(URI_CLASS_ACTIVITY);
			CLASS_HELP=resMap.get(URI_CLASS_HELP);
			CLASS_TASK_SEQ=resMap.get(URI_CLASS_TASK_SEQ);
			CLASS_WORKFLOW= resMap.get(URI_CLASS_WORKFLOW);
			
			RT_CLASS_MAP=computeRTClassMap();
		}
		
	}
	
	private static final EnumMap<EWorkflowProperty, Property> computeEWorkflowPropToJena()
	{
		Object[][] eWorklflowPropToJena=
		{	
			{	EWorkflowProperty.HAS_LOCATION,	URI_PROP_HAS_LOCATION},
			{	EWorkflowProperty.HAS_TYPE,	URI_PROP_HAS_TYPE},
			{	EWorkflowProperty.IS_DERIVED_FROM,	URI_PROP_IS_DERIVED_FROM},
			{	EWorkflowProperty.CONTAINS,	URI_PROP_IS_CONTAINS},};
	
		EnumMap<EWorkflowProperty, Property> toPropertyMap= 
				new EnumMap<EWorkflowProperty, Property>(EWorkflowProperty.class);
		
		for(Object[] pair:eWorklflowPropToJena)
		{
//			logger.info("pair0="+(EWorkflowProperty)pair[0]+
//						" pair1="+pair[1].getClass());
			toPropertyMap.put(
					(EWorkflowProperty) pair[0], 
					(Property)schemaModel.getProperty((String)pair[1]));
		}
		
		return toPropertyMap;
	}
	
	private static Map<Object, IResourceWrapperConstructor> computeRTClassMap()
	{
		Object[][] eWorklflowPropToJena=
		{	
			{	IHelp.class,	CONSTRUCTOR_HELP},
			{	ITask.class,	CONSTRUCTOR_TASK},
			{	IActivity.class,	CONSTRUCTOR_ACTIVITY},
			{	ITaskGroup.class,	CONSTRUCTOR_TASK_GROUP},
			{	ISubTaskGroup.class,	CONSTRUCTOR_SUB_TASK_GROUP},
			{	IPhase.class,	CONSTRUCTOR_PHASE},
			{	IWorkflow.class,	CONSTRUCTOR_WORKFLOW},
			{ CLASS_WORKFLOW, CONSTRUCTOR_WORKFLOW}
			//{	,	CONSTRUCTOR_ACTIVITY_SEQ},
			};
	
		 Map<Object, IResourceWrapperConstructor> map= 
				new Hashtable<Object, IResourceWrapperConstructor>();
		
		for(Object[] pair:eWorklflowPropToJena)
		{
			map.put(
					pair[0], 
					(IResourceWrapperConstructor)pair[1]);
		}
		logger.info(map);
		return Collections.unmodifiableMap(map);
	}

	private static final void fillPropMap(
									Map<String, 
									Property> propMap, 
									String[] uris)
	{
		Property curProp;
		for(String uri:uris)
		{
			curProp=schemaModel.getProperty(uri);
			if(curProp==null)
			{
				throw new RuntimeException("No Prop for "+uri);
			}
			propMap.put(uri, curProp);
		}
	}
	
	private static final void fillResMap(Map<String, Resource> propMap, String[] uris)
	{
		Resource curRes;
		for(String uri:uris)
		{
			curRes=schemaModel.getResource(uri);
			if(curRes==null)
			{
				throw new RuntimeException("No res for "+uri);
			}
			propMap.put(uri, curRes);
		}
	}
	
	static public boolean isActivityLinkStatement(Property predicate)
	{
		if(predicate==null)
		{
			return false;
		}
		else
		{
			return ACTIVITY_LINK_PROPS.contains(predicate);
		}
	}
	
	
	static public Property toJenaProperty(EWorkflowProperty prop)
	{
		return toPropertyMap.get(prop);
	}
	
	static public void main(String[] args) throws IOException
	{
		System.out.println("------------------------------");
		FileOutputStream out= new FileOutputStream("G:\\test.txt");
		PrintStream pOut= System.out;//new PrintStream(out);
		//System.setOut(pOut);
//		System.out.println(Schema.CLASS_ACTIVITY);
		pOut.println("Exception="+Schema.PROBLEM_FLAG);
		pOut.println("hasName="+Schema.PROP_HAS_NAME);
		pOut.println("hasActivity="+PROP_HAS_ACTIVITY);
		pOut.println("isRoot="+PROP_IS_ROOT);
		pOut.println("partOf="+PROP_PART_OF);
		pOut.println("exestate="+PROP_EXE_STATE);
		pOut.println("workflow="+CLASS_WORKFLOW);
		pOut.println("activity="+CLASS_ACTIVITY);
		pOut.println("Phase="+CLASS_PHASE);
		//pOut.println("schemaModel="+schemaModel);
		
		//schemaModel.write(pOut);
//		Schema.PROBLEM_FLAG.printStackTrace(pOut);
		//pOut.println(Schema.class.getResource(WORKFLOW_SCHEMA_FILE));
		//pOut.println(Schema.class.getResource("res/workflow_schema.rdfs"));
	}

	final  static public EActivityRelationship toEActivityRelationship(Property prop)
	{
		return REL_STATEMENT_PROP_MAP.get(prop);
	}
	
	final static public RDFNode getProperty(
										Resource resource,
										EWorkflowProperty prop)
	{
		Statement stm= resource.getProperty(toJenaProperty(prop));
		if(stm==null)
		{
			return null;
		}
		else
		{
			return stm.getObject();
		}
	}
	
	final static public String getStringProperty(
									Resource resource,
									EWorkflowProperty prop)
	{
		Statement stm= resource.getProperty(toJenaProperty(prop));
		if(stm==null)
		{
			return null;
		}
		else
		{
			RDFNode obj= stm.getObject();
			if(obj.isLiteral())
			{
				return ((Literal)obj).getString();
			}
			else
			{
				throw new RuntimeException("Node not a literal:"+stm);
			}
		}
	}
	
	final  static public void createStatement(
			Model model,
			IWorkflowData subject, 
			IWorkflowData object, 
			EWorkflowProperty prop)
	{
		model.add(
				(Resource)subject.getModelObject(),
				toJenaProperty(prop) , 
				(Resource)object.getModelObject());
	}
	
	final  static public void removeStatement(
			Model model,
			IWorkflowData subject, 
			IWorkflowData object, 
			EWorkflowProperty prop)
	{
		model.removeAll(
				(Resource)subject.getModelObject(),
				toJenaProperty(prop) , 
				(Resource)object.getModelObject());
	}
	
	static final public List<IWorkflowData> getWorkflowDataByType(Model model,String type)
	{
		ResIterator it=model.listSubjectsWithProperty(
				toJenaProperty(EWorkflowProperty.HAS_TYPE), type);
		List<IWorkflowData> list= new ArrayList<IWorkflowData>();
		for(;it.hasNext();)
		{
			list.add(new WorkflowData(it.nextResource()));
		}
		return list;
	}
	
	final static public IWorkflowData getWorkflowDataById(Model model, String id)
	{
		Resource res=model.getResource(id);
		if(res!=null)
		{
				return new WorkflowData(res);
		}
		else
		{
			return null;
		}
	}
	
	final  static private String toURIString(Model model,String id)
	{
		logger.info("id0="+id);
		try
		{
			URI uri=new URI(id);
			if(uri.getScheme()==null)
			{
				id=model.getNsPrefixURI(Schema.PJT_NS)+id;
			}
		}
		catch(Throwable th)
		{
			id=model.getNsPrefixURI(Schema.PJT_NS)+id;
		}
		logger.info("id1="+id);
		return id;
	}
	
	final static public IWorkflowData createWorkflowData(
			Model model,
			IWorkflowData parent, 
			String childId)
	{
		Resource res = 
				model.createResource(toURIString(model, childId),CLASS_WORKFLOW_DATA);
		
		
		res.addProperty(PROP_HAS_NAME, childId);
		if(parent!=null)
		{
			model.add(
					(Resource)parent.getModelObject(), 
					PROP_IS_DERIVED_FROM, 
					res);
		}
		return new WorkflowData(res);
	}
	
	final static public IWorkflowData derivedWorkflowData(
								Model model,
								IWorkflowData parent, 
								String childId)
	{
		Resource res=
			model.createResource(
					toURIString(model, childId),
					CLASS_WORKFLOW_DATA);
		model.createStatement(
					res, 
					toJenaProperty(EWorkflowProperty.IS_DERIVED_FROM), 
					(Resource)parent.getModelObject());
		return new WorkflowData(res);
	}
	
	final  static public List<IWorkflowData> getRootWorkflowDataByType(
													Model model,String type)
	{
		ResIterator it=model.listSubjectsWithProperty(
											RDF.type, 
											CLASS_WORKFLOW_DATA);
		logger.info("Iterator for type="+type+" "+it.hasNext());
		List<IWorkflowData> list= new ArrayList<IWorkflowData>();
		Resource res;
		//final Property PROP=toJenaProperty(EWorkflowProperty.HAS_TYPE);
		for(;it.hasNext();)
		{
			res=it.nextResource();
			if(!res.hasProperty(PROP_HAS_TYPE))
			{
				list.add(new WorkflowData(res));
			}
		}
		return list;
	}

	public static String getName(Resource resource)
	{
		logger.info("Getting name:"+resource);
		if(resource==null)
		{
			return null;
		}
		else
		{
			Statement stm=
				resource.getProperty(PROP_HAS_NAME);
			if(stm==null)
			{
				logger.warn("No name property:"+resource.getModel());
				return null;
			}
			else
			{
				return stm.getObject().toString();
			}
		}
	}

	public static IHelp getHelp(Resource resource)
	{
		if(resource==null)
		{
			return null;
		}
		else
		{
			Statement stm=resource.getProperty(PROP_HAS_HELP);
			if(stm==null)
			{
				return null;
			}
			else
			{
				
				RDFNode objectNode=stm.getObject();
				if(objectNode.isLiteral())
				{
					return new Help(objectNode.toString(), HELP_TYPE.PLAIN_TEXT);
				}
				else
				{
					//TODO extends help support
					throw new RuntimeException("Help Type not supported yet:"+objectNode);
				}
				
			}
		}
		
	}
	
	static public List<IActivity> getActivities(Resource resource)
	{
		if(resource==null)
		{
			logger.warn("Resource is null returning 0 size activity list");
			return Collections.emptyList();
		}
		else
		{
			Statement stm=resource.getProperty(PROP_HAS_ACTIVITIES);
			if(stm==null)
			{
				return Collections.emptyList();
			}
			else
			{
				try
				{
					Seq activities=stm.getSeq();
					return new ActivitySeq(activities);
				}
				catch(Throwable th)
				{
					logger.error(
							"No activities sequence found",th);
					return Collections.emptyList();
				}
			}
		}
	}

	static public Seq createActivitySeq(String uri, Model model)
	{
		//TODO implement create sactivity sequence
		return null;
	}

	static public EActivityExeState getExeState(Resource resource)
	{
		if(resource==null)
		{
			return EActivityExeState.UNKNOWN;
		}
		else
		{
			Statement stm=resource.getProperty(PROP_EXE_STATE);
			if(stm==null)
			{
				logger.warn("No exe state property");
				return EActivityExeState.UNKNOWN;
			}
			else
			{
				try
				{
					
					EActivityExeState state=EActivityExeState.valueOf(stm.getString());
					if(state==null)
					{
						state=EActivityExeState.UNKNOWN;
					}
					return state;
				}
				catch(Throwable th)
				{
					logger.warn("exe property not a literal", th);
					return EActivityExeState.UNKNOWN;
				}
			}
		}
		
	}

	public static IWorkflowData getProcessedWorkflowData(Resource resource)
	{
		if(resource==null)
		{
			throw new IllegalArgumentException();
		}
		else
		{
			Statement stm=resource.getProperty(PROP_PROCESSED_RES);
			if(stm==null)
			{
				logger.warn(
						"Context does not have processed resource:"+PROP_PROCESSED_RES+
						"\n"+resource);
				return null;
			}
			else
			{
				try
				{
					Resource wfData=stm.getResource();
					return new WorkflowData(wfData);
				}
				catch(Throwable th)
				{
					logger.error("Cannot processed workflow data", th);
					return null;
				}
			}
			
		}
	}

	public static IWorkflowPart getExecutingWorkflowPart(Resource resource)
	{
		if(resource==null)
		{
			return null;
		}
		else
		{
			Statement stm=resource.getProperty(PROP_EXECUTING_WORKFLOW_PART);
			if(stm==null)
			{
				logger.info("resource has no executing workflowpart:"+resource);
				return null;
			}
			else
			{
				try
				{
					Resource wfPart= stm.getResource(); 
					Statement typeStm=wfPart.getProperty(RDF.type);
					IResourceWrapperConstructor c=
						RT_CLASS_MAP.get(typeStm.getResource());
					//logger.info(CLASS_WORKFLOW.getURI().equals(typeStm.getResource().getURI()));
					logger.info(typeStm);
					logger.info(c);
					IWorkflowPart wfp= (IWorkflowPart)c.construct(wfPart);
					return wfp;
				}
				catch(Throwable th)
				{
					logger.error("Error while creating wrapper",th);
					return null;
				}
			}
		}
		
	}
	
	final  static public IWorkflowPart toWorkflowPart(Resource resource)
	{
		if(resource ==null)
		{
			return null;
		}
		else
		{
			Resource type=resource.getProperty(RDF.type).getResource();
			Class cls=null;
			try
			{
				Constructor c=cls.getConstructor(new Class[]{Resource.class});
				return (IWorkflowPart)c.newInstance(new Object[]{resource});
			}
			catch(Throwable th)
			{
				logger.error("Could not create wrapper class for:"+resource,th);
				return null;
			}
			
		}
	}
	
	final  static public <T extends IWorkflowPart> T asWorkflowPart(Resource resource, Class<T> cls)
	{
		if(resource ==null)
		{
			return null;
		}
		else
		{
			Resource type=resource.getProperty(RDF.type).getResource();
			
			try
			{
				//Constructor c=cls.getConstructor(new Class[]{Resource.class});
				IResourceWrapperConstructor constructor=RT_CLASS_MAP.get(cls);
				return (T)constructor.construct(resource);
			}
			catch(Throwable th)
			{
				logger.error("Could not create wrapper class for:"+resource,th);
				return null;
			}
			
		}
	}

	public static List<ITask> getTasks(Resource resource)
	{
		if(resource==null)
		{
			logger.warn("Resource is null returning 0 size activity list");
			return Collections.emptyList();
		}
		else
		{
			Statement stm=resource.getProperty(PROP_HAS_TASKS);
			if(stm==null)
			{
				return Collections.emptyList();
			}
			else
			{
				try
				{
					Seq tasks=stm.getSeq();
					return new TaskSeq(tasks);
				}
				catch(Throwable th)
				{
					logger.error(
							"No activities sequence found",th);
					return Collections.emptyList();
				}
			}
		}
	}

	public static List<ISubTaskGroup> getSubTaskGroups(Resource resource)
	{
		if(resource==null)
		{
			logger.warn("Resource is null returning 0 size activity list");
			return Collections.emptyList();
		}
		else
		{
			Statement stm=resource.getProperty(PROP_HAS_SUB_TASK_GROUPS);
			if(stm==null)
			{
				return Collections.emptyList();
			}
			else
			{
				try
				{
					Seq tasks=stm.getSeq();
					return new SubTaskGroupSeq(tasks);
				}
				catch(Throwable th)
				{
					logger.error(
							"No activities sequence found",th);
					return Collections.emptyList();
				}
			}
		}

	}

	public static List<ITaskGroup> getTaskGroups(Resource resource)
	{
		if(resource==null)
		{
			logger.warn("Resource is null returning 0 size activity list");
			return Collections.emptyList();
		}
		else
		{
			Statement stm=resource.getProperty(PROP_HAS_TASK_GROUPS);
			if(stm==null)
			{
				return Collections.emptyList();
			}
			else
			{
				try
				{
					Seq tg=stm.getSeq();
					return new TaskGroupSeq(tg);
				}
				catch(Throwable th)
				{
					logger.error(
							"No activities sequence found",th);
					return Collections.emptyList();
				}
			}
		}

	}

	public static List<IPhase> getPhase(Resource resource)
	{
		if(resource==null)
		{
			logger.warn("Resource is null returning 0 size activity list");
			return Collections.emptyList();
		}
		else
		{
			Statement stm=resource.getProperty(PROP_HAS_PHASES);
			if(stm==null)
			{
				return Collections.emptyList();
			}
			else
			{
				try
				{
					Seq tg=stm.getSeq();
					return new WorkflowPartSeq<IPhase>(tg,IPhase.class);
				}
				catch(Throwable th)
				{
					logger.error(
							"No activities sequence found",th);
					return Collections.emptyList();
				}
			}
		}
	}

}
