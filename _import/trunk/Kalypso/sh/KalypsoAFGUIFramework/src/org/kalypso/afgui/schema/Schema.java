package org.kalypso.afgui.schema;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.PrintStream;
import java.lang.reflect.Array;
import java.net.URL;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.EnumMap;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.apache.log4j.Logger;
import org.eclipse.ui.internal.dialogs.EmptyPropertyPage;
import org.kalypso.afgui.db.EWorkflowProperty;
import org.kalypso.afgui.model.EActivityRelationship;
import org.kalypso.afgui.model.IActivity;
import org.kalypso.afgui.model.IActivitySpecification;
import org.kalypso.afgui.model.IRelationshipStatement;
import org.kalypso.afgui.model.IWorkflowData;
import org.kalypso.afgui.model.impl.WorkflowData;


import com.hp.hpl.jena.rdf.model.Literal;
import com.hp.hpl.jena.rdf.model.Model;
import com.hp.hpl.jena.rdf.model.ModelFactory;
import com.hp.hpl.jena.rdf.model.Property;
import com.hp.hpl.jena.rdf.model.RDFNode;
import com.hp.hpl.jena.rdf.model.ResIterator;
import com.hp.hpl.jena.rdf.model.Resource;
import com.hp.hpl.jena.rdf.model.Statement;
import com.hp.hpl.jena.rdf.model.StmtIterator;
import com.hp.hpl.jena.vocabulary.RDF;
import com.hp.hpl.jena.vocabulary.RDFS;
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
	static final private Logger logger=
						Logger.getLogger(Schema.class);
	public static final String SCHEMA_NS=
		"http://www.tu-harburg.de/wb/kalypso/schema/workflow#";
	
	final static public String WORKFLOW_SCHEMA_FOLDER="res";
	final static public String WORKFLOW_SCHEMA_FILE="res/workflow_schema.xml";
	final static public Model schemaModel=ModelFactory.createDefaultModel();

	final static public  Property PROP_HAS_NAME;
	final static public  String URI_PROP_HAS_NAME=RDFS.label.getURI();//SCHEMA_NS+"hasName";
	
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
	final static public Resource CLASS_ACTIVITY=null;
	
	final static public String URI_CLASS_WORKFLOW=SCHEMA_NS+"WORKFLOW";
	final static public Resource CLASS_WORKFLOW=null;
	
	final static public String URI_CLASS_HELP=SCHEMA_NS+"Help";
	final static public Resource CLASS_HELP=null;
	
	final static public String URI_CLASS_PHASE=SCHEMA_NS+"Phase";
	final static public Resource CLASS_PHASE=null;
	
	final static public String URI_CLASS_WORKFLOW_STATUS=SCHEMA_NS+"WorkflowStatus";
	final static public Resource CLASS_WORKFLOW_STATUS=null;
	
	final static public String URI_CLASS_ACTIVITY_STATUS=SCHEMA_NS+"ActivityStatus";
	final static public Resource CLASS_ACTIVITY_STATUS=null;
	//
	final static public String URI_CLASS_WORKFLOW_DATA=SCHEMA_NS+"WorkflowData";
	final static public Resource CLASS_WORKFLOW_DATA=null;
	
	
	//hasType
	final static public String URI_PROP_HAS_TYPE = SCHEMA_NS+"hasType";
	final static public Property PROP_HAS_TYPE; 
	
	 
	
	final static public List<Property> ACTIVITY_LINK_PROPS;
	
	final static public Map<Property,
							EActivityRelationship> REL_STATEMENT_PROP_MAP=null;
	
	final static private EnumMap<EWorkflowProperty,Property> toPropertyMap;
	
	final static public String URI_PROP_HAS_LOCATION = SCHEMA_NS+"hasLocation";
	
	final static public String URI_PROP_IS_DERIVED_FROM = SCHEMA_NS+"isDerivedFrom";
	
	final static public String URI_PROP_IS_WORKS_ON = SCHEMA_NS+"worksOn";
	
	final static public String URI_PROP_IS_CONTAINS = SCHEMA_NS+"contains";
	
	
	/**
	 * Holds the throwable which describt a failure while initialising 
	 * the class static fields.
	 * A value null signal an error free initialisation
	 */
	static final public Throwable PROBLEM_FLAG;

	
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
					URI_PROP_HAS_TYPE};
			String resUris[]={
					URI_CLASS_ACTIVITY,URI_CLASS_WORKFLOW,URI_CLASS_HELP,
					URI_CLASS_PHASE,URI_CLASS_WORKFLOW_STATUS,
					URI_CLASS_ACTIVITY_STATUS,URI_CLASS_WORKFLOW_DATA};
			
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
//			CLASS_ACTIVITY=resMap.get(URI_CLASS_ACTIVITY);
//			CLASS_WORKFLOW=resMap.get(URI_CLASS_WORKFLOW);
//			CLASS_HELP=resMap.get(URI_CLASS_HELP);
//			CLASS_PHASE=resMap.get(URI_CLASS_PHASE);
//			CLASS_WORKFLOW_STATUS= resMap.get(URI_CLASS_WORKFLOW_STATUS);
//			CLASS_ACTIVITY_STATUS= resMap.get(URI_CLASS_ACTIVITY_STATUS);
//			CLASS_WORKFLOW_DATA=resMap.get(URI_CLASS_WORKFLOW_DATA);
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
			System.out.println("pair0="+(EWorkflowProperty)pair[0]+
					" pair1="+pair[1].getClass());
			toPropertyMap.put(
					(EWorkflowProperty) pair[0], 
					(Property)schemaModel.getProperty((String)pair[1]));
		}
		
		return toPropertyMap;
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
	
	public IWorkflowData derivedWorkflowData(
								Model model,
								IWorkflowData parent, 
								String childId)
	{
		Resource res=model.createResource(childId,CLASS_WORKFLOW_DATA);
		model.createStatement(
					res, 
					toJenaProperty(EWorkflowProperty.IS_DERIVED_FROM), 
					(Resource)parent.getModelObject());
		return new WorkflowData(res);
	}
}
