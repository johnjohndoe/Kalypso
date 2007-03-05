package org.kalypso.afgui.schema;

import java.io.InputStream;
import java.net.URI;
import java.util.ArrayList;
import java.util.EnumMap;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.logging.Level;
import java.util.logging.Logger;

import org.eclipse.core.runtime.Platform;
import org.kalypso.afgui.db.EWorkflowProperty;
import org.kalypso.afgui.model.IWorkflowConcept;
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
import com.hp.hpl.jena.vocabulary.RDF;

/**
 * A utility java representation of the workflow schema. It contains a jena model for the schema, and the element
 * declared in the schema. The vocabulary elements, resources and properties, are all static final field. If their
 * initialization fails because of an exception, this exception is store in {@link #PROBLEM_FLAG} are all element are
 * consequently set to null.
 * 
 * @author Patrice Congo
 */
final public class Schema
{

  interface IResourceWrapperConstructor
  {
    public IWorkflowConcept construct( Object resource );
  }

  static final Logger logger = Logger.getLogger( Schema.class.getName() );

  private static final boolean log = Boolean.parseBoolean( Platform.getDebugOption( "org.kalypso.afgui/debug" ) );

  static
  {
    if( !log )
      logger.setUseParentHandlers( false );
  }

  public static final String SCHEMA_NS = "http://www.tu-harburg.de/wb/kalypso/schema/workflow#";

  final static public String WORKFLOW_SCHEMA_FOLDER = "res";

  final static public String WORKFLOW_SCHEMA_FILE = "res/workflow_schema.xml";

  final static public Model schemaModel = ModelFactory.createDefaultModel();

  final static public Property PROP_HAS_NAME;

  final static public String URI_PROP_HAS_NAME = SCHEMA_NS + "hasName";

  final static public Property PROP_PART_OF;

  final static public String URI_PROP_PART_OF = SCHEMA_NS + "partOf";

  final static public String URI_PROP_FOLLOWS = SCHEMA_NS + "follows";

  final static public Property PROP_FOLLOWS;

  final static public String URI_CLASS_WORKFLOW_STATUS = SCHEMA_NS + "WorkflowStatus";

  final static public Resource CLASS_WORKFLOW_STATUS = null;

  final static public String URI_CLASS_ACTIVITY_STATUS = SCHEMA_NS + "ActivityStatus";

  final static public Resource CLASS_ACTIVITY_STATUS = null;

  //
  final static public String URI_CLASS_WORKFLOW_DATA = SCHEMA_NS + "WorkflowData";

  final static public Resource CLASS_WORKFLOW_DATA;

  final static public IResourceWrapperConstructor CONSTRUCTOR_WORKFLOW_DATA = new IResourceWrapperConstructor()
  {

    public IWorkflowConcept construct( Object resource )
    {
      return new WorkflowData( (Resource) resource );
    }
  };

  // hasType
  final static public String URI_PROP_HAS_TYPE = SCHEMA_NS + "hasType";

  final static public Property PROP_HAS_TYPE;

  final static private EnumMap<EWorkflowProperty, Property> toPropertyMap;

  final static public String URI_PROP_HAS_LOCATION = SCHEMA_NS + "hasLocation";

  final static public String URI_PROP_IS_DERIVED_FROM = SCHEMA_NS + "isDerivedFrom";

  final static public Property PROP_IS_DERIVED_FROM;

  // executedWorkflowPart
  final static public String URI_PROP_EXECUTING_WORKFLOW_PART = SCHEMA_NS + "executingWorkflowPart";

  final static public Property PROP_EXECUTING_WORKFLOW_PART;

  // processedResource
  final static public String URI_PROP_PROCESSED_RES = SCHEMA_NS + "processedResource";

  final static public Property PROP_PROCESSED_RES;

  // hasSubRTContext
  final static public String URI_PROP_HAS_SUB_RT_CONTEXT = SCHEMA_NS + "hasSubRTContext";

  final static public Property PROP_HAS_SUB_RT_CONTEXT;

  final static public String URI_PROP_IS_WORKS_ON = SCHEMA_NS + "worksOn";

  final static public String URI_PROP_IS_CONTAINS = SCHEMA_NS + "contains";

  /**
   * Holds the throwable which describt a failure while initialising the class static fields. A value null signal an
   * error free initialisation
   */
  static final public Throwable PROBLEM_FLAG;

  final static public String PJT_NS = "pjtNS";

  private Schema( )
  {
    // empty
  }

  static
  {
    Map<String, Property> propMap = new HashMap<String, Property>();
    Map<String, Resource> resMap = new HashMap<String, Resource>();
    Throwable backupTh = null;
    try
    {
      // load model
      // File resFolder=
      // new File(Schema.class.getResource(WORKFLOW_SCHEMA_FOLDER).getPath());

      InputStream iStream = Schema.class.getResourceAsStream( WORKFLOW_SCHEMA_FILE );// new FileInputStream(new
                                                                                      // File(resFolder,WORKFLOW_SCHEMA_FILE));
      schemaModel.read( iStream, "" );
      String propUris[] = { URI_PROP_HAS_NAME, URI_PROP_PART_OF, URI_PROP_FOLLOWS, URI_PROP_HAS_TYPE, URI_PROP_EXECUTING_WORKFLOW_PART, URI_PROP_PROCESSED_RES, URI_PROP_HAS_SUB_RT_CONTEXT,
          URI_PROP_IS_DERIVED_FROM };

      String resUris[] = { URI_CLASS_WORKFLOW_STATUS, URI_CLASS_ACTIVITY_STATUS, URI_CLASS_WORKFLOW_DATA };

      // find vokabulary elements
      fillPropMap( propMap, propUris );
      fillResMap( resMap, resUris );
    }
    catch( Throwable th )
    {
      // remove all sothat vocabulary element will be init with null
      logger.log( Level.SEVERE, "Loading fails", th );
      propMap.clear();
      resMap.clear();
      backupTh = th;
    }
    finally
    {
      // init vokabulary element
      PROBLEM_FLAG = backupTh;
      PROP_HAS_NAME = propMap.get( URI_PROP_HAS_NAME );
      PROP_PART_OF = propMap.get( URI_PROP_PART_OF );
      PROP_FOLLOWS = propMap.get( URI_PROP_FOLLOWS );
      PROP_HAS_TYPE = propMap.get( URI_PROP_HAS_TYPE );
      PROP_IS_DERIVED_FROM = propMap.get( URI_PROP_IS_DERIVED_FROM );
      PROP_EXECUTING_WORKFLOW_PART = propMap.get( URI_PROP_EXECUTING_WORKFLOW_PART );
      PROP_PROCESSED_RES = propMap.get( URI_PROP_PROCESSED_RES );
      PROP_HAS_SUB_RT_CONTEXT = propMap.get( URI_PROP_HAS_SUB_RT_CONTEXT );
      toPropertyMap = computeEWorkflowPropToJena();
      CLASS_WORKFLOW_DATA = resMap.get( URI_CLASS_WORKFLOW_DATA );
    }

  }

  private static final EnumMap<EWorkflowProperty, Property> computeEWorkflowPropToJena( )
  {
    Object[][] eWorklflowPropToJena = { { EWorkflowProperty.HAS_LOCATION, URI_PROP_HAS_LOCATION }, { EWorkflowProperty.HAS_TYPE, URI_PROP_HAS_TYPE },
        { EWorkflowProperty.IS_DERIVED_FROM, URI_PROP_IS_DERIVED_FROM }, { EWorkflowProperty.CONTAINS, URI_PROP_IS_CONTAINS }, };

    EnumMap<EWorkflowProperty, Property> toPropertyMap = new EnumMap<EWorkflowProperty, Property>( EWorkflowProperty.class );

    for( Object[] pair : eWorklflowPropToJena )
    {
      // logger.info("pair0="+(EWorkflowProperty)pair[0]+
      // " pair1="+pair[1].getClass());
      toPropertyMap.put( (EWorkflowProperty) pair[0], schemaModel.getProperty( (String) pair[1] ) );
    }

    return toPropertyMap;
  }

  private static final void fillPropMap( Map<String, Property> propMap, String[] uris )
  {
    Property curProp;
    for( String uri : uris )
    {
      curProp = schemaModel.getProperty( uri );
      if( curProp == null )
      {
        throw new RuntimeException( "No Prop for " + uri );
      }
      propMap.put( uri, curProp );
    }
  }

  private static final void fillResMap( Map<String, Resource> propMap, String[] uris )
  {
    Resource curRes;
    for( String uri : uris )
    {
      curRes = schemaModel.getResource( uri );
      if( curRes == null )
      {
        throw new RuntimeException( "No res for " + uri );
      }
      propMap.put( uri, curRes );
    }
  }

  static public Property toJenaProperty( EWorkflowProperty prop )
  {
    return toPropertyMap.get( prop );
  }

  // static public void main(String[] args) throws IOException
  // {
  // System.out.println("------------------------------");
  // // FileOutputStream out= new FileOutputStream("G:\\test.txt");
  // PrintStream pOut= System.out;//new PrintStream(out);
  // //System.setOut(pOut);
  // // System.out.println(Schema.CLASS_ACTIVITY);
  // pOut.println("Exception="+Schema.PROBLEM_FLAG);
  // pOut.println("hasName="+Schema.PROP_HAS_NAME);
  // pOut.println("hasActivity="+PROP_HAS_ACTIVITY);
  // pOut.println("isRoot="+PROP_IS_ROOT);
  // pOut.println("partOf="+PROP_PART_OF);
  // pOut.println("exestate="+PROP_EXE_STATE);
  // pOut.println("workflow="+CLASS_WORKFLOW);
  // pOut.println("activity="+CLASS_ACTIVITY);
  // pOut.println("Phase="+CLASS_PHASE);
  // //pOut.println("schemaModel="+schemaModel);
  //		
  // //schemaModel.write(pOut);
  // // Schema.PROBLEM_FLAG.printStackTrace(pOut);
  // //pOut.println(Schema.class.getResource(WORKFLOW_SCHEMA_FILE));
  // //pOut.println(Schema.class.getResource("res/workflow_schema.rdfs"));
  // }

  final static public RDFNode getProperty( Resource resource, EWorkflowProperty prop )
  {
    Statement stm = resource.getProperty( toJenaProperty( prop ) );
    if( stm == null )
    {
      return null;
    }
    else
    {
      return stm.getObject();
    }
  }

  final static public String getStringProperty( Resource resource, EWorkflowProperty prop )
  {
    Statement stm = resource.getProperty( toJenaProperty( prop ) );
    if( stm == null )
    {
      return null;
    }
    else
    {
      RDFNode obj = stm.getObject();
      if( obj.isLiteral() )
      {
        return ((Literal) obj).getString();
      }
      else
      {
        throw new RuntimeException( "Node not a literal:" + stm );
      }
    }
  }

  final static public void createStatement( Model model, IWorkflowData subject, IWorkflowData object, EWorkflowProperty prop )
  {
    model.add( (Resource) subject.getModelObject(), toJenaProperty( prop ), (Resource) object.getModelObject() );
  }

  final static public void removeStatement( Model model, IWorkflowData subject, IWorkflowData object, EWorkflowProperty prop )
  {
    model.removeAll( (Resource) subject.getModelObject(), toJenaProperty( prop ), (Resource) object.getModelObject() );
  }

  static final public List<IWorkflowData> getWorkflowDataByType( Model model, String type )
  {
    ResIterator it = model.listSubjectsWithProperty( toJenaProperty( EWorkflowProperty.HAS_TYPE ), type );
    List<IWorkflowData> list = new ArrayList<IWorkflowData>();
    for( ; it.hasNext(); )
    {
      list.add( new WorkflowData( it.nextResource() ) );
    }
    return list;
  }

  final static public IWorkflowData getWorkflowDataById( Model model, String id )
  {
    Resource res = model.getResource( id );
    if( res != null )
    {
      return new WorkflowData( res );
    }
    else
    {
      return null;
    }
  }

  final static private String toURIString( Model model, String id )
  {
    logger.info( "id0=" + id );
    try
    {
      URI uri = new URI( id );
      if( uri.getScheme() == null )
      {
        id = model.getNsPrefixURI( Schema.PJT_NS ) + id;
      }
    }
    catch( Throwable th )
    {
      id = model.getNsPrefixURI( Schema.PJT_NS ) + id;
    }
    logger.info( "id1=" + id );
    return id;
  }

  final static public IWorkflowData createWorkflowData( Model model, IWorkflowData parent, String childId )
  {
    Resource res = model.createResource( toURIString( model, childId ), CLASS_WORKFLOW_DATA );

    res.addProperty( PROP_HAS_NAME, childId );
    if( parent != null )
    {
      try
      {
        // model.add(
        // (Resource)parent.getModelObject(),
        // PROP_IS_DERIVED_FROM,
        // res);

        model.add( res, PROP_IS_DERIVED_FROM, (Resource) parent.getModelObject() );
      }
      catch( Throwable th )
      {
        logger.log( Level.SEVERE, "New Res=" + res + " parent=" + parent + " propIsDevFrom=" + PROP_IS_DERIVED_FROM, th );
      }
    }
    return new WorkflowData( res );
  }

  final static public IWorkflowData derivedWorkflowData( Model model, IWorkflowData parent, String childId )
  {
    Resource res = model.createResource( toURIString( model, childId ), CLASS_WORKFLOW_DATA );
    model.createStatement( res, toJenaProperty( EWorkflowProperty.IS_DERIVED_FROM ), (Resource) parent.getModelObject() );
    return new WorkflowData( res );
  }

  final static public List<IWorkflowData> getRootWorkflowDataByType( Model model, String type )
  {
    ResIterator it = model.listSubjectsWithProperty( RDF.type, CLASS_WORKFLOW_DATA );
    // logger.info("Iterator for type="+type+" "+it.hasNext());
    List<IWorkflowData> list = new ArrayList<IWorkflowData>();
    Resource res;
    // final Property PROP=toJenaProperty(EWorkflowProperty.HAS_TYPE);
    for( ; it.hasNext(); )
    {
      res = it.nextResource();
      // TODO change has_a to has_child
      if( !res.hasProperty( PROP_HAS_TYPE ) && !res.hasProperty( PROP_IS_DERIVED_FROM ) )
      {
        list.add( new WorkflowData( res ) );
      }
    }
    return list;
  }

  public static String getName( Resource resource )
  {
    logger.info( "Getting name:" + resource.getURI() );
    if( resource == null )
    {
      return null;
    }
    else
    {
      Statement stm = resource.getProperty( PROP_HAS_NAME );
      if( stm == null )
      {
        logger.warning( "No name property:" + resource.getURI() );
        return null;
      }
      else
      {
        return stm.getObject().toString();
      }
    }
  }

  public static IWorkflowData getProcessedWorkflowData( Resource resource )
  {
    if( resource == null )
    {
      throw new IllegalArgumentException();
    }
    else
    {
      Statement stm = resource.getProperty( PROP_PROCESSED_RES );
      if( stm == null )
      {
        logger.warning( "Context does not have processed resource:" + PROP_PROCESSED_RES + "\n" + resource );
        return null;
      }
      else
      {
        try
        {
          Resource wfData = stm.getResource();
          return new WorkflowData( wfData );
        }
        catch( Throwable th )
        {
          logger.log( Level.SEVERE, "Cannot processed workflow data", th );
          return null;
        }
      }

    }
  }

  public static void removeWorkflowData( Resource resource )
  {
    Model model = resource.getModel();
    logger.info( "REM_MODEL0:" + model );
    model.removeAll( resource, null, null );
    model.removeAll( null, null, resource );
    logger.info( "REM_MODEL1:" + model );
  }
}
