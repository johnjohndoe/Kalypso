/*----------------    FILE HEADER  ------------------------------------------
 *
 This file is part of deegree.
 Copyright (C) 2001 by:
 EXSE, Department of Geography, University of Bonn
 http://www.giub.uni-bonn.de/exse/
 lat/lon Fitzke/Fretter/Poth GbR
 http://www.lat-lon.de

 This library is free software; you can redistribute it and/or
 modify it under the terms of the GNU Lesser General Public
 License as published by the Free Software Foundation; either
 version 2.1 of the License, or (at your option) any later version.

 This library is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 Lesser General Public License for more details.

 You should have received a copy of the GNU Lesser General Public
 License along with this library; if not, write to the Free Software
 Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

 Contact:

 Andreas Poth
 lat/lon Fitzke/Fretter/Poth GbR
 Meckenheimer Allee 176
 53115 Bonn
 Germany
 E-Mail: poth@lat-lon.de

 Jens Fitzke
 Department of Geography
 University of Bonn
 Meckenheimer Allee 166
 53115 Bonn
 Germany
 E-Mail: jens.fitzke@uni-bonn.de

 
 ---------------------------------------------------------------------------*/
package org.deegree_impl.services.gazetteer;

import java.util.ArrayList;
import java.util.List;
import java.util.TreeSet;

import org.deegree.gml.GMLGeometry;
import org.deegree.model.feature.Feature;
import org.deegree.model.feature.FeatureCollection;
import org.deegree.model.feature.FeatureProperty;
import org.deegree.model.feature.FeatureType;
import org.deegree.model.feature.FeatureTypeProperty;
import org.deegree.model.geometry.GM_Object;
import org.deegree.services.OGCWebService;
import org.deegree.services.OGCWebServiceClient;
import org.deegree.services.OGCWebServiceEvent;
import org.deegree.services.OGCWebServiceRequest;
import org.deegree.services.OGCWebServiceResponse;
import org.deegree.services.WebServiceException;
import org.deegree.services.gazetteer.GazetteerException;
import org.deegree.services.gazetteer.Node;
import org.deegree.services.gazetteer.SI_LocationType;
import org.deegree.services.gazetteer.capabilities.WFSGCapabilities;
import org.deegree.services.gazetteer.protocol.WFSGDescribeFeatureTypeRequest;
import org.deegree.services.gazetteer.protocol.WFSGGetCapabilitiesRequest;
import org.deegree.services.gazetteer.protocol.WFSGGetCapabilitiesResponse;
import org.deegree.services.gazetteer.protocol.WFSGGetFeatureRequest;
import org.deegree.services.wfs.filterencoding.Filter;
import org.deegree.services.wfs.filterencoding.Operation;
import org.deegree.services.wfs.protocol.WFSDescribeFeatureTypeResponse;
import org.deegree.services.wfs.protocol.WFSGetCapabilitiesResponse;
import org.deegree.services.wfs.protocol.WFSGetFeatureRequest;
import org.deegree.services.wfs.protocol.WFSGetFeatureResponse;
import org.deegree.services.wfs.protocol.WFSQuery;
import org.deegree_impl.gml.GMLFactory;
import org.deegree_impl.model.feature.FeatureFactory;
import org.deegree_impl.services.OGCWebServiceEvent_Impl;
import org.deegree_impl.services.OGCWebServiceResponse_Impl;
import org.deegree_impl.services.OGCWebService_Impl;
import org.deegree_impl.services.gazetteer.protocol.WFSGProtocolFactory;
import org.deegree_impl.services.wfs.WFSFactory;
import org.deegree_impl.services.wfs.filterencoding.ComplexFilter;
import org.deegree_impl.services.wfs.filterencoding.Literal;
import org.deegree_impl.services.wfs.filterencoding.OperationDefines;
import org.deegree_impl.services.wfs.filterencoding.PropertyIsCOMPOperation;
import org.deegree_impl.services.wfs.filterencoding.PropertyName;
import org.deegree_impl.services.wfs.filterencoding.SpatialOperation;
import org.deegree_impl.services.wfs.protocol.WFSProtocolFactory;
import org.deegree_impl.tools.Debug;
import org.deegree_impl.tools.IDGenerator;

/**
 * 
 * This is the basic implementation for a OGC conform web coverage service
 * within the deegree framework. A <tt>WCSService</tt> extends the
 * <tt>OGCWebService</tt> interface to act like a OGC web service. This means
 * that a WCS is callable through the <tt>doService</tt> -method inherited
 * from <tt>OGCWebService</tt>.
 * 
 * 
 * @author <a href="mailto:poth@lat-lon.de">Andreas Poth </a>
 * @version $Revision$ $Date$
 */
public class GazetteerService_Impl extends OGCWebService_Impl implements OGCWebServiceClient
{

  private OGCWebServiceEvent event = null;

  private int hierarchyLevels = 0;

  private String relationType = "";

  private String[] propertyNames = null;

  // Tree for saving the responses
  private GazetteerResponseTree gazetteerresponsetree = null;

  /**
   * @param capabilities
   */
  public GazetteerService_Impl( WFSGCapabilities capabilities )
  {
    this.capabilities = capabilities;
  }

  /**
   * implements the <tt>doService</tt> method inherited from the
   * <tt>OGCWebService_Impl</tt> class. The method receives a WFSG... request
   * and delievers it to one or more WFS's
   * 
   * @param event
   *          event object that contains the requests to be performed
   */
  public void doService( OGCWebServiceEvent event ) throws WebServiceException
  {
    Debug.debugMethodBegin();

    this.event = event;

    OGCWebServiceRequest request = event.getRequest();

    if( request instanceof WFSGGetCapabilitiesRequest )
    {
      handleGetCapabilities( (WFSGGetCapabilitiesRequest)request );
    }
    else if( request instanceof WFSGDescribeFeatureTypeRequest )
    {
      handleDescribeFeatureType( (WFSGDescribeFeatureTypeRequest)request );
    }
    else if( request instanceof WFSGGetFeatureRequest )
    {
      try
      {
        handleGetFeatureRequest( (WFSGGetFeatureRequest)request );
      }
      catch( GazetteerException e )
      {
        throw new WebServiceException( "GazetteerException: " + e.getMessage() );
      }
    }

    Debug.debugMethodEnd();
  }

  /**
   * implements the <tt>doService</tt> method inherited from the
   * <tt>OGCWebService_Impl</tt> class. The method receives a WFSG... request
   * and delievers it to one or more WFS's
   * 
   * @param req
   *          requet to be performed
   */
  public OGCWebServiceResponse doService( OGCWebServiceRequest req )
  {
    throw new NoSuchMethodError( "doService(OGCWebServiceRequest req)" );
  }

  /**
   * @param request
   * @throws WebServiceException
   */
  private void handleGetCapabilities( WFSGGetCapabilitiesRequest request )
      throws WebServiceException
  {
    Debug.debugMethodBegin();

    WFSGGetCapabilitiesResponse res = WFSGProtocolFactory.createWFSGGetCapabilitiesResponse(
        request, (WFSGCapabilities)capabilities );
    OGCWebServiceEvent event_ = new OGCWebServiceEvent_Impl( this, res, "" );
    event.getDestination().write( event_ );

    Debug.debugMethodEnd();
  }

  /**
   * @param request
   * @throws WebServiceException
   */
  private void handleDescribeFeatureType( WFSGDescribeFeatureTypeRequest request )
      throws WebServiceException
  {
    Debug.debugMethodBegin();

    Debug.debugMethodEnd();
  }

  /**
   * creates and performs the initial GetFeature request
   * 
   * @param request
   */
  private void handleGetFeatureRequest( WFSGGetFeatureRequest request ) throws GazetteerException
  {
    Debug.debugMethodBegin();

    String requestid = request.getId() + "--Gaz:";

    WFSQuery[] queries = request.getQuery();

    SI_LocationType locationtype = null;

    for( int i = 0; i < queries.length; i++ )
    {
      Filter filter = queries[i].getFilter();
      propertyNames = queries[i].getPropertyNames();

      if( filter instanceof ThesaurusFilter )
      {
        // WFS-G Thesaurus filter only contains one featureid
        this.hierarchyLevels = ( (ThesaurusFilter)filter ).getHierarchyLevel();
        // the relationType (BT, NT, RT)
        this.relationType = ( (ThesaurusFilter)filter ).getRelationType();
        locationtype = ( (ThesaurusFilter)filter ).getLocationType();
        // recreate WFSQuery to ensure that all properties (especially
        // geographicExtent) are requested
        // TODO just request required propeties
        queries[i] = WFSProtocolFactory.createQuery( null, null, request.getVersion(), queries[i]
            .getTypeName(), queries[i].getFilter() );
      }
      else
      {
        this.hierarchyLevels = 0;
        this.relationType = "NT";
        locationtype = new SI_LocationType_Impl( queries[i].getTypeName(), null, queries[i]
            .getTypeName(), null, null, null, null, null );
      }

      WFSGetFeatureRequest wfsrequest = WFSProtocolFactory.createWFSGetFeatureRequest( request
          .getVersion(), requestid, null, request.getNative(), "FEATURECOLLECTION", request
          .getHandle(), null, request.getMaxFeatures(), request.getStartPosition(), new WFSQuery[]
      { queries[i] } );
      // initializes the tree that manages all performed requests
      initResponseTree( requestid, locationtype );

      performGetFeatureRequest( wfsrequest );
    }
    Debug.debugMethodEnd();
  }

  /**
   * initializes the tree that manages all submitted requests and the responses
   * to it. Each request will form a node in the tree.
   * 
   * @param id
   *          id of the root node
   * @param locationtype
   *          type of the request assigned to the root node
   */
  private void initResponseTree( String id, SI_LocationType locationtype )
  {
    Debug.debugMethodBegin();

    //Node gazetteerresponsenode = new Node_Impl( requestid, 0, dao, null );
    gazetteerresponsetree = new GazetteerResponseTree( id );

    // create feature collection at the root node
    DataObject dao = new DataObject( id, locationtype, FeatureFactory.createFeatureCollection( id,
        100 ) );
    gazetteerresponsetree.getRoot().setData( dao );

    Debug.debugMethodEnd();
  }

  /**
   * @param request
   * @throws GazetteerException
   */
  private void performGetFeatureRequest( WFSGetFeatureRequest request ) throws GazetteerException
  {

    try
    {
      OGCWebServiceEvent event = new OGCWebServiceEvent_Impl( this, request, null, this );
      OGCWebService service = WFSFactory.createWFSService( (WFSGCapabilities)capabilities, null );
      service.doService( event );
    }
    catch( Exception e )
    {
      e.printStackTrace();
      throw new GazetteerException( "Exception occured while creating the "
          + "OGCWebService for the FeatureFilter-Request " + e.toString() );
    }
  }

  /**
   * Method from OGCWebServiceClient. All responses to requests will be passed
   * to this method
   * 
   * @param result
   */
  public void write( Object result )
  {

    Debug.debugMethodBegin();

    OGCWebServiceEvent wse = (OGCWebServiceEvent_Impl)result;

    if( wse.getResponse().getException() == null )
    {
      if( wse.getResponse() instanceof WFSGetCapabilitiesResponse )
      {
        // TODO GetCapabilitiesResponse
      }
      else if( wse.getResponse() instanceof WFSDescribeFeatureTypeResponse )
      {
        // TODO DescribeFeatureTypeResponse
      }
      else if( wse.getResponse() instanceof WFSGetFeatureResponse )
      {
        handleGetFeatureResponse( wse );
      }
    }
    else
    {
      OGCWebServiceResponse res = new OGCWebServiceResponse_Impl( wse.getRequest(), wse
          .getResponse().getException() );
      OGCWebServiceEvent event_ = new OGCWebServiceEvent_Impl( this, res, "" );
      this.event.getDestination().write( event_ );
    }

    Debug.debugMethodEnd();
  }

  /**
   * handles a response to a GetFeature request by informing the controller
   * about the received response and creating a list of new GetFeature requests
   * for the next hierarchy level if the last hierarchy level hasn't been
   * reached.
   * 
   * @param wse
   *          OGCWebServiceEvent containing a WFSGetFeatureResponse
   */
  private synchronized void handleGetFeatureResponse( OGCWebServiceEvent wse )
  {
    Debug.debugMethodBegin();

    WFSGetFeatureResponse response = (WFSGetFeatureResponse)wse.getResponse();
    WFSGetFeatureRequest request = (WFSGetFeatureRequest)response.getRequest();

    Node node = this.gazetteerresponsetree.getNodeAtId( request.getId() );

    if( node.getLevel() != hierarchyLevels )
    {
      try
      {
        performHierachyRequests( response, node );
      }
      catch( Exception e )
      {
        e.printStackTrace();
        OGCWebServiceResponse res = new OGCWebServiceResponse_Impl( wse.getRequest(), wse
            .getResponse().getException() );
        OGCWebServiceEvent event_ = new OGCWebServiceEvent_Impl( this, res, "" );
        this.event.getDestination().write( event_ );
      }
    }
    else
    {
      DataObject pDao = (DataObject)node.getData();
      // if the last level is reached the responses will be collected in
      // nodes that will form the leafs of the tree. For each feature
      // contained in the result collection one node must be created
      FeatureCollection fc = (FeatureCollection)response.getResponse();
      Feature[] features = fc.getAllFeatures();
      for( int i = 0; i < features.length; i++ )
      {
        long lo = IDGenerator.getInstance().generateUniqueID();
        features[i] = recreateFeature( features[i], propertyNames, true );
        DataObject dao = new DataObject( request.getId() + lo, null, features[i] );
        // is true because it's a leaf
        dao.finnished = true;
        node.appendChild( new Node_Impl( dao.id, node.getLevel() + 1, dao, null ) );
        if( pDao.data instanceof FeatureCollection )
        {
          ( (FeatureCollection)pDao.data ).appendFeature( features[i] );
        }
        else
        {
          fc = (FeatureCollection)( (Feature)pDao.data ).getProperty( "children" );
          fc.appendFeature( features[i] );
        }
      }
      // true because the response to the request assigned to this node
      // has been processed
      DataObject dao = (DataObject)node.getData();
      dao.finnished = true;
    }

    // check if all responses for all requests has been received
    // if so finalize the request performing and send the result back
    // to the client
    Node root = gazetteerresponsetree.getRoot();
    if( isFinnished( root ) )
    {
      finishGazetteerService();
    }

    Debug.debugMethodEnd();
  }

  /**
   * creates GetFeature requests to receive the locations instances for the next
   * hierarchy level related to the passed response
   */
  private synchronized void performHierachyRequests( WFSGetFeatureResponse response, Node node )
      throws GazetteerException
  {
    Debug.debugMethodBegin();

    DataObject dao = (DataObject)node.getData();

    if( relationType.equals( "RT" ) )
    {
      if( dao != null && dao.locationType != null )
      {
        // a broder term request is performed against the same locationType
        // as the source of the relation is assigned to
        // !!!! maybe it shall be offered that the related locationType
        // isn't identical to the source locationType
        performWFSRequest( response, dao.locationType );
      }
    }
    else if( relationType.equals( "NT" ) )
    {
      if( dao != null && dao.locationType != null && dao.locationType.getChild() != null
          && dao.locationType.getChild().length > 0 )
      {
        SI_LocationType[] childarray = dao.locationType.getChild();
        for( int i = 0; i < childarray.length; i++ )
        {
          performWFSRequest( response, childarray[i] );
        }
      }
    }
    else if( relationType.equals( "BT" ) )
    {
      if( dao != null && dao.locationType.getParent() != null
          && dao.locationType.getParent().length > 0 )
      {
        SI_LocationType[] parentarray = dao.locationType.getParent();
        for( int i = 0; i < parentarray.length; i++ )
        {
          performWFSRequest( response, parentarray[i] );
        }
      }
    }

    dao.finnished = true;
    Debug.debugMethodEnd();
  }

  /**
   * performs requests for accessing the location instances ofthe next narrower
   * term corresponding to the current hierarchy level of the passed
   * WFSGetFeatureRespose
   * 
   * @param response
   * @param locType
   */
  private void performWFSRequest( WFSGetFeatureResponse response, SI_LocationType locType )
      throws GazetteerException
  {
    Debug.debugMethodBegin();

    if( response.getResponse() instanceof FeatureCollection )
    {

      OGCWebServiceRequest request = response.getRequest();

      FeatureCollection fc = (FeatureCollection)response.getResponse();
      for( int i = 0; i < fc.getSize(); i++ )
      {

        Feature feature = recreateFeature( fc.getFeature( i ), propertyNames, false );

        // appends a child node for this request to the parent node
        Node node = appendChild( request.getId(), locType, feature );

        // create GetFeature request for accessing the locationinstances of the
        // next level
        // TODO get correct property name
        Object prop = fc.getFeature( i ).getProperty( "geographicExtent" );

        //operation formulating the relation between two location types
        Operation operation = null;
        if( prop instanceof GM_Object )
        {
          operation = createOperation( (GM_Object)prop );
        }
        else
        {
          operation = createOperation( prop );
        }

        WFSGetFeatureRequest wfsrequest = createGetFeatureRequest( node.getId(), locType.getName(),
            operation, request.getVersion() );
        // performes a GetFeature request against a WFS
        performGetFeatureRequest( wfsrequest );
      }
    }
    else
    {
      throw new GazetteerException( "Internal Gazetteer error: FeatureID Request "
          + "doesn't return FeatureCollection." );
    }
    Debug.debugMethodEnd();
  }

  /**
   * creates the FilterEncoding operation formulating the relation between two
   * location types for Geometry type properties
   * 
   * @param prop
   *          geometry property acting as literal of the operation
   * @return @throws
   *         GazetteerException
   */
  private Operation createOperation( GM_Object prop ) throws GazetteerException
  {
    Debug.debugMethodBegin();

    GMLGeometry fid_geometry = null;
    try
    {
      fid_geometry = GMLFactory.createGMLGeometry( (GM_Object)prop );
    }
    catch( Exception e )
    {
      throw new GazetteerException( e.toString() );
    }

    // create an WITHIN-Operation: TYPE_SPATIAL = 0;
    int op_id = 0;
    if( relationType.equals( "NT" ) )
    {
      op_id = OperationDefines.INTERSECTS;
    }
    else if( relationType.equals( "BT" ) )
    {
      op_id = OperationDefines.CONTAINS;
    }
    else if( relationType.equals( "RT" ) )
    {
      op_id = OperationDefines.INTERSECTS;
    }
    PropertyName propertyName = new PropertyName( "geographicExtent" );
    // Spatial Operation: int operatorId, PropertyName propertyName, GMLGeometry
    // gmlGeometry
    Operation operation = new SpatialOperation( op_id, propertyName, fid_geometry );

    Debug.debugMethodEnd();
    return operation;
  }

  /**
   * creates the FilterEncoding operation formulating the relation between two
   * location types for none Geometry type properties
   * 
   * @param prop
   *          feature property acting as literal of the operation
   * @return @throws
   *         GazetteerException
   */
  private Operation createOperation( Object prop ) throws GazetteerException
  {
    Debug.debugMethodBegin();

    // TODO get correct property name
    PropertyName propertyName = new PropertyName( "geographicIdentifier" );

    Operation operation = new PropertyIsCOMPOperation( OperationDefines.PROPERTYISEQUALTO,
        propertyName, new Literal( prop.toString() ) );

    Debug.debugMethodEnd();
    return operation;
  }

  /**
   * creates a GetFeature request for accessing the locationinstances of the
   * next level.
   */
  private WFSGetFeatureRequest createGetFeatureRequest( String id, String featureType,
      Operation operation, String wfsVersion ) throws GazetteerException
  {
    Debug.debugMethodBegin();

    // create the new (spatial) Filter
    ComplexFilter cf = new ComplexFilter( operation );

    // create a WFS-Query
    WFSQuery[] query = new WFSQuery[1];
    query[0] = WFSProtocolFactory.createQuery( null, null, wfsVersion, featureType, cf );
    // create the WFSGetFeatureRequest
    WFSGetFeatureRequest wfsrequest = WFSProtocolFactory.createWFSGetFeatureRequest( wfsVersion,
        id, null, null, "FEATURECOLLECTION", null, null, 0, 0, query );

    Debug.debugMethodEnd();
    return wfsrequest;
  }

  /**
   * appends a child node to the node identified by the passed ID
   * 
   * @param id
   *          of the parent node
   * @param locType
   *          location type of the requested instances
   * @param feature
   *          feature assigned to the new node
   */
  private Node appendChild( String id, SI_LocationType locType, Feature feature )
  {
    Debug.debugMethodBegin();

    Node node = this.gazetteerresponsetree.getNodeAtId( id );
    // get parent nodes data object
    DataObject pDao = (DataObject)node.getData();

    // if the parents node data is a FeatureCollection it must be the
    // root node and the feature can simply be appended to it. Otherwise it
    // must be appended to the 'children' attribute of the feature that also
    // is a FeatureCollection
    if( pDao.data instanceof FeatureCollection )
    {
      ( (FeatureCollection)pDao.data ).appendFeature( feature );
    }
    else
    {
      FeatureCollection fc = (FeatureCollection)( (Feature)pDao.data ).getProperty( "children" );
      fc.appendFeature( feature );
    }

    long lo = IDGenerator.getInstance().generateUniqueID();
    id = id + lo;

    DataObject dao = new DataObject( id, locType, feature );
    int level = node.getLevel() + 1;
    Node newNode = new Node_Impl( id, level, dao, null );
    node.appendChild( newNode );

    Debug.debugMethodEnd();
    return newNode;
  }

  /**
   * creates a new feature by adding a property named 'children' that will
   * contain all children of the passed feature. Also the geographicExtent
   * property will be removed from the feature if not requested to be returned
   * 
   * @param feature
   *          source feature to re-create
   * @param propertyNames
   *          names of the properties to be consider
   */
  private Feature recreateFeature( Feature feature, String[] propertyNames, boolean leaf )
  {
    Debug.debugMethodBegin();

    TreeSet ts = new TreeSet();
    if( propertyNames != null )
    {
      for( int i = 0; i < propertyNames.length; i++ )
      {
        ts.add( propertyNames[i] );
      }
    }

    Object o = null;

    List ftpList = new ArrayList();
    List fpList = new ArrayList();

    FeatureProperty fp = null;
    FeatureTypeProperty ftp = null;
    if( ts.size() == 0 || ts.contains( "geographicIdentifier" ) )
    {
      ftp = FeatureFactory.createFeatureTypeProperty( "geographicIdentifier", "java.lang.String",
          false );
      ftpList.add( ftp );
      o = feature.getProperty( "geographicIdentifier" );
      fp = FeatureFactory.createFeatureProperty( "geographicIdentifier", o );
      fpList.add( fp );
    }
    if( ts.size() == 0 || ts.contains( "identifier" ) )
    {
      ftp = FeatureFactory.createFeatureTypeProperty( "identifier", "java.lang.String", false );
      ftpList.add( ftp );
      o = feature.getProperty( "identifier" );
      fp = FeatureFactory.createFeatureProperty( "identifier", o );
      fpList.add( fp );
    }
    if( ts.size() == 0 || ts.contains( "geographicExtent" ) )
    {
      ftp = FeatureFactory.createFeatureTypeProperty( "geographicExtent",
          "org.deegree.model.geometry.GM_Object", true );
      ftpList.add( ftp );
      o = feature.getProperty( "geographicExtent" );
      fp = FeatureFactory.createFeatureProperty( "geographicExtent", o );
      fpList.add( fp );
    }
    if( ts.size() == 0 || ts.contains( "position" ) )
    {
      ftp = FeatureFactory.createFeatureTypeProperty( "position",
          "org.deegree.model.geometry.GM_Point", true );
      ftpList.add( ftp );
      o = feature.getProperty( "position" );
      fp = FeatureFactory.createFeatureProperty( "position", o );
      fpList.add( fp );
    }

    if( !leaf )
    {
      // must just be available if not leaf
      ftp = FeatureFactory.createFeatureTypeProperty( "children",
          "org.deegree.feature.FeatureCollection", true );
      ftpList.add( ftp );
      o = FeatureFactory.createFeatureCollection( feature.getId() + "c", 100 );
      fp = FeatureFactory.createFeatureProperty( "children", o );
      fpList.add( fp );
    }

    FeatureTypeProperty[] ftpL = (FeatureTypeProperty[])ftpList
        .toArray( new FeatureTypeProperty[ftpList.size()] );
    FeatureProperty[] fpL = (FeatureProperty[])fpList.toArray( new FeatureProperty[fpList.size()] );
    FeatureType featureType = FeatureFactory.createFeatureType( null, null, feature
        .getFeatureType().getName(), ftpL);

    Debug.debugMethodEnd();

    return FeatureFactory.createFeature( feature.getId(), featureType, fpL );
  }

  /**
   * traverses the tree in preorder starting at the passed node
   */
  public synchronized boolean isFinnished( Node node )
  {

    DataObject dao = (DataObject)node.getData();
    if( !dao.finnished )
      return false;

    Node[] nodes = node.getChildren();

    boolean finnished = true;
    if( nodes != null )
    {
      for( int i = 0; i < nodes.length; i++ )
      {
        finnished = isFinnished( nodes[i] );
        if( !finnished )
          return false;
      }
    }
    return finnished;
  }

  /**
   * 
   *  
   */
  private void finishGazetteerService()
  {

    Node root = gazetteerresponsetree.getRoot();
    FeatureCollection fc = (FeatureCollection)( (DataObject)root.getData() ).data;

    OGCWebServiceResponse res = WFSGProtocolFactory.createWFSGGetFeatureResponse( this.event
        .getRequest(), null, null, fc );
    OGCWebServiceEvent wse = new OGCWebServiceEvent_Impl( this, res, "message" );

    this.event.getDestination().write( wse );
  }

}