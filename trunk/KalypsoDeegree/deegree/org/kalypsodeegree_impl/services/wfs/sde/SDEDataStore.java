/*----------------    FILE HEADER  ------------------------------------------

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
package org.deegree_impl.services.wfs.sde;

import java.net.URL;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;

import org.deegree.model.feature.Feature;
import org.deegree.model.feature.FeatureProperty;
import org.deegree.model.feature.FeatureTypeProperty;
import org.deegree.model.geometry.GM_Envelope;
import org.deegree.model.geometry.GM_Object;
import org.deegree.model.table.Table;
import org.deegree.services.OGCWebServiceException;
import org.deegree.services.OGCWebServiceRequest;
import org.deegree.services.OGCWebServiceResponse;
import org.deegree.services.wfs.DataStoreException;
import org.deegree.services.wfs.WFSConstants;
import org.deegree.services.wfs.configuration.FeatureType;
import org.deegree.services.wfs.configuration.GeoFieldIdentifier;
import org.deegree.services.wfs.filterencoding.Filter;
import org.deegree.services.wfs.protocol.WFSDescribeFeatureTypeRequest;
import org.deegree.services.wfs.protocol.WFSGetFeatureRequest;
import org.deegree.services.wfs.protocol.WFSGetFeatureWithLockRequest;
import org.deegree.services.wfs.protocol.WFSLockFeatureRequest;
import org.deegree.services.wfs.protocol.WFSQuery;
import org.deegree.services.wfs.protocol.WFSTransactionRequest;
import org.deegree.tools.ParameterList;
import org.deegree_impl.io.sdeapi.SpatialQuery;
import org.deegree_impl.model.cs.Adapters;
import org.deegree_impl.model.cs.ConvenienceCSFactory;
import org.deegree_impl.model.cs.CoordinateSystem;
import org.deegree_impl.model.feature.FeatureFactory;
import org.deegree_impl.model.geometry.GM_Object_Impl;
import org.deegree_impl.model.geometry.GeometryFactory;
import org.deegree_impl.services.OGCWebServiceException_Impl;
import org.deegree_impl.services.wfs.AbstractDataStore;
import org.deegree_impl.services.wfs.AbstractDescribeFeatureType;
import org.deegree_impl.services.wfs.AbstractGetFeature;
import org.deegree_impl.services.wfs.WFSMainLoop;
import org.deegree_impl.services.wfs.filterencoding.ComplexFilter;
import org.deegree_impl.services.wfs.filterencoding.FilterTools;
import org.deegree_impl.services.wfs.protocol.WFSProtocolFactory;
import org.deegree_impl.tools.Debug;
import org.deegree_impl.tools.ParameterList_Impl;
import org.opengis.cs.CS_CoordinateSystem;
import org.w3c.dom.Document;

/**
 * The class provides reading and writing access to ESRI shape- files. The
 * access is capsulated withing the query and transaction mechanism described at
 * the OGC WFS specifications.
 * <p>
 * </p>
 * The data store uses a cache for keeping features once read from a shape-file
 * in memory to provide a mauch fast access to them. The cache can be configured
 * throw the configuration XML-document that also contains the names and
 * locations of the shapes handled by an instance of the <tt>SDEDataStore</tt>.
 * <p>
 * ---------------------------------------------------------------------
 * </p>
 * 
 * @author <a href="mailto:poth@lat-lon.de">Andreas Poth </a>
 * @version $Revision$ $Date$
 */
public class SDEDataStore extends AbstractDataStore
{

  /**
   * Creates a new SDEDataStore object.
   * 
   * @param config
   * 
   * @throws DataStoreException
   */
  public SDEDataStore( URL config ) throws DataStoreException
  {
    super( config );
  }

  /**
   * returns the describtion of one or more feature types
   * 
   * @param request
   *          conainting the list of feature types that should be described
   */
  public void describeFeatureType( WFSDescribeFeatureTypeRequest request )
  {
    Thread thread = new CDescribeFeatureType( this, request );
    thread.start();
  }

  /**
   * returns the features that matches the submitted request
   * 
   * @param request
   *          containing the request for zero, one or more features. The
   *          request, may contains a filter that describes the request more
   *          detailed
   */
  public void getFeature( WFSGetFeatureRequest request )
  {
    Thread thread = new CGetFeature( this, request );
    thread.start();
  }

  /**
   * same as <tt>getFeature(..)</tt> but locking the feature during
   * processing.
   * 
   * @see SDEDataStore#getFeature(WFSGetFeatureRequest)
   * 
   * @param request
   *          containing the request for zero, one or more features. The
   *          request, may contains a filter that describes the request more
   *          detailed.
   */
  public void getFeatureWithLock( WFSGetFeatureWithLockRequest request )
  {
    Thread thread = new CGetFeatureWithLock( this, request );
    thread.start();
  }

  /**
   * performs a transaction against the data store. This could be an update, an
   * insert or a delete of one or more features.
   * 
   * @param request
   *          containing the transaction instruction(s)
   */
  public void transaction( WFSTransactionRequest request )
  {
    Thread thread = new CTransaction( this, request );
    thread.start();
  }

  /**
   * performs the locking/unlocking of one or more features.
   * 
   * @param request
   *          the features that should be (un)locked
   */
  public void lockFeature( WFSLockFeatureRequest request )
  {
    Thread thread = new CLockFeature( this, request );
    thread.start();
  }

  //////////////////////////////////////////////////////////////////////////
  //                           inner classes //
  //////////////////////////////////////////////////////////////////////////

  /**
   * inner interface defining the processing of a DescribeFeatureType request
   */
  private class CDescribeFeatureType extends AbstractDescribeFeatureType
  {
    /**
     * Creates a new CDescribeFeatureType object.
     * 
     * @param parent
     * @param request
     */
    public CDescribeFeatureType( SDEDataStore parent, OGCWebServiceRequest request )
    {
      super( parent, request );
    }

    /**
     * creates a xml schema definition of the submitted feature type on the fly
     */
    protected Document createSchema( String featureType ) throws Exception
    {
      throw new Exception( "At the moment createSchema is a not supported " + "function." );
    }
  }

  /**
   * inner interface defining the processing of a getFeature request
   */
  private class CGetFeature extends AbstractGetFeature
  {
    /**
     * Creates a new CGetFeature object.
     * 
     * @param parent
     * @param request
     */
    public CGetFeature( SDEDataStore parent, OGCWebServiceRequest request )
    {
      super( parent, request );
    }

    /**
     * 
     * 
     * @param request
     * 
     * @return
     */
    protected OGCWebServiceResponse[] performRequest( OGCWebServiceRequest request )
    {
      Debug.debugMethodBegin( this, "performRequest" );

      HashMap map = new HashMap();
      OGCWebServiceResponse[] response = null;
      String[] affectedFeatureTypes = null;
      WFSGetFeatureRequest req = (WFSGetFeatureRequest)request;

      try
      {
        WFSQuery[] queries = req.getQuery();
        affectedFeatureTypes = getAffectedFeatureTypes( queries );

        int startPosition = req.getStartPosition();
        int maxFeatures = req.getMaxFeatures();

        // get describtions for each feature type that's handled
        // by this data store
        for( int i = 0; i < queries.length; i++ )
        {
          if( isKnownFeatureType( queries[i].getTypeName() ) )
          {
            FeatureType ft = config.getFeatureType( queries[i].getTypeName() );
            // create query string
            String[] cols = getQueryProperties( queries[i], ft );
            // perform query and return result as table model
            Object[] o = getDataFromSDE( queries[i], cols );
            Feature[] features = getFeatures( queries[i], (GM_Object[])o[1], (Table)o[0],
                startPosition, maxFeatures );

            ParameterList pl = new ParameterList_Impl();
            pl.addParameter( WFSConstants.FEATURES, features );
            pl.addParameter( WFSConstants.FEATURETYPE, ft );
            pl.addParameter( WFSConstants.CRS, ft.getCRS() );

            // transform table model to GMLFeatureCollection
            // and put it onto a HashMap associated with its
            // table (feature type) name.
            map.put( queries[i].getTypeName(), pl );
          }
        }

        // create response
        response = createResponse( map, affectedFeatureTypes );
      }
      catch( Exception e )
      {
        Debug.debugException( e, null );

        OGCWebServiceException exce = new OGCWebServiceException_Impl(
            "CGetFeature: performRequest", e.toString() );

        response = new OGCWebServiceResponse[1];
        response[0] = WFSProtocolFactory.createWFSGetFeatureResponse( request,
            affectedFeatureTypes, exce, null );
      }

      Debug.debugMethodEnd();
      return response;
    }

    /**
     * returns the name of the datastore fields (SDE table column names) that
     * shall be returned by the query or null if all columns shall be returned
     */
    private String[] getQueryProperties( WFSQuery query, FeatureType ft ) throws Exception
    {
      Debug.debugMethodBegin();

      String[] props = query.getPropertyNames();
      String[] fields = null;
      if( props != null && props.length > 0 )
      {
        fields = new String[props.length];
        for( int i = 0; i < props.length; i++ )
        {
          String[] s = ft.getDatastoreField( props[i] );
          if( s == null || s.length == 0 )
          {
            throw new Exception( "no field for property: " + props[i] + " can be found!" );
          }
          fields[i] = s[0];
        }
      }
      else
      {
        HashMap map = ft.getMappings();
        fields = new String[map.size()];
        // map the database field names to the property names
        Iterator iterator = map.keySet().iterator();
        int i = 0;
        while( iterator.hasNext() )
        {
          String prop = (String)iterator.next();
          String[] s = (String[])map.get( prop );
          fields[i++] = s[0];
        }
      }

      Debug.debugMethodEnd();
      return fields;
    }

    /**
     * queries and retrieves data from a ArcSDE
     * 
     * @param query
     *          query to perform
     * @param cols
     *          columns to target
     */
    private Object[] getDataFromSDE( WFSQuery query, String[] cols ) throws Exception
    {
      Debug.debugMethodBegin();

      Filter filter = query.getFilter();
      FeatureType ft = config.getFeatureType( query.getTypeName() );

      // returns an object that enables performing of spatial queries
      // against a SDE
      SpatialQuery sq = openSpatialQuery();

      GM_Envelope bbox = null;
      // split filter into spatial and logical component
      if( filter instanceof ComplexFilter )
      {
        Object[] objects = FilterTools.extractFirstBBOX( ( (ComplexFilter)filter ) );
        bbox = (GM_Envelope)objects[0];
        filter = (Filter)objects[1];
      }
      if( bbox == null )
      {
        bbox = GeometryFactory.createGM_Envelope( -9E99, -9E99, 9E99, 9E99 );
      }

      // open the specified layer
      sq.setLayer( ft.getMasterTable().getName() );
      // set the spatial filter (BB)
      sq.setSpatialFilter( bbox.getMin().getX(), bbox.getMin().getY(), bbox.getMax().getX(), bbox
          .getMax().getY() );
      // run query to get the geometry objects
      Table table = sq.runSpatialQuery( cols );

      // get all geo objects resulting form the query
      GM_Object[] deegree_gm_obj = sq.getGeometries();

      Object[] o = new Object[2];
      o[0] = table;
      o[1] = deegree_gm_obj;

      Debug.debugMethodEnd();
      return o;
    }

    /**
     * reads features from a shape file
     */
    private Feature[] getFeatures( WFSQuery query, GM_Object[] deegree_gm_obj, Table table,
        int startPosition, int maxFeatures ) throws Exception
    {
      Debug.debugMethodBegin();

      Filter filter = query.getFilter();
      FeatureType ft = config.getFeatureType( query.getTypeName() );

      // create feature type
      org.deegree.model.feature.FeatureType ftt = createFeatureType( table, ft,
          deegree_gm_obj.length == table.getRowCount() );

      // consider that not all feature may be are requested
      if( startPosition < 0 )
      {
        startPosition = 0;
      }
      if( startPosition >= table.getRowCount() )
      {
        return new Feature[0];
      }
      if( ( maxFeatures < 0 ) || ( maxFeatures >= table.getRowCount() ) )
      {
        maxFeatures = table.getRowCount();
      }

      ArrayList features = new ArrayList( maxFeatures );

      // create features
      String geoFieldName = null;
      GeoFieldIdentifier[] gfi = ft.getMasterTable().getGeoFieldIdentifier();
      if( gfi == null || gfi.length == 0 )
      {
        geoFieldName = "GEOM";
      }
      else
      {
        geoFieldName = gfi[0].getDatastoreFieldBaseName();
      }

      // get coordinate system associated to the feature type
      CS_CoordinateSystem crs = getReferenceSystem( ft.getCRS() );

      if( deegree_gm_obj.length == table.getRowCount() )
      {
        // features including geometries
        // write each feature between the start and end position that matches
        // the filter into an ArrayList
        for( int i = startPosition; i < table.getRowCount(); i++ )
        {
          FeatureProperty[] fp = new FeatureProperty[table.getColumnCount() + 1];
          for( int j = 0; j < ( fp.length - 1 ); j++ )
          {
            String s = ft.getProperty( table.getColumnName( j ) );
            fp[j] = FeatureFactory.createFeatureProperty( s, table
                .getValueAt( i - startPosition, j ) );
          }

          if( deegree_gm_obj[i - startPosition] != null )
          {
            ( (GM_Object_Impl)deegree_gm_obj[i - startPosition] ).setCoordinateSystem( crs );
          }
          String s = ft.getProperty( geoFieldName );
          fp[fp.length - 1] = FeatureFactory.createFeatureProperty( s, deegree_gm_obj[i
              - startPosition] );
          Feature feature = FeatureFactory.createFeature( i + "", ftt, fp );
          // check the feature against the filter
          if( filter == null || filter.evaluate( feature ) )
          {
            features.add( feature );
            if( features.size() == maxFeatures )
              break;
          }
        }
      }
      else
      {
        // features not including geometries
        // write each feature between the start and end position that matches
        // the filter into an ArrayList
        for( int i = startPosition; i < table.getRowCount(); i++ )
        {
          FeatureProperty[] fp = new FeatureProperty[table.getColumnCount()];
          for( int j = 0; j < fp.length; j++ )
          {
            String s = ft.getProperty( table.getColumnName( j ) );
            fp[j] = FeatureFactory.createFeatureProperty( s, table
                .getValueAt( i - startPosition, j ) );
          }

          Feature feature = FeatureFactory.createFeature( i + "", ftt, fp );
          // check the feature against the filter
          if( filter == null || filter.evaluate( feature ) )
          {
            features.add( feature );
            if( features.size() == maxFeatures )
              break;
          }
        }
      }

      Debug.debugMethodEnd();
      // create and return feature collection
      return (Feature[])features.toArray( new Feature[features.size()] );
    }

    private SpatialQuery openSpatialQuery() throws Exception
    {
      Debug.debugMethodBegin();
      // get server address and port
      String logon = config.getConnection().getLogon();
      int pos = logon.indexOf( ':' );
      String server = logon.substring( 0, pos );
      String ports = logon.substring( pos + 1, logon.length() );
      int port = 80;

      if( ( ports != null ) && ( ports.length() > 0 ) )
      {
        port = Integer.parseInt( ports );
      }

      String database = config.getConnection().getSDEDatabase();

      // proceed if openConnection succeeds
      SpatialQuery sq = new SpatialQuery( server, port, database, config.getConnection().getUser(),
          config.getConnection().getPassword() );

      Debug.debugMethodEnd();
      return sq;
    }

    /**
     * creates and returns a CRS by its name
     */
    private CS_CoordinateSystem getReferenceSystem( String name )
    {
      CS_CoordinateSystem crs = null;
      try
      {
        ConvenienceCSFactory csFactory = ConvenienceCSFactory.getInstance();
        CoordinateSystem cs = csFactory.getCSByName( name );
        crs = Adapters.getDefault().export( cs );
      }
      catch( Exception e )
      {
        System.out.println( e );
      }
      return crs;
    }

    private org.deegree.model.feature.FeatureType createFeatureType( Table table, FeatureType ft,
        boolean spatialField )
    {
      Debug.debugMethodBegin();

      // create features
      String geoFieldName = null;
      GeoFieldIdentifier[] gfi = ft.getMasterTable().getGeoFieldIdentifier();
      if( gfi == null || gfi.length == 0 )
      {
        geoFieldName = "GEOM";
      }
      else
      {
        geoFieldName = gfi[0].getDatastoreFieldBaseName();
      }

      // create feature type
      FeatureTypeProperty[] ftp = null;
      if( spatialField )
      {
        ftp = new FeatureTypeProperty[table.getColumnCount() + 1];
        for( int i = 0; i < table.getColumnCount(); i++ )
        {
          String s = ft.getProperty( table.getColumnName( i ) );
          ftp[i] = FeatureFactory.createFeatureTypeProperty( s, table.getColumnType( i ), true );
        }

        String s = ft.getProperty( geoFieldName );
        ftp[ftp.length - 1] = FeatureFactory.createFeatureTypeProperty( s,
            "org.deegree.model.geometry.GM_Object", true );
      }
      else
      {
        ftp = new FeatureTypeProperty[table.getColumnCount()];
        for( int i = 0; i < table.getColumnCount(); i++ )
        {
          String s = ft.getProperty( table.getColumnName( i ) );
          ftp[i] = FeatureFactory.createFeatureTypeProperty( s, table.getColumnType( i ), true );
        }
      }

      org.deegree.model.feature.FeatureType ftt = FeatureFactory.createFeatureType( null, null, ft
          .getMasterTable().getName(), ftp );
      Debug.debugMethodEnd();
      return ftt;
    }

  }

  /**
   * inner interface defining the processing of a lockFeature request
   */
  private class CLockFeature extends WFSMainLoop
  {
    /**
     * Creates a new CLockFeature object.
     * 
     * @param parent
     * @param request
     */
    public CLockFeature( SDEDataStore parent, OGCWebServiceRequest request )
    {
      super( parent, request );
    }

    /**
     * 
     * 
     * @param request
     * 
     * @return
     */
    protected OGCWebServiceResponse[] performRequest( OGCWebServiceRequest request )
    {
      return null;
    }
  }

  /**
   * class defining the processing of a getFeature request
   */
  class CGetFeatureWithLock extends WFSMainLoop
  {
    /**
     * Creates a new CGetFeatureWithLock object.
     * 
     * @param parent
     * @param request
     */
    public CGetFeatureWithLock( SDEDataStore parent, OGCWebServiceRequest request )
    {
      super( parent, request );
    }

    /**
     * 
     * 
     * @param request
     * 
     * @return
     */
    protected OGCWebServiceResponse[] performRequest( OGCWebServiceRequest request )
    {
      return null;
    }
  }
}