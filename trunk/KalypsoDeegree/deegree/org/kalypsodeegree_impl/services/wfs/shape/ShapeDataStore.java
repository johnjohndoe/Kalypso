// $Header:
// /cvsroot/deegree/deegree/org/deegree_impl/services/wfs/shape/ShapeDataStore.java,v
// 1.29 2004/06/07 11:49:35 poth Exp $
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
package org.deegree_impl.services.wfs.shape;

import java.net.URL;
import java.util.ArrayList;
import java.util.HashMap;

import org.deegree.model.feature.Feature;
import org.deegree.model.feature.FeatureProperty;
import org.deegree.model.feature.FeatureTypeProperty;
import org.deegree.model.geometry.GM_Envelope;
import org.deegree.model.geometry.GM_Object;
import org.deegree.services.OGCWebServiceException;
import org.deegree.services.OGCWebServiceRequest;
import org.deegree.services.OGCWebServiceResponse;
import org.deegree.services.wfs.DataStoreException;
import org.deegree.services.wfs.WFSConstants;
import org.deegree.services.wfs.configuration.FeatureType;
import org.deegree.services.wfs.filterencoding.Filter;
import org.deegree.services.wfs.protocol.WFSDescribeFeatureTypeRequest;
import org.deegree.services.wfs.protocol.WFSGetFeatureRequest;
import org.deegree.services.wfs.protocol.WFSGetFeatureWithLockRequest;
import org.deegree.services.wfs.protocol.WFSLockFeatureRequest;
import org.deegree.services.wfs.protocol.WFSQuery;
import org.deegree.services.wfs.protocol.WFSTransactionRequest;
import org.deegree.tools.ParameterList;
import org.deegree_impl.io.shpapi.KeyNotFoundException;
import org.deegree_impl.io.shpapi.ShapeFile;
import org.deegree_impl.model.cs.ConvenienceCSFactory;
import org.deegree_impl.model.feature.FeatureFactory;
import org.deegree_impl.model.geometry.GM_Object_Impl;
import org.deegree_impl.services.OGCWebServiceException_Impl;
import org.deegree_impl.services.wfs.AbstractDataStore;
import org.deegree_impl.services.wfs.AbstractDescribeFeatureType;
import org.deegree_impl.services.wfs.AbstractGetFeature;
import org.deegree_impl.services.wfs.WFSMainLoop;
import org.deegree_impl.services.wfs.filterencoding.ComplexFilter;
import org.deegree_impl.services.wfs.filterencoding.FeatureFilter;
import org.deegree_impl.services.wfs.filterencoding.FeatureId;
import org.deegree_impl.services.wfs.filterencoding.FilterTools;
import org.deegree_impl.services.wfs.filterencoding.OperationDefines;
import org.deegree_impl.services.wfs.protocol.WFSProtocolFactory;
import org.deegree_impl.tools.Cache_Impl;
import org.deegree_impl.tools.Debug;
import org.deegree_impl.tools.ParameterList_Impl;
import org.opengis.cs.CS_CoordinateSystem;
import org.w3c.dom.Document;

/**
 * The class provides reading and writing access to ESRI shapefiles. The access
 * is capsulated within the query and transaction mechanism described at the OGC
 * WFS specifications.
 * <p>
 * </p>
 * The data store uses a cache for keeping features once read from a shapefile
 * in memory to provide a much faster access to them. The cache can be
 * configured through the configuration XML-document that also contains the
 * names and locations of the shapes handled by an instance of the
 * <tt>ShapeDataStore</tt>.
 * <p>
 * 
 * @author <a href="mailto:poth@lat-lon.de">Andreas Poth </a>
 * @author <a href="mailto:mschneider@lat-lon.de">Markus Schneider </a>
 * @version $Revision$ $Date$
 */
public class ShapeDataStore extends AbstractDataStore
{
  protected static Cache_Impl cache = null;

  static
  {
    cache = new Cache_Impl();
    cache.setMaxEntries( 50000 );
  }

  /**
   * Creates a new ShapeDataStore object.
   * 
   * @param config
   * 
   * @throws DataStoreException
   */
  public ShapeDataStore( URL config ) throws DataStoreException
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
   * @see #getFeature(WFSGetFeatureRequest)
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
    public CDescribeFeatureType( ShapeDataStore parent, OGCWebServiceRequest request )
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
    public CGetFeature( ShapeDataStore parent, OGCWebServiceRequest request )
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

            // create one filter-object that contains the
            // operations from both filters (if two filters are given)
            Filter filter = null;
            Filter filter1 = req.getFilter();
            Filter filter2 = queries[i].getFilter();

            if( ( filter1 != null ) && ( filter2 != null ) )
            {
              filter = new ComplexFilter( (ComplexFilter)filter1, (ComplexFilter)filter2,
                  OperationDefines.AND );
            }
            else if( filter1 != null )
            {
              filter = filter1;
            }
            else if( filter2 != null )
            {
              filter = filter2;
            }

            // perform query and return result as table model
            Feature[] features = getFeatures( queries[i].getTypeName(), filter, startPosition,
                maxFeatures );

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
     * Collects the <tt>Feature</tt> s from the shape file that match the
     * given <tt>Filter</tt>.
     * <p>
     * To make use of the indexing of the shapefile, the <tt>Filter</tt> is
     * examined and if a BBOX-Operation is found it is used on the index of the
     * shapefile.
     * <p>
     * 
     * @param typeName
     *          name of the FeatureType
     * @param filter
     * @param startPosition
     * @param maxFeatures
     * @return @throws
     *         Exception
     */
    private Feature[] getFeatures( String typeName, Filter filter, int startPosition,
        int maxFeatures ) throws Exception
    {
      Debug.debugMethodBegin( this, "getFeatures" );

      // initialize shape file access
      FeatureType ft = config.getFeatureType( typeName );
      String tn = ft.getMasterTable().getTargetName();
      ShapeFile sf = new ShapeFile( ft.getMasterTable().getName() );

      // get IDs of the relevant shapes
      int[] shapeIds = getShapeIds( sf, ft, filter, startPosition, maxFeatures );
      if( shapeIds == null || shapeIds.length == 0 )
      {
        return new Feature[0];
      }

      //only drop first bbox if ComplexFilter (!=FeatureFilter)
      if( filter instanceof ComplexFilter )
      {
        Object[] objects = FilterTools.extractFirstBBOX( (ComplexFilter)filter );
        filter = (Filter)objects[1];
      }

      // check parameters for sanity
      if( startPosition < 0 )
      {
        startPosition = 0;
      }

      if( ( maxFeatures < 0 ) || ( maxFeatures >= shapeIds.length ) )
      {
        maxFeatures = shapeIds.length;
      }

      // get coordinate system associated to the feature type
      CS_CoordinateSystem crs = ConvenienceCSFactory.getInstance().getOGCCSByName( ft.getCRS() );

      ArrayList features = new ArrayList( maxFeatures );

      org.deegree.model.feature.FeatureType featureTypeOrg = sf.getFeatureByRecNo( shapeIds[0] )
          .getFeatureType();
      org.deegree.model.feature.FeatureType featureTypeMapped = recreateFeatureType(
          featureTypeOrg, ft );

      // determine the index of the IDField in the array of shapefile-properties
      String[] properties = sf.getProperties();
      String idField = ft.getMasterTable().getIdField();
      int idFieldIndex = -1;
      if( idField != null )
      {
        for( int i = 0; i < properties.length; i++ )
        {
          if( idField.equalsIgnoreCase( properties[i] ) )
          {
            idFieldIndex = i;
            break;
          }
        }
      }
      String idValue = null;

      // collect features that match the filter
      for( int i = startPosition; i < maxFeatures; i++ )
      {
        Feature feature = (Feature)cache.get( tn + shapeIds[i] );

        if( feature == null )
        {
          Object[] dbfProps = sf.getRow( shapeIds[i] );

          if( idFieldIndex >= 0 )
          {
            idValue = dbfProps[idFieldIndex].toString();
          }
          else
          {
            idValue = tn + shapeIds[i];
          }

          GM_Object geo = sf.getGM_ObjectByRecNo( shapeIds[i] );

          Object[] shapeProps = new Object[featureTypeOrg.getProperties().length];
          System.arraycopy( dbfProps, 0, shapeProps, 0, dbfProps.length );
          shapeProps[dbfProps.length] = geo;

          feature = FeatureFactory.createFeature( idValue, featureTypeOrg, shapeProps );
          feature = recreateFeature( feature, featureTypeMapped );
          cache.push( tn + shapeIds[i], feature );
        }

        // check the feature against the filter
        if( ( filter == null ) || filter.evaluate( feature ) )
        {
          GM_Object[] gp = feature.getGeometryProperties();
          if( gp.length > 0 && gp[0] != null )
          {
            ( (GM_Object_Impl)gp[0] ).setCoordinateSystem( crs );
          }

          features.add( feature );
        }
      }

      sf.close();
      Debug.debugMethodEnd();

      // create and return feature array
      return (Feature[])features.toArray( new Feature[features.size()] );
    }
  }

  private int[] getShapeIds( ShapeFile sf, FeatureType ft, Filter filter, int startPosition,
      int maxFeatures ) throws Exception
  {
    Debug.debugMethodBegin();

    int[] shapeIds = null;
    GM_Envelope bbox = null;

    String idField = ft.getMasterTable().getIdField();
    boolean hasIdIndex = false;
    if( idField != null )
    {
      hasIdIndex = sf.hasDBaseIndex( idField );
    }

    if( filter instanceof ComplexFilter )
    {
      Object[] objects = FilterTools.extractFirstBBOX( (ComplexFilter)filter );
      bbox = (GM_Envelope)objects[0];
      filter = (Filter)objects[1];

      if( bbox == null )
      {
        // TODO: see if filter includes PropertyIsEqualTo-operator on indexed
        // column
        int num = sf.getRecordNum();
        shapeIds = new int[num];

        for( int i = 0; i < num; i++ )
        {
          shapeIds[i] = i + 1;
        }
      }
      else
      {
        // get ids of geometries that are inside the BBOX
        shapeIds = sf.getGeoNumbersByRect( bbox );

        if( ( shapeIds == null ) || ( shapeIds.length < 1 ) || ( startPosition >= shapeIds.length ) )
        {
          sf.close();
          return new int[0];
        }
      }
      // use FeatureFilter if IDField is indexed
    }
    else if( filter instanceof FeatureFilter && hasIdIndex )
    {

      ArrayList featureIds = ( (FeatureFilter)filter ).getFeatureIds();
      int[] recsTemp = new int[featureIds.size()];
      int noofRecs = 0;

      for( int i = 0; i < featureIds.size(); i++ )
      {
        String idValue = ( (FeatureId)featureIds.get( i ) ).getValue();
        Comparable keyValue = null;
        String[] fields = new String[1];
        fields[0] = idField;
        String[] dataTypes = sf.getDataTypes( fields );
        if( dataTypes[0].equalsIgnoreCase( "C" ) )
          keyValue = idValue;
        else
          keyValue = new Double( idValue );
        int[] recs = null;

        try
        {
          recs = sf.getGeoNumbersByAttribute( idField, keyValue );
        }
        //non-existent feature-ids may be specified
        catch( KeyNotFoundException keyNotFound )
        {}
        if( recs != null )
        {
          if( recs.length != 1 )
          {
            throw new Exception( "index on IDField " + idField + " not unique" );
          }
          recsTemp[noofRecs] = recs[0];
          noofRecs++;
        }
      }
      shapeIds = new int[noofRecs];
      System.arraycopy( recsTemp, 0, shapeIds, 0, noofRecs );

    }
    else
    {
      int num = sf.getRecordNum();
      shapeIds = new int[num];

      for( int i = 0; i < num; i++ )
      {
        shapeIds[i] = i + 1;
      }
    }

    Debug.debugMethodEnd();

    return shapeIds;
  }

  /**
   * recreates the passed <tt>org.deegree.model.feature.FeatureType</tt> by
   * performing a mapping between shapefile/dbase property names and user
   * defined property names.
   * 
   * @param featureType
   *          feature type to be re-created with new property names
   * @param ft
   *          feature type configuration containing mapping informations
   * @return feature type with new (mapped) property names
   */
  private org.deegree.model.feature.FeatureType recreateFeatureType(
      org.deegree.model.feature.FeatureType featureType, FeatureType ft )
  {
    FeatureTypeProperty[] ftp = featureType.getProperties();
    FeatureTypeProperty[] nftp = new FeatureTypeProperty[ftp.length];
    for( int i = 0; i < ftp.length; i++ )
    {
      String name = ft.getProperty( ftp[i].getName() );
      if( name == null )
        name = ftp[i].getName();
      nftp[i] = FeatureFactory.createFeatureTypeProperty( name, ftp[i].getType(), true );
    }
    return FeatureFactory.createFeatureType( null, null, featureType.getName(), nftp );
  }

  /**
   * recreates the passed <tt>org.deegree.model.feature.Feature</tt> by
   * performing a mapping between shapefile/dbase property names and user
   * defined property names.
   * 
   * @param feature
   *          feature to be assigend with a new feature type
   * @param featureType
   *          new feature type of the re-created feature
   * @return feature with renamed properties
   */
  private Feature recreateFeature( Feature feature,
      org.deegree.model.feature.FeatureType featureType )
  {
    FeatureTypeProperty[] ftp = feature.getFeatureType().getProperties();
    FeatureProperty[] fp = new FeatureProperty[ftp.length];
    for( int i = 0; i < fp.length; i++ )
    {
      fp[i] = FeatureFactory.createFeatureProperty( ftp[i].getName(), feature.getProperty( ftp[i]
          .getName() ) );
    }
    return FeatureFactory.createFeature( feature.getId(), featureType, feature.getProperties() );
  }

  /**
   * inner interface defining the processing of a transaction request
   */
  private class CTransaction extends WFSMainLoop
  {
    /**
     * Creates a new CTransaction object.
     * 
     * @param parent
     * @param request
     */
    public CTransaction( ShapeDataStore parent, OGCWebServiceRequest request )
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
    public CLockFeature( ShapeDataStore parent, OGCWebServiceRequest request )
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
    public CGetFeatureWithLock( ShapeDataStore parent, OGCWebServiceRequest request )
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
/*******************************************************************************
 * Changes to this class. What the people have been up to: $Log:
 * ShapeDataStore.java,v $ Revision 1.29 2004/06/07 11:49:35 poth no message
 * 
 * Revision 1.28 2004/04/27 15:40:38 poth no message
 * 
 * Revision 1.27 2004/04/07 06:43:50 poth no message
 * 
 * Revision 1.26 2004/04/06 08:17:18 poth no message
 * 
 * Revision 1.25 2004/04/05 07:36:40 poth no message
 * 
 * Revision 1.24 2004/03/22 11:44:34 poth no message
 * 
 * 
 *  
 ******************************************************************************/