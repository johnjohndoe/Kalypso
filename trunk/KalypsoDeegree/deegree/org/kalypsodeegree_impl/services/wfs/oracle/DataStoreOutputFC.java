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
package org.deegree_impl.services.wfs.oracle;

import java.io.ByteArrayOutputStream;
import java.util.HashMap;
import java.util.Iterator;

import org.deegree.model.feature.Feature;
import org.deegree.model.feature.FeatureCollection;
import org.deegree.model.feature.FeatureProperty;
import org.deegree.model.feature.FeatureTypeProperty;
import org.deegree.model.geometry.ByteUtils;
import org.deegree.model.geometry.GM_Exception;
import org.deegree.model.geometry.GM_Object;
import org.deegree.model.table.Table;
import org.deegree.services.wfs.DataStoreOutputFormat;
import org.deegree.services.wfs.WFSConstants;
import org.deegree.services.wfs.configuration.FeatureType;
import org.deegree.services.wfs.configuration.TableDescription;
import org.deegree.tools.Parameter;
import org.deegree.tools.ParameterList;
import org.deegree_impl.model.cs.Adapters;
import org.deegree_impl.model.cs.ConvenienceCSFactory;
import org.deegree_impl.model.cs.CoordinateSystem;
import org.deegree_impl.model.feature.FeatureFactory;
import org.deegree_impl.model.geometry.GM_SurfaceInterpolation_Impl;
import org.deegree_impl.model.geometry.GeometryFactory;
import org.deegree_impl.tools.Debug;
import org.opengis.cs.CS_CoordinateSystem;

/**
 * Implements the DataStoreOutputFormat interface to format the result of a data
 * accessing class returned within the values of a HashMap as deegree feature
 * collection
 * 
 * <p>
 * -----------------------------------------------------------------------
 * </p>
 * 
 * @author <a href="mailto:poth@lat-lon.de">Andreas Poth </a>
 * @version $Revision$ $Date$
 *          <p>
 */
public class DataStoreOutputFC implements DataStoreOutputFormat
{

  /**
   * @see org.deegree.services.wfs.DataStoreOutputFormat#format(java.util.HashMap,
   *      org.deegree.tools.ParameterList)
   */
  public Object format( HashMap map, ParameterList parameter ) throws Exception
  {
    Iterator iterator = map.values().iterator();
    ParameterList pl = (ParameterList)iterator.next();
    FeatureCollection fc = (FeatureCollection)pl.getParameter( WFSConstants.FEATURECOLLECTION )
        .getValue();
    while( iterator.hasNext() )
    {
      pl = (ParameterList)iterator.next();
      fc.appendFeatures( (FeatureCollection)pl.getParameter( WFSConstants.FEATURECOLLECTION )
          .getValue() );
    }
    return fc;
  }

  /**
   * formats the data store at the values of the HashMap into one single data
   * structure.
   */
  public Object formatTable( HashMap map, ParameterList parameter ) throws Exception
  {
    Debug.debugMethodBegin();
    Iterator iterator = map.values().iterator();
    int initcap = 0;
    while( iterator.hasNext() )
    {
      ParameterList pl = (ParameterList)iterator.next();
      Parameter p = pl.getParameter( WFSConstants.TABLE );
      Table table = (Table)p.getValue();
      initcap += table.getRowCount();
    }

    FeatureCollection fc = FeatureFactory.createFeatureCollection( "" + map.hashCode(), initcap );

    // get iterator to iterate each feature collection contained within
    // the HashMap
    iterator = map.values().iterator();

    while( iterator.hasNext() )
    {

      ParameterList pl = (ParameterList)iterator.next();
      Parameter p = pl.getParameter( WFSConstants.TABLE );
      Table table = (Table)p.getValue();
      tableToFC( table, pl, fc );

    }

    Debug.debugMethodEnd();
    return fc;
  }

  /**
   * creates a <tt>FeatureCollection</tt> from a <tt>Table</tt>. the method
   * is recursivly called to create complex features.
   */
  private FeatureCollection tableToFC( Table table, ParameterList pl, FeatureCollection fc )
      throws Exception
  {
    Debug.debugMethodBegin();

    String tableName = table.getTableName();

    // create coordinate reference system for the tables (feature types)
    // geometries
    String crs = (String)pl.getParameter( WFSConstants.CRS ).getValue();
    ConvenienceCSFactory fac = ConvenienceCSFactory.getInstance();
    CoordinateSystem cs_ = fac.getCSByName( crs );
    CS_CoordinateSystem cs = Adapters.getDefault().export( cs_ );

    FeatureType ft = (FeatureType)pl.getParameter( WFSConstants.FEATURETYPE ).getValue();
    String[] columnNames = table.getColumnNames();
    TableDescription td = ft.getTableByName( tableName );

    org.deegree.model.feature.FeatureType ftype = createFeatureType( ft, table );

    // get id property name
    String idProp = td.getIdField();

    FeatureProperty[] fp = new FeatureProperty[ftype.getProperties().length];

    // for each row a feature will be created and added to
    // the feature collection
    for( int r = 0; r < table.getRowCount(); r++ )
    {

      String id = "" + r;
      Object[] row = table.getRow( r );
      int k = 0;
      for( int i = 0; i < row.length; i++ )
      {

        String pn = null;
        if( columnNames[i].equals( "COUNTCOUNT" ) )
        {
          // if a count(*) statement has been performed set the property
          // name to "_COUNT_"
          pn = "_COUNT_";
        }
        else
        {
          String s = ft.getPropertyFromAlias( columnNames[i] );
          if( s == null )
            s = ft.getPropertyFromAlias( tableName + "." + columnNames[i] );
          if( s == null )
            s = tableName + "." + columnNames[i];
          pn = s;
        }
        // get feature id
        if( columnNames[i].equalsIgnoreCase( idProp ) )
          id = row[i].toString();

        if( !( row[i] instanceof ByteArrayOutputStream ) )
        {
          if( row[i] instanceof Table )
          {
            // create and add complex property to the feature
            TableDescription td_ = ft.getTableByName( ( (Table)row[i] ).getTableName() );
            FeatureCollection fc_ = FeatureFactory.createFeatureCollection( td_.getTargetName(),
                1000 );
            fc_ = tableToFC( (Table)row[i], pl, fc_ );
            fp[k++] = FeatureFactory.createFeatureProperty( pn, fc_ );
          }
          else
          {
            // create and add none-geometry property to the feature
            fp[k++] = FeatureFactory.createFeatureProperty( pn, row[i] );
          }
        }
        else
        {
          GM_Object geo = null;
          if( row[i] != null )
          {
            // create and add geometry property to the feature
            try
            {
              ByteArrayOutputStream bos = (ByteArrayOutputStream)row[i];
              geo = createGeometry( bos, cs );
            }
            catch( Exception e )
            {
              throw new Exception( "couldn't create geo property \n" + e );
            }
          }
          fp[k++] = FeatureFactory.createFeatureProperty( pn, geo );

        }
      }

      // add feature to the feature collection
      Feature feature = FeatureFactory.createFeature( id, ftype, fp );
      fc.appendFeature( feature );

    }

    Debug.debugMethodEnd();

    return fc;
  }

  /**
   * returns the feature type calculated from the column names and column types
   * of the submitted table
   */
  private org.deegree.model.feature.FeatureType createFeatureType( FeatureType ft, Table table )
  {
    String[] columnNames = table.getColumnNames();
    String[] columnTypes = table.getColumnTypes();

    TableDescription td = ft.getTableByName( table.getTableName() );

    // feature type without geo property
    FeatureTypeProperty[] ftp = new FeatureTypeProperty[columnTypes.length];

    int k = 0;
    for( int i = 0; i < columnNames.length; i++ )
    {

      String pn = columnNames[i];
      if( pn == null )
        continue;
      if( columnNames[i].equals( "COUNTCOUNT" ) )
      {
        // if a count(*) statement has been performed set the property
        // name to "_COUNT_"
        pn = "_COUNT_";
      }
      else
      {
        String s = ft.getPropertyFromAlias( columnNames[i] );
        if( s == null )
          s = ft.getPropertyFromAlias( table.getTableName() + "." + columnNames[i] );
        if( s == null )
          s = table.getTableName() + "." + columnNames[i];
        pn = s;
      }

      if( td.getGeoFieldIdentifier( columnNames[i] ) == null )
      {
        if( columnTypes[i].equals( "org.deegree.model.table.Table" ) )
        {
          // if the current column is complex (type == table) create
          // a FeatureTypeProperty with type == FeatureCollection
          ftp[k++] = FeatureFactory.createFeatureTypeProperty( pn,
              "org.deegree.model.feature.FeatureCollection", true );
        }
        else
        {
          // create a FeatureTypeProperty with type of the current
          // column
          ftp[k++] = FeatureFactory.createFeatureTypeProperty( pn, columnTypes[i], true );
        }
      }
      else
      {
        // create a FeatureTypeProperty with geometry type
        ftp[k++] = FeatureFactory.createFeatureTypeProperty( pn,
            "org.deegree.model.geometry.GM_Object", true );
      }
    }

    return FeatureFactory.createFeatureType( null, null, td.getTargetName(), ftp );
  }

  /**
   * creates FeatureCollections from OGC WKBs
   * 
   * @param bos
   *          wkb datastructure
   * @param srs
   *          spatial reference system used for each feature within the feature
   *          collection.
   */
  private GM_Object createGeometry( ByteArrayOutputStream bos, CS_CoordinateSystem srs )
      throws Exception
  {
    Debug.debugMethodBegin( this, "createGeometry" );

    GM_Object geo = null;

    byte[] wkb = bos.toByteArray();
    bos.close();

    // create geometries from the wkb considering the geomerty typ
    switch( getGeometryType( wkb ) )
    {
    case 1:
      geo = GeometryFactory.createGM_Point( wkb, srs );
      break;
    case 2:
      geo = GeometryFactory.createGM_Curve( wkb, srs );
      break;
    case 3:
      geo = GeometryFactory.createGM_Surface( wkb, srs, new GM_SurfaceInterpolation_Impl( 1 ) );
      break;
    case 4:
      geo = GeometryFactory.createGM_MultiPoint( wkb, srs );
      break;
    case 5:
      geo = GeometryFactory.createGM_MultiCurve( wkb, srs );
      break;
    case 6:
      geo = GeometryFactory.createGM_MultiSurface( wkb, srs, new GM_SurfaceInterpolation_Impl( 1 ) );
      break;
    default:
      geo = null;
    }

    Debug.debugMethodEnd();
    return geo;
  }

  private int getGeometryType( byte[] wkb ) throws GM_Exception
  {
    int wkbType = 0;
    byte byteorder = wkb[0];

    if( byteorder == 0 )
      wkbType = ByteUtils.readBEInt( wkb, 1 );
    else
      wkbType = ByteUtils.readLEInt( wkb, 1 );

    return wkbType;
  }

}