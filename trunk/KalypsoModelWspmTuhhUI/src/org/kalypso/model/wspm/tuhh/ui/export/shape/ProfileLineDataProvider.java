/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraﬂe 22
 *  21073 Hamburg, Germany
 *  http://www.tuhh.de/wb
 * 
 *  and
 *  
 *  Bjoernsen Consulting Engineers (BCE)
 *  Maria Trost 3
 *  56070 Koblenz, Germany
 *  http://www.bjoernsen.de
 * 
 *  This library is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU Lesser General Public
 *  License as published by the Free Software Foundation; either
 *  version 2.1 of the License, or (at your option) any later version.
 * 
 *  This library is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *  Lesser General Public License for more details.
 * 
 *  You should have received a copy of the GNU Lesser General Public
 *  License along with this library; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 * 
 *  Contact:
 * 
 *  E-Mail:
 *  belger@bjoernsen.de
 *  schlienger@bjoernsen.de
 *  v.doemming@tuhh.de
 *   
 *  ---------------------------------------------------------------------------*/
package org.kalypso.model.wspm.tuhh.ui.export.shape;

import java.nio.charset.Charset;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Iterator;

import org.kalypso.model.wspm.core.gml.IProfileFeature;
import org.kalypso.model.wspm.tuhh.core.results.WspmResultLengthSectionColumn;
import org.kalypso.shape.IShapeData;
import org.kalypso.shape.ShapeDataException;
import org.kalypso.shape.dbf.DBFField;
import org.kalypso.shape.dbf.DBaseException;
import org.kalypso.shape.dbf.FieldType;
import org.kalypso.shape.dbf.IDBFValue;
import org.kalypso.shape.deegree.FeatureNameValue;
import org.kalypso.shape.deegree.FeatureValue;
import org.kalypso.shape.deegree.GM_Object2Shape;
import org.kalypso.shape.geometry.ISHPGeometry;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.geometry.GM_Curve;
import org.kalypsodeegree_impl.model.feature.gmlxpath.GMLXPath;

/**
 * @author Gernot Belger
 */
public class ProfileLineDataProvider implements IShapeData
{
  private final IProfileFeature[] m_profiles;

  private final Charset m_charset;

  private final GM_Object2Shape m_shapeConverter;

  private final IDBFValue[] m_fields;

  public ProfileLineDataProvider( final IProfileFeature[] profiles, final Charset charset, final GM_Object2Shape shapeConverter, final WspmResultLengthSectionColumn[] lsColumns )
  {
    m_profiles = profiles;
    m_charset = charset;
    m_shapeConverter = shapeConverter;

    m_fields = fillMapping( lsColumns );
  }

  private IDBFValue[] fillMapping( final WspmResultLengthSectionColumn[] lsColumns )
  {
    final Collection<IDBFValue> fields = new ArrayList<IDBFValue>();
    try
    {
      final DBFField nameField = new DBFField( "NAME", FieldType.C, (short) 50, (short) 0 );
      fields.add( new FeatureNameValue( nameField ) );

      final DBFField descriptionField = new DBFField( "DESCRIPTION", FieldType.C, (short) 128, (short) 0 );
      fields.add( new FeatureValue( descriptionField, new GMLXPath( Feature.QN_DESCRIPTION ) ) );

      final DBFField stationField = new DBFField( "STATION", FieldType.N, (short) 10, (short) 4 );
      fields.add( new ProfileStationValue( stationField ) );

      final DBFField waterField = new DBFField( "WATERBODY", FieldType.C, (short) 30, (short) 0 );
      fields.add( new ProfileWaterValue( waterField ) );

      for( final WspmResultLengthSectionColumn column : lsColumns )
        fields.add( new WspmResultValue( column ) );
    }
    catch( final DBaseException e )
    {
      e.printStackTrace();
    }

    return fields.toArray( new IDBFValue[fields.size()] );
  }

  /**
   * @see org.kalypso.shape.IShapeData#getCharset()
   */
  @Override
  public Charset getCharset( )
  {
    return m_charset;
  }

  /**
   * @see org.kalypso.shape.IShapeData#getCoordinateSystem()
   */
  @Override
  public String getCoordinateSystem( )
  {
    return m_shapeConverter.getCoordinateSystem();
  }

  /**
   * @see org.kalypso.shape.IShapeData#getFields()
   */
  @Override
  public IDBFValue[] getFields( )
  {
    return m_fields;
  }

  /**
   * @see org.kalypso.shape.IShapeData#getGeometry(java.lang.Object)
   */
  @Override
  public ISHPGeometry getGeometry( final Object element ) throws ShapeDataException
  {
    final IProfileFeature profile = (IProfileFeature) element;
    final GM_Curve line = profile.getLine();
    return m_shapeConverter.convert( line );
  }

  /**
   * @see org.kalypso.shape.IShapeData#getShapeType()
   */
  @Override
  public int getShapeType( )
  {
    return m_shapeConverter.getShapeType();
  }

  /**
   * @see org.kalypso.shape.IShapeData#iterator()
   */
  @Override
  public Iterator< ? > iterator( )
  {
    return Arrays.asList( m_profiles ).iterator();
  }

  /**
   * @see org.kalypso.shape.IShapeData#size()
   */
  @Override
  public int size( )
  {
    return m_profiles.length;
  }
}
