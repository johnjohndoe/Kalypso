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
import java.util.Arrays;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Set;

import org.kalypso.model.wspm.core.gml.IProfileFeature;
import org.kalypso.shape.IShapeData;
import org.kalypso.shape.ShapeDataException;
import org.kalypso.shape.dbf.DBFField;
import org.kalypso.shape.dbf.DBaseException;
import org.kalypso.shape.dbf.FieldType;
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

  private final Map<DBFField, IDBFValue> m_mapping = new LinkedHashMap<DBFField, IDBFValue>();

  public ProfileLineDataProvider( final IProfileFeature[] profiles, final Charset charset, final GM_Object2Shape shapeConverter )
  {
    m_profiles = profiles;
    m_charset = charset;
    m_shapeConverter = shapeConverter;

    fillMapping();

  }

  private void fillMapping( )
  {
    try
    {
      final DBFField nameField = new DBFField( "NAME", FieldType.C, (short) 50, (short) 0 );
      m_mapping.put( nameField, new FeatureNameValue( nameField ) );

      final DBFField descriptionField = new DBFField( "DESCRIPTION", FieldType.C, (short) 128, (short) 0 );
      m_mapping.put( descriptionField, new FeatureValue( descriptionField, new GMLXPath( Feature.QN_DESCRIPTION ) ) );

      final DBFField stationField = new DBFField( "STATION", FieldType.N, (short) 10, (short) 4 );
      m_mapping.put( stationField, new ProfileStationValue( stationField ) );

      final DBFField waterField = new DBFField( "WATERBODY", FieldType.C, (short) 30, (short) 0 );
      m_mapping.put( waterField, new ProfileWaterValue( waterField ) );

      // TODO other stuff...

      // TODO: results

    }
    catch( final DBaseException e )
    {
      e.printStackTrace();
    }
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
   * @see org.kalypso.shape.IShapeData#getData(java.lang.Object, int)
   */
  @Override
  public Object getData( final Object element, final int field ) throws ShapeDataException
  {
    final DBFField[] fields = getFields();
    final IDBFValue dbfValue = m_mapping.get( fields[field] );
    return dbfValue.getValue( element );
  }

  /**
   * @see org.kalypso.shape.IShapeData#getFields()
   */
  @Override
  public DBFField[] getFields( )
  {
    final Set<DBFField> keySet = m_mapping.keySet();
    return keySet.toArray( new DBFField[keySet.size()] );
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
