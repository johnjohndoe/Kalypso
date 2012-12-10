/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestra�e 22
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
package org.kalypso.kalypsomodel1d2d.conv;

import java.io.File;

import javax.xml.namespace.QName;

import org.kalypso.gmlschema.GMLSchemaFactory;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.IPropertyType;
import org.kalypso.gmlschema.property.IValuePropertyType;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypso.gmlschema.types.IMarshallingTypeHandler;
import org.kalypso.gmlschema.types.ITypeRegistry;
import org.kalypso.gmlschema.types.MarshallingTypeRegistrySingleton;
import org.kalypso.ogc.gml.serialize.GmlSerializeException;
import org.kalypso.ogc.gml.serialize.ShapeSerializer;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.geometry.GM_Surface;
import org.kalypsodeegree_impl.io.shpapi.ShapeConst;
import org.kalypsodeegree_impl.io.shpapi.dataprovider.SurfacePolygonZShapeDataProvider;
import org.kalypsodeegree_impl.model.feature.FeatureFactory;
import org.kalypsodeegree_impl.tools.GMLConstants;

/**
 * @author Thomas Jung
 */
public class SHPModelImporter implements IDiscModelImporter
{

  private final File m_file;

  private final GMLWorkspace m_workspace;

  private final Feature m_shapeRootFeature;

  private final IRelationType m_shapeParentRelation;

  private final IFeatureType m_shapeFT;

  private int m_count = 0;

  public SHPModelImporter( final File outputFile )
  {
    m_file = outputFile;
    /* Create feature type which describes what data the shape file contains */
    final ITypeRegistry<IMarshallingTypeHandler> typeRegistry = MarshallingTypeRegistrySingleton.getTypeRegistry();

    //    final IMarshallingTypeHandler doubleTypeHandler = typeRegistry.getTypeHandlerForTypeName( XmlTypes.XS_DOUBLE ); //$NON-NLS-1$
    //    final IMarshallingTypeHandler intTypeHandler = typeRegistry.getTypeHandlerForTypeName( XmlTypes.XS_INTEGER ); //$NON-NLS-1$
    //    final IMarshallingTypeHandler stringTypeHandler = typeRegistry.getTypeHandlerForTypeName( XmlTypes.XS_STRING ); //$NON-NLS-1$
    final IMarshallingTypeHandler polygonTypeHandler = typeRegistry.getTypeHandlerForTypeName( GMLConstants.QN_POLYGON );

    final QName shapeTypeQName = new QName( "anyNS", "shapeType" ); //$NON-NLS-1$ //$NON-NLS-2$

    //    final IValuePropertyType doubleType = GMLSchemaFactory.createValuePropertyType( new QName( "anyNS", "aNumber" ), doubleTypeHandler, 1, 1, false ); //$NON-NLS-1$ //$NON-NLS-2$
    //    final IValuePropertyType intType = GMLSchemaFactory.createValuePropertyType( new QName( "anyNS", "aNumber" ), intTypeHandler, 1, 1, false ); //$NON-NLS-1$ //$NON-NLS-2$
    //    final IValuePropertyType stringType = GMLSchemaFactory.createValuePropertyType( new QName( "anyNS", "aString" ), stringTypeHandler, 1, 1, false ); //$NON-NLS-1$ //$NON-NLS-2$
    final IValuePropertyType polygonType = GMLSchemaFactory.createValuePropertyType( new QName( "anyNS", "aGeometry" ), polygonTypeHandler, 1, 1, false ); //$NON-NLS-1$ //$NON-NLS-2$

    final IPropertyType[] properties = new IPropertyType[] { polygonType };
    m_shapeFT = GMLSchemaFactory.createFeatureType( shapeTypeQName, properties );

    m_shapeRootFeature = ShapeSerializer.createWorkspaceRootFeature( m_shapeFT, ShapeConst.SHAPE_TYPE_POLYGONZ );
    m_workspace = m_shapeRootFeature.getWorkspace();
    m_shapeParentRelation = (IRelationType) m_shapeRootFeature.getFeatureType().getProperty( ShapeSerializer.PROPERTY_FEATURE_MEMBER );
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.conv.IDiscModelImporter#addElement(org.kalypsodeegree.model.geometry.GM_SurfacePatch)
   */
  @Override
  public void addElement( final GM_Surface surface )
  {
    try
    {
      // final double aDouble = i * Math.PI;
      //    final String aString = "Item Number: " + i; //$NON-NLS-1$
      // final GM_Point aPoint = GeometryFactory.createGM_Point( i * 4.3, i * 2.1, crs );
      m_count++;
      final Object[] data = new Object[] { surface };
      final Feature feature = FeatureFactory.createFeature( m_shapeRootFeature, m_shapeParentRelation, "Feature_" + m_count, m_shapeFT, data ); //$NON-NLS-1$
      m_workspace.addFeatureAsComposition( m_shapeRootFeature, m_shapeParentRelation, -1, feature );
    }
    catch( final Exception e )
    {
      // TODO Auto-generated catch block
      e.printStackTrace();
    }
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.conv.IDiscModelImporter#finish()
   */
  @Override
  public void finish( )
  {
    try
    {
      ShapeSerializer.serialize( m_workspace, m_file.getAbsolutePath(), new SurfacePolygonZShapeDataProvider( m_workspace.getFeatures( m_shapeFT ), ShapeConst.SHAPE_TYPE_POLYGONZ ) );
      System.out.println( "Wrote shapeFile to:" + m_file.getAbsolutePath() ); //$NON-NLS-1$
    }
    catch( final GmlSerializeException e )
    {
      // TODO Auto-generated catch block
      e.printStackTrace();
    }

  }

}
