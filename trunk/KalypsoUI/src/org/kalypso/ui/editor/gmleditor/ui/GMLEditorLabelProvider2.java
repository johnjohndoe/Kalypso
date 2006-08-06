/*
 * --------------- Kalypso-Header --------------------------------------------------------------------
 * 
 * This file is part of kalypso. Copyright (C) 2004, 2005 by:
 * 
 * Technical University Hamburg-Harburg (TUHH) Institute of River and coastal engineering Denickestr. 22 21073 Hamburg,
 * Germany http://www.tuhh.de/wb
 * 
 * and
 * 
 * Bjoernsen Consulting Engineers (BCE) Maria Trost 3 56070 Koblenz, Germany http://www.bjoernsen.de
 * 
 * This library is free software; you can redistribute it and/or modify it under the terms of the GNU Lesser General
 * Public License as published by the Free Software Foundation; either version 2.1 of the License, or (at your option)
 * any later version.
 * 
 * This library is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied
 * warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public License for more
 * details.
 * 
 * You should have received a copy of the GNU Lesser General Public License along with this library; if not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
 * 
 * Contact:
 * 
 * E-Mail: belger@bjoernsen.de schlienger@bjoernsen.de v.doemming@tuhh.de
 * 
 * ---------------------------------------------------------------------------------------------------
 */
package org.kalypso.ui.editor.gmleditor.ui;

import java.util.HashMap;
import java.util.Map;

import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.swt.graphics.Image;
import org.kalypso.contribs.java.lang.DisposeHelper;
import org.kalypso.gmlschema.adapter.IAnnotation;
import org.kalypso.gmlschema.property.IValuePropertyType;
import org.kalypso.ogc.gml.AnnotationUtilities;
import org.kalypso.ui.ImageProvider;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.geometry.GM_Object;
import org.kalypsodeegree_impl.model.feature.FeatureHelper;
import org.kalypsodeegree_impl.tools.GeometryUtilities;

/**
 * @author kuepfer
 */
public class GMLEditorLabelProvider2 extends LabelProvider
{
  private Map<ImageDescriptor, Image> m_images = new HashMap<ImageDescriptor, Image>( 20 );

  /**
   * @see org.eclipse.jface.viewers.LabelProvider#dispose()
   */
  @Override
  public void dispose( )
  {
    super.dispose();

    new DisposeHelper( m_images.values() ).dispose();
    m_images.clear();
  }

  /**
   * @see org.eclipse.jface.viewers.ILabelProvider#getImage(java.lang.Object)
   */
  @Override
  public Image getImage( final Object element )
  {
    ImageDescriptor descriptor = null;
    if( element instanceof Feature )
      descriptor = ImageProvider.IMAGE_FEATURE;
    else if( element instanceof FeatureAssociationTypeElement )
      descriptor = ImageProvider.IMAGE_FEATURE_RELATION_COMPOSITION;
    else if( element instanceof LinkedFeatureElement2 )
      descriptor = ImageProvider.IMAGE_FEATURE_LINKED;
    else if( element instanceof IValuePropertyType )
    {
      final IValuePropertyType vpt = (IValuePropertyType) element;
      if( GeometryUtilities.isPointGeometry( vpt ) )
        descriptor = ImageProvider.IMAGE_GEOM_PROP_POINT;
      if( GeometryUtilities.isMultiPointGeometry( vpt ) )
        descriptor = ImageProvider.IMAGE_GEOM_PROP_MULTIPOINT;
      if( GeometryUtilities.isLineStringGeometry( vpt ) )
        descriptor = ImageProvider.IMAGE_GEOM_PROP_LINE;
      if( GeometryUtilities.isMultiLineStringGeometry( vpt ) )
        descriptor = ImageProvider.IMAGE_GEOM_PROP_MULTILINE;
      if( GeometryUtilities.isPolygonGeometry( vpt ) )
        descriptor = ImageProvider.IMAGE_GEOM_PROP_POLYGON;
      if( GeometryUtilities.isMultiPolygonGeometry( vpt ) )
        descriptor = ImageProvider.IMAGE_GEOM_PROP_MULTIPOLYGON;
    }
    else if( GeometryUtilities.getPolygonClass().isAssignableFrom( element.getClass() ) )
      descriptor = ImageProvider.IMAGE_GEOM_PROP_POLYGON;
    else
      return null;

    if( m_images.containsKey( descriptor ) )
      return m_images.get( descriptor );

    final Image createImage = descriptor.createImage();
    m_images.put( descriptor, createImage );

    return createImage;
  }

  /**
   * @see org.eclipse.jface.viewers.ILabelProvider#getText(java.lang.Object)
   */
  @Override
  public String getText( final Object element )
  {
    if( element instanceof GMLWorkspace )
      return "GML";
    
    if( element instanceof Feature )
      return FeatureHelper.getLabel( (Feature) element );

    if( element instanceof FeatureAssociationTypeElement )
    {
      final IAnnotation annotation = AnnotationUtilities.getAnnotation( ((FeatureAssociationTypeElement) element).getAssociationTypeProperty() );
      if( annotation != null )
        return annotation.getLabel();
      return "<-> ";
    }
    
    if( element instanceof LinkedFeatureElement2 )
    {
      final Feature decoratedFeature = ((LinkedFeatureElement2) element).getDecoratedFeature();
      return "-> " + getText( decoratedFeature );
    }
    
    if( element instanceof IValuePropertyType )
    {
      IValuePropertyType vpt = (IValuePropertyType) element;
      return vpt.getValueClass().getName().replaceAll( ".+\\.", "" );
    }
    
    if( element instanceof GM_Object )
      return element.getClass().getName().replaceAll( ".+\\.", "" );

    return "unknown";
  }
}
