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

import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.swt.graphics.Image;
import org.kalypso.ui.ImageProvider;
import org.kalypso.ui.KalypsoGisPlugin;
import org.kalypsodeegree.model.feature.Annotation;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureTypeProperty;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree_impl.tools.GeometryUtilities;

/**
 * 
 * TODO: insert type comment here
 * 
 * @author kuepfer
 */
public class GMLEditorLabelProvider2 extends LabelProvider
{

  /*
   * (non-Javadoc)
   * 
   * @see org.eclipse.jface.viewers.ILabelProvider#getImage(java.lang.Object)
   */
  public Image getImage( Object element )
  {
    ImageDescriptor descriptor = null;
    if( element instanceof Feature )
      descriptor = ImageProvider.IMAGE_FEATURE;
    else if( element instanceof FeatureAssociationTypeElement )
      descriptor = ImageProvider.IMAGE_FEATURE_RELATION_COMPOSITION;
    else if( element instanceof LinkedFeatureElement2 )
      descriptor = ImageProvider.IMAGE_FEATURE_LINKED;
    else if( element instanceof FeatureTypeProperty )
    {
      if( GeometryUtilities.isPointGeometry( (FeatureTypeProperty)element ) )
        descriptor = ImageProvider.IMAGE_GEOM_PROP_POINT;
      if( GeometryUtilities.isMultiPointGeometry( (FeatureTypeProperty)element ) )
        descriptor = ImageProvider.IMAGE_GEOM_PROP_MULTIPOINT;
      if( GeometryUtilities.isLineStringGeometry( (FeatureTypeProperty)element ) )
        descriptor = ImageProvider.IMAGE_GEOM_PROP_LINE;
      if( GeometryUtilities.isMultiLineStringGeometry( (FeatureTypeProperty)element ) )
        descriptor = ImageProvider.IMAGE_GEOM_PROP_MULTILINE;
      if( GeometryUtilities.isPolygonGeometry( (FeatureTypeProperty)element ) )
        descriptor = ImageProvider.IMAGE_GEOM_PROP_POLYGON;
      if( GeometryUtilities.isMultiPolygonGeometry( (FeatureTypeProperty)element ) )
        descriptor = ImageProvider.IMAGE_GEOM_PROP_MULTIPOLYGON;
    }
    else
      return null;
    return descriptor.createImage();
  }

  /*
   * (non-Javadoc)
   * 
   * @see org.eclipse.jface.viewers.ILabelProvider#getText(java.lang.Object)
   */
  public String getText( Object element )
  {
    final String lang = KalypsoGisPlugin.getDefault().getLang();
    if( element instanceof GMLWorkspace )
      return "GML";
    if( element instanceof Feature )
    {
      final Feature feature = (Feature)element;
      final Annotation annotation = feature.getFeatureType().getAnnotation( lang );
      return annotation.getLabel() + " #" + feature.getId();
    }
    if( element instanceof FeatureAssociationTypeElement )
    {
      final Annotation annotation = ( (FeatureAssociationTypeElement)element ).getAssociationTypeProperty()
          .getAnnotation( lang );
      return annotation.getLabel();
    }
    if( element instanceof LinkedFeatureElement2 )
    {
      final Feature decoratedFeature = ( (LinkedFeatureElement2)element ).getDecoratedFeature();
      return "-> " + getText( decoratedFeature );
    }
    if( element instanceof FeatureTypeProperty )
    {
      FeatureTypeProperty ftp = (FeatureTypeProperty)element;
      return GeometryUtilities.getClass( ftp ).getName().replaceAll( ".+\\.", "" );

    }
    return "unknown";
  }
}
