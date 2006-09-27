/*--------------- Kalypso-Header --------------------------------------------------------------------

 This file is part of kalypso.
 Copyright (C) 2004, 2005 by:

 Technical University Hamburg-Harburg (TUHH)
 Institute of River and coastal engineering
 Denickestr. 22
 21073 Hamburg, Germany
 http://www.tuhh.de/wb

 and
 
 Bjoernsen Consulting Engineers (BCE)
 Maria Trost 3
 56070 Koblenz, Germany
 http://www.bjoernsen.de

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

 E-Mail:
 belger@bjoernsen.de
 schlienger@bjoernsen.de
 v.doemming@tuhh.de
 
 ---------------------------------------------------------------------------------------------------*/
package org.kalypso.ogc.gml.outline;

import org.eclipse.jface.viewers.ILabelProvider;
import org.eclipse.jface.viewers.ILabelProviderListener;
import org.eclipse.swt.graphics.Image;
import org.kalypso.ogc.gml.IKalypsoFeatureTheme;
import org.kalypso.ogc.gml.IKalypsoTheme;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypso.ogc.gml.mapmodel.IMapModell;
import org.kalypsodeegree.graphics.sld.UserStyle;
import org.kalypsodeegree.model.feature.FeatureList;

/**
 * @author bce
 */
public class MapModellLabelProvider implements ILabelProvider
{
  private IMapModell m_mapModell = null;

  public void setMapModell( IMapModell mapModell )
  {
    m_mapModell = mapModell;
  }

  /**
   * @see org.eclipse.jface.viewers.ILabelProvider#getImage(java.lang.Object)
   */
  public Image getImage( Object element )
  {
    return null;
    // ImageDescriptor descriptor = null;
    // descriptor = ImageProvider.IMAGE_FEATURE;
    // return descriptor.createImage();
  }

  /**
   * @see org.eclipse.jface.viewers.ILabelProvider#getText(java.lang.Object)
   */
  public String getText( final Object element )
  {
    if( element instanceof IKalypsoTheme )
    {
      final IKalypsoTheme kalypsoTheme = (IKalypsoTheme) element;
      final StringBuffer sb = new StringBuffer();
      final String type = kalypsoTheme.getType();
      final String themeName = kalypsoTheme.getName();
      if( type != null && type.length() > 0 )
        sb.append( "[" + type + "] " + themeName );
      else
        sb.append( themeName );

      // falls aktiviert
      if( m_mapModell != null && m_mapModell.getActiveTheme() == kalypsoTheme )
        sb.append( " - aktiv" );

      if( kalypsoTheme instanceof IKalypsoFeatureTheme )
      {
        final IKalypsoFeatureTheme kft = (IKalypsoFeatureTheme) kalypsoTheme;

        final CommandableWorkspace workspace = kft.getWorkspace();
        if( workspace == null )
          sb.append( " - loading..." );
        else
        {
          final FeatureList featureList = kft.getFeatureList();
          if( featureList != null )
            sb.append( "(" + featureList.size() + ")" );

          if( workspace.isDirty() )
            sb.append( '*' );
        }
      }

      return sb.toString();
    }

    if( element instanceof ThemeStyleTreeObject )
      return element.toString();

    if( element instanceof UserStyle )
      return ((UserStyle) element).getName();
    return element.toString();
  }

  /**
   * @see org.eclipse.jface.viewers.IBaseLabelProvider#addListener(org.eclipse.jface.viewers.ILabelProviderListener)
   */
  public void addListener( ILabelProviderListener listener )
  {
    // unsused
  }

  /**
   * @see org.eclipse.jface.viewers.IBaseLabelProvider#dispose()
   */
  public void dispose( )
  {
    // unused
  }

  /**
   * @see org.eclipse.jface.viewers.IBaseLabelProvider#isLabelProperty(java.lang.Object, java.lang.String)
   */
  public boolean isLabelProperty( Object element, String property )
  {
    return false;
  }

  /**
   * @see org.eclipse.jface.viewers.IBaseLabelProvider#removeListener(org.eclipse.jface.viewers.ILabelProviderListener)
   */
  public void removeListener( ILabelProviderListener listener )
  {
    // unused
  }
}
