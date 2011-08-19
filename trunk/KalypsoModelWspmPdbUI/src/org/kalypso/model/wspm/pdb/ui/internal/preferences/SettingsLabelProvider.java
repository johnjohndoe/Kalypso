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
package org.kalypso.model.wspm.pdb.ui.internal.preferences;

import org.apache.commons.lang3.ObjectUtils;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.jface.resource.ImageRegistry;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.swt.graphics.Image;
import org.kalypso.model.wspm.pdb.connect.IPdbSettings;

/**
 * @author Gernot Belger
 */
public class SettingsLabelProvider extends LabelProvider
{
  private final String m_format;

  private final ImageRegistry m_images = new ImageRegistry();

  public SettingsLabelProvider( final String format )
  {
    m_format = format;
  }

  @Override
  public void dispose( )
  {
    m_images.dispose();

    super.dispose();
  }

  @Override
  public String getText( final Object element )
  {
    if( element instanceof IPdbSettings )
    {
      final String label = ((IPdbSettings) element).getName();
      return String.format( m_format, label, element.toString() );
    }

    return super.getText( element );
  }

  @Override
  public Image getImage( final Object element )
  {
    if( element instanceof IPdbSettings )
    {
      final String key = ObjectUtils.identityToString( element );
      final Image image = m_images.get( key );
      if( image != null )
        return image;

      final ImageDescriptor descr = ((IPdbSettings) element).getImage();
      if( descr != null )
      {
        m_images.put( key, descr );
        return m_images.get( key );
      }
    }

    return super.getImage( element );
  }
}
