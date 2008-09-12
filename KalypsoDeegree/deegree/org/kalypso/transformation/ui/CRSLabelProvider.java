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
package org.kalypso.transformation.ui;

import org.deegree.model.crs.CoordinateSystem;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.swt.graphics.Image;

/**
 * A label provider for coordinate systems.
 * 
 * @author Holger Albert
 */
public class CRSLabelProvider extends LabelProvider
{
  /**
   * If true, the EPSG code will be shown in brackets after the name of the coordinate systems.
   */
  private boolean m_showCode;

  /**
   * The constructor.
   * 
   * @param showCode
   *            If true, the EPSG code will be shown in brackets after the name of the coordinate systems.
   */
  public CRSLabelProvider( boolean showCode )
  {
    m_showCode = showCode;
  }

  /**
   * @see org.eclipse.jface.viewers.LabelProvider#getImage(java.lang.Object)
   */
  @Override
  public Image getImage( Object element )
  {
    return super.getImage( element );
  }

  /**
   * @see org.eclipse.jface.viewers.LabelProvider#getText(java.lang.Object)
   */
  @Override
  public String getText( Object element )
  {
    if( element instanceof CoordinateSystem )
    {
      CoordinateSystem crs = (CoordinateSystem) element;

      String name = crs.getCRS().getName();
      if( name == null )
        return crs.getCRS().getIdentifier();

      if( !m_showCode )
        return name;

      return name + " (" + crs.getCRS().getIdentifier() + ")";
    }

    return super.getText( element );
  }
}