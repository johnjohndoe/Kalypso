/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraße 22
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
package org.kalypso.model.wspm.tuhh.ui.resolutions;

import java.util.LinkedList;

import org.kalypso.model.wspm.core.profil.IProfil;
import org.kalypso.model.wspm.core.profil.IProfilChange;
import org.kalypso.model.wspm.core.profil.IProfilPoint;
import org.kalypso.model.wspm.core.profil.IProfilPoint.POINT_PROPERTY;
import org.kalypso.model.wspm.core.profil.changes.ActiveObjectEdit;
import org.kalypso.model.wspm.core.profil.changes.PointPropertyEdit;

/**
 * @author kimwerner
 */

public class EditPointResolution extends AbstractProfilMarkerResolution
{
  final private int m_index;

  final private POINT_PROPERTY m_property;

  final private double m_value;

  /**
   * verschieben der Trennfläche auf die Trenner "Durchströmter Bereich"
   * 
   * @param deviderTyp,deviderIndex
   *          devider=IProfil.getDevider(deviderTyp)[deviderIndex]
   */
  public EditPointResolution( final int index, final POINT_PROPERTY property, final double value )
  {
    super( "Ändern der Eigenschaft " + property.toString() + " auf einen gültigen Wert", null, null );
    m_index = index;
    m_property = property;
    m_value = value;
  }

  /**
   * @see org.kalypso.model.wspm.tuhh.ui.resolutions.AbstractProfilMarkerResolution#resolve(org.kalypso.model.wspm.core.profil.IProfil,
   *      org.eclipse.core.resources.IMarker)
   */
  @Override
  protected IProfilChange[] resolve( IProfil profil )
  {
    final LinkedList<IProfilPoint> points = profil.getPoints();
    if( points.isEmpty() )
    {
      return null;
    }
    final IProfilPoint point = points.get( m_index );
    if( point == null )
    {
      return null;
    }
    return new IProfilChange[] { new PointPropertyEdit( point, m_property, m_value ), new ActiveObjectEdit( profil, point, m_property ) };
  }
}
