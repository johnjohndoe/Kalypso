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
package org.kalypso.model.wspm.core.profil.impl.points;

import org.kalypso.model.wspm.core.profil.IProfilPoint;
import org.kalypso.model.wspm.core.profil.IProfilPointProperty;

/**
 * @author kimwerner
 *
 */
public class PointProperty implements IProfilPointProperty
{

  final double m_precision;
  final String[] m_dependencies;
  final boolean m_optional;
  final String m_id;
  final String m_label;
  
  public PointProperty(final String id,final String label, final double precision, final String[] dependencies, final boolean optional )
  {
    m_precision = precision;
    m_dependencies = dependencies;
    m_optional = optional;
    m_id = id;
    m_label = label;
  }

  /**
   * @see org.kalypso.model.wspm.core.profil.IProfilPointProperty#doInterpolation(double, double)
   */
  public double doInterpolation( final IProfilPoint point1,final IProfilPoint point2 )
  {
    final double value1 = point1.getValueFor( m_id );
    final double value2 = point2.getValueFor( m_id );
    return (value1+value2)/2.0;
  }

  /**
   * @see org.kalypso.model.wspm.core.profil.IProfilPointProperty#getDependencies()
   */
  public String[] getDependencies( )
  {
    return m_dependencies;
  }

  /**
   * @see org.kalypso.model.wspm.core.profil.IProfilPointProperty#getPrecision()
   */
  public double getPrecision( )
  {
    return m_precision;
  }

  /**
   * @see org.kalypso.model.wspm.core.profil.IProfilPointProperty#isOptional()
   */
  public boolean isOptional( )
  {
    return m_optional;
  }
@Override
public String toString()
{
  return m_id;
}
  /**
   * @see org.kalypso.model.wspm.core.profil.IProfilPointProperty#getName()
   */
  public String getId( )
  {
    
    return m_id;
  }

  /**
   * @see org.kalypso.model.wspm.core.profil.IProfilPointProperty#getLabel()
   */
  public String getLabel( )
  {
        return m_label;
  }

}
