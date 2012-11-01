/** This file is part of Kalypso
 *
 *  Copyright (c) 2012 by
 *
 *  Björnsen Beratende Ingenieure GmbH, Koblenz, Germany (Bjoernsen Consulting Engineers), http://www.bjoernsen.de
 *  Technische Universität Hamburg-Harburg, Institut für Wasserbau, Hamburg, Germany
 *  (Technical University Hamburg-Harburg, Institute of River and Coastal Engineering), http://www.tu-harburg.de/wb/
 *
 *  Kalypso is free software: you can redistribute it and/or modify it under the terms
 *  of the GNU Lesser General Public License (LGPL) as published by the Free Software
 *  Foundation, either version 3 of the License, or (at your option) any later version.
 *
 *  Kalypso is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied
 *  warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public License for more details.
 *
 *  You should have received a copy of the GNU Lesser General Public
 *  License along with Kalypso.  If not, see <http://www.gnu.org/licenses/>.
 */
package org.kalypso.model.wspm.pdb.gaf;

import org.kalypso.model.wspm.pdb.internal.i18n.Messages;

/**
 * Teilprofiltyp, bekannt in GAF.<br/>
 * Entspricht CrossSectionPartType in der PDB, aber: die PDB muss nicht unbedingt zu GAF passen (d.h. kann andere Typen definieren).
 * 
 * @author Gernot Belger
 */
public enum GafKind
{
  P( Messages.getString( "GafKind.0" ) ), //$NON-NLS-1$
  S( Messages.getString( "GafKind.1" ) ), //$NON-NLS-1$
  W( Messages.getString( "GafKind.2" ) ), //$NON-NLS-1$
  A( Messages.getString( "GafKind.3" ) ), //$NON-NLS-1$
  UK( Messages.getString( "GafKind.4" ) ), //$NON-NLS-1$
  K( Messages.getString( "GafKind.5" ) ), //$NON-NLS-1$
  EI( Messages.getString( "GafKind.6" ) ), //$NON-NLS-1$
  MA( Messages.getString( "GafKind.7" ) ), //$NON-NLS-1$
  AR( Messages.getString( "GafKind.8" ) ), //$NON-NLS-1$
  HA( Messages.getString( "GafKind.9" ) ), //$NON-NLS-1$
  OK( Messages.getString( "GafKind.10" ) ); //$NON-NLS-1$

  private GafKind( final String label )
  {
    m_label = label;
  }

  private final String m_label;

  @Override
  public String toString( )
  {
    return m_label;
  }

  /**
   * Quietly parse kind, return <code>null</code> if given element is not of this enum.
   */
  public static GafKind quietValueOf( final String type )
  {
    if( type == null )
      return null;

    try
    {
      return valueOf( type );
    }
    catch( final IllegalArgumentException e )
    {
      return null;
    }
  }
}