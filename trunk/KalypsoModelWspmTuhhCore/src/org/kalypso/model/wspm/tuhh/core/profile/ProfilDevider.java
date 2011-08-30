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
package org.kalypso.model.wspm.tuhh.core.profile;

import org.kalypso.model.wspm.core.profil.impl.marker.PointMarker;
import org.kalypso.model.wspm.tuhh.core.IWspmTuhhConstants;
import org.kalypso.observation.result.IComponent;
import org.kalypso.observation.result.IRecord;

/**
 * @author kimwerner
 */
public class ProfilDevider extends PointMarker
{
  public ProfilDevider( final IComponent typ, final IRecord point )
  {
    super( typ, point );
  }

  /* Interpreted ui values to obtain backward compability */
  @Override
  public Object getIntepretedValue( )
  {
    if( IWspmTuhhConstants.MARKER_TYP_TRENNFLAECHE.equals( getComponent().getId() ) )
    {
      final Object value = getValue();

      if( value == null )
        return null;
      else if( "low".equals( value.toString().toLowerCase() ) ) //$NON-NLS-1$
        return false;
      else if( "high".equals( value.toString().toLowerCase() ) ) //$NON-NLS-1$
        return true;

      return null;
    }

    return getValue();
  }

  @Override
  public void setInterpretedValue( final Object value )
  {
    if( IWspmTuhhConstants.MARKER_TYP_TRENNFLAECHE.equals( getComponent().getId() ) )
    {
      if( value instanceof Boolean )
      {
        final Boolean flag = (Boolean) value;

        if( flag )
        {
          setValue( "high" ); //$NON-NLS-1$
        }
        else
        {
          setValue( "low" ); //$NON-NLS-1$
        }

        return;
      }

      throw new UnsupportedOperationException();
    }

    setValue( value );
  }

}
