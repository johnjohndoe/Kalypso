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
package org.kalypso.model.wspm.pdb.internal.utils;

import org.apache.commons.lang3.StringUtils;
import org.eclipse.core.databinding.validation.ValidationStatus;
import org.eclipse.core.runtime.IStatus;
import org.kalypso.commons.databinding.validation.TypedValidator;

/**
 * @author Gernot Belger
 */
public class PortValidator extends TypedValidator<String>
{
  private final int m_defaultPort;

  public PortValidator( final int defaultPort )
  {
    super( String.class, IStatus.ERROR, "Invalid port" );

    m_defaultPort = defaultPort;
  }

  @Override
  protected IStatus doValidate( final String value )
  {
    if( StringUtils.isBlank( value ) )
      return ValidationStatus.info( String.format( "Using default port (%s)", m_defaultPort ) );

    try
    {
      final int port = Integer.parseInt( value );
      if( port <= 0 || port > 9999 )
        return ValidationStatus.error( "Port must be an integer between 1 and 9999" );

      return ValidationStatus.ok();
    }
    catch( final NumberFormatException e )
    {
      return ValidationStatus.error( "Port must be an integer" );
    }
  }
}