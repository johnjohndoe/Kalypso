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
package org.kalypso.model.wspm.tuhh.ui.export.bankline;

import java.util.ArrayList;
import java.util.Collection;

import org.kalypso.model.wspm.tuhh.core.IWspmTuhhConstants;
import org.kalypso.model.wspm.tuhh.ui.i18n.Messages;

/**
 * Factory for available {@link IBanklineMarkerProvider}s.
 *
 * @author Gernot Belger
 */
public class BanklineMarkerProviderFactory
{
  private IBanklineMarkerProvider[] m_providers;

  // FIXME
  // TODO: get from extension point?
  public IBanklineMarkerProvider[] getAvailableProviders( )
  {
    if( m_providers == null )
      m_providers = createProviders();

    return m_providers;
  }

  private IBanklineMarkerProvider[] createProviders( )
  {
    final Collection<IBanklineMarkerProvider> providers = new ArrayList<>();

    final boolean checkMarkers = true;

    providers.add( new BanklineMarkerProvider( Messages.getString( "BanklineMarkerProviderFactory_0" ), IWspmTuhhConstants.MARKER_TYP_TRENNFLAECHE, null, checkMarkers ) ); //$NON-NLS-1$
    providers.add( new BanklineMarkerProvider( Messages.getString( "BanklineMarkerProviderFactory_1" ), IWspmTuhhConstants.MARKER_TYP_BORDVOLL, IWspmTuhhConstants.MARKER_TYP_TRENNFLAECHE, checkMarkers ) ); //$NON-NLS-1$
    providers.add( new BanklineMarkerProvider( Messages.getString( "BanklineMarkerProviderFactory_2" ), IWspmTuhhConstants.MARKER_TYP_DURCHSTROEMTE, null, checkMarkers ) ); //$NON-NLS-1$
    providers.add( new BanklineMarkerProvider( Messages.getString( "BanklineMarkerProviderFactory_3" ), BanklineMarkerProvider.PROFILE_START_END, null, checkMarkers ) ); //$NON-NLS-1$

    // TODO: add provider for all known codes
    // TODO: add provider for all currently available results

    return providers.toArray( new IBanklineMarkerProvider[providers.size()] );
  }

  public IBanklineMarkerProvider getProvider( final String providerId )
  {
    for( final IBanklineMarkerProvider provider : m_providers )
    {
      if( provider.getId().equals( providerId ) )
        return provider;
    }

    return null;
  }
}