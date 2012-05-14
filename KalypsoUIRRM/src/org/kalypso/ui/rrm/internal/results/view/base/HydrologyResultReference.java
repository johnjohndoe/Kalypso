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
package org.kalypso.ui.rrm.internal.results.view.base;

import java.net.MalformedURLException;
import java.net.URL;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IFolder;
import org.kalypso.model.hydrology.binding.model.Catchment;
import org.kalypso.model.hydrology.binding.model.channels.StorageChannel;
import org.kalypso.model.hydrology.binding.model.nodes.Node;

/**
 * @author Dirk Kuch
 */
public class HydrologyResultReference implements IHydrologyResultReference
{

  private final IFile m_file;

  public HydrologyResultReference( final IFolder calcCaseFolder, final StorageChannel channel, final String resultFileName )
  {
    m_file = calcCaseFolder.getFile( String.format( "Ergebnisse/Berechnet/SpeicherStrang/%s/%s", channel.getName(), resultFileName ) );
  }

  public HydrologyResultReference( final IFolder calcCaseFolder, final Catchment catchment, final String resultFileName )
  {
    m_file = calcCaseFolder.getFile( String.format( "Ergebnisse/Berechnet/Teilgebiet/%s/%s", catchment.getName(), resultFileName ) );
  }

  public HydrologyResultReference( final IFolder calcCaseFolder, final Node node, final String resultFileName )
  {
    m_file = calcCaseFolder.getFile( String.format( "Ergebnisse/Berechnet/Knoten/%s/%s", node.getName(), resultFileName ) );
  }

  @Override
  public Object getAdapter( final Class adapter )
  {
    if( adapter.isAssignableFrom( IHydrologyResultReference.class ) )
      return this;

    return null;
  }

  /**
   * @see org.kalypso.ui.rrm.internal.results.view.base.IHydrologyResultReference#getUrl()
   */
  @Override
  public URL getUrl( ) throws MalformedURLException
  {

    return m_file.getLocationURI().toURL();

  }

}
