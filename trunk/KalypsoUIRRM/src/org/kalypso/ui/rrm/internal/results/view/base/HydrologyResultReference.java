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
import org.kalypso.commons.java.net.UrlUtilities;
import org.kalypso.contribs.eclipse.core.resources.ResourceUtilities;
import org.kalypso.contribs.java.net.UrlResolverSingleton;
import org.kalypso.ui.rrm.internal.results.view.base.KalypsoHydrologyResults.RRM_RESULT;
import org.kalypso.ui.rrm.internal.results.view.base.KalypsoHydrologyResults.RRM_RESULT_TYPE;
import org.kalypso.zml.obslink.TimeseriesLinkType;
import org.kalypsodeegree.model.feature.Feature;

/**
 * @author Dirk Kuch
 */
public class HydrologyResultReference implements IHydrologyResultReference
{
  private final RRM_RESULT m_type;

  private final IFile m_file;

  public HydrologyResultReference( final IFolder calcCaseFolder, final Feature feature, final RRM_RESULT result )
  {
    final RRM_RESULT_TYPE type = result.getType();
    switch( type )
    {
    // FIXME i18n - english project template

      case eCatchment:
        m_file = calcCaseFolder.getFile( String.format( "/Teilgebiet/%s/%s", feature.getName(), result.getFileName() ) ); //$NON-NLS-1$
        break;

      case eNode:
        m_file = calcCaseFolder.getFile( String.format( "/Knoten/%s/%s", feature.getName(), result.getFileName() ) ); //$NON-NLS-1$
        break;

      case eStorage:
        m_file = calcCaseFolder.getFile( String.format( "/SpeicherStrang/%s/%s", feature.getName(), result.getFileName() ) ); //$NON-NLS-1$
        break;

      default:
        throw new UnsupportedOperationException();
    }

    m_type = result;
  }

  public HydrologyResultReference( final URL context, final TimeseriesLinkType link, final RRM_RESULT type ) throws MalformedURLException
  {
    final URL url = UrlResolverSingleton.resolveUrl( context, link.getHref() );

    m_file = ResourceUtilities.findFileFromURL( url );
    m_type = type;
  }

  @Override
  public Object getAdapter( final Class adapter )
  {
    if( adapter.isAssignableFrom( IHydrologyResultReference.class ) )
      return this;

    return null;
  }

  @Override
  public URL getUrl( ) throws MalformedURLException
  {
    return m_file.getLocationURI().toURL();
  }

  @Override
  public RRM_RESULT getType( )
  {
    return m_type;
  }

  @Override
  public boolean isValid( )
  {
    try
    {
      return UrlUtilities.checkIsAccessible( getUrl() );
    }
    catch( final MalformedURLException e )
    {
      e.printStackTrace();
    }

    return false;
  }
}
