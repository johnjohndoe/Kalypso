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
package org.kalypso.model.rcm.util;

import java.net.URL;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.kalypso.contribs.eclipse.ui.progress.ProgressUtilities;
import org.kalypso.model.rcm.IRainfallModelProvider;
import org.kalypso.model.rcm.binding.IRainfallCatchmentModel;
import org.kalypso.model.rcm.internal.KalypsoModelRcmActivator;
import org.kalypso.model.rcm.internal.i18n.Messages;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypsodeegree.KalypsoDeegreePlugin;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree_impl.model.feature.visitors.TransformVisitor;

/**
 * An {@link IRainfallModelProvider} implementation that loads the model from a location ( {@link URL} ).
 *
 * @author Gernot Belger
 */
public class UrlRainfallModellProvider implements IRainfallModelProvider
{
  private final URL m_rcmLocation;

  public UrlRainfallModellProvider( final URL rcmLocation)
  {
    m_rcmLocation = rcmLocation;
  }

  @Override
  public IRainfallCatchmentModel getRainfallCatchmentModell( final IProgressMonitor monitor ) throws CoreException
  {
    monitor.beginTask( Messages.getString("UrlRainfallModellProvider_0"), 100 ); //$NON-NLS-1$

    IRainfallCatchmentModel rcm;
    try
    {
      final GMLWorkspace rcmWorkspace = GmlSerializer.createGMLWorkspace( m_rcmLocation, null );

      final Feature rootFeature = rcmWorkspace.getRootFeature();
      if( !(rootFeature instanceof IRainfallCatchmentModel) )
      {
        final String msg = String.format( "Root feature must be of type %s", IRainfallCatchmentModel.FEATURE_RAINFALL_CATCHMENT_MODEL ); //$NON-NLS-1$
        final IStatus status = new Status( IStatus.ERROR, KalypsoModelRcmActivator.PLUGIN_ID, msg );
        throw new CoreException( status );
      }

      rcm = (IRainfallCatchmentModel) rootFeature;

      // TODO: do we really need to do that? No geo-operations will be done
      final TransformVisitor transformVisitor = new TransformVisitor( KalypsoDeegreePlugin.getDefault().getCoordinateSystem() );
      rcmWorkspace.accept( transformVisitor, rootFeature, TransformVisitor.DEPTH_INFINITE );
    }
    catch( final Exception e )
    {
      e.printStackTrace();
      final IStatus status = new Status( IStatus.ERROR, KalypsoModelRcmActivator.PLUGIN_ID, Messages.getString("UrlRainfallModellProvider_2"), e ); //$NON-NLS-1$
      throw new CoreException( status );
    }
    finally
    {
      ProgressUtilities.done( monitor );
    }

    return rcm;
  }
}
