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
package org.kalypso.model.wspm.pdb.internal.wspm;

import java.net.URI;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.kalypso.contribs.eclipse.ui.progress.ProgressUtilities;
import org.kalypso.model.wspm.core.gml.IProfileFeature;
import org.kalypso.model.wspm.core.gml.WspmWaterBody;
import org.kalypso.model.wspm.core.profil.IProfile;
import org.kalypso.model.wspm.pdb.db.mapping.CrossSection;
import org.kalypso.model.wspm.pdb.db.mapping.State;
import org.kalypso.model.wspm.pdb.db.mapping.WaterBody;
import org.kalypso.model.wspm.pdb.internal.WspmPdbCorePlugin;
import org.kalypso.model.wspm.pdb.internal.i18n.Messages;
import org.kalypso.model.wspm.pdb.wspm.CheckoutDataMapping;
import org.kalypso.model.wspm.tuhh.core.IWspmTuhhConstants;
import org.kalypso.model.wspm.tuhh.core.gml.TuhhReach;
import org.kalypsodeegree_impl.model.geometry.JTSAdapter;

import com.vividsolutions.jts.geom.Geometry;

/**
 * @author Gernot Belger
 */
public class CheckoutCrossSectionsWorker
{
  private final CheckoutDataMapping m_mapping;

  private final URI m_documentBase;

  public CheckoutCrossSectionsWorker( final CheckoutDataMapping mapping, final URI documentBase )
  {
    m_mapping = mapping;
    m_documentBase = documentBase;
  }

  public void execute( final IProgressMonitor monitor ) throws CoreException
  {
    final CrossSection[] crossSections = m_mapping.getCrossSections();

    monitor.beginTask( Messages.getString( "CheckoutCrossSectionsWorker.0" ), crossSections.length ); //$NON-NLS-1$

    try
    {
      /* Convert the cross sections */
      for( final CrossSection crossSection : crossSections )
      {
        monitor.subTask( String.format( Messages.getString( "CheckoutCrossSectionsWorker.1" ), crossSection.getStation() ) ); //$NON-NLS-1$
        insert( crossSection );
        ProgressUtilities.worked( monitor, 1 );
      }
    }
    catch( final Exception e )
    {
      e.printStackTrace();
      final IStatus status = new Status( IStatus.ERROR, WspmPdbCorePlugin.PLUGIN_ID, "Should never happen", e ); //$NON-NLS-1$
      throw new CoreException( status );
    }
    finally
    {
      ProgressUtilities.done( monitor );
    }
  }

  public void insert( final CrossSection section ) throws Exception
  {
    final WaterBody waterBody = section.getWaterBody();
    final WspmWaterBody wspmWaterBody = m_mapping.getWspmWaterBody( waterBody );

    final State state = section.getState();
    final TuhhReach reach = m_mapping.getReach( state );

    final IProfileFeature profile = insertProfile( section, wspmWaterBody );
    reach.createProfileSegment( profile, profile.getStation() );
  }

  private IProfileFeature insertProfile( final CrossSection section, final WspmWaterBody waterBody )
  {
    final IProfileFeature newProfile = waterBody.createNewProfile();
    newProfile.setProfileType( IWspmTuhhConstants.PROFIL_TYPE_PASCHE );

    final Geometry line = section.getLine();
    final String srs = line == null ? null : JTSAdapter.toSrs( line.getSRID() );
    newProfile.setSrsName( srs );

    final IProfile profile = newProfile.getProfile();

    final CrossSectionConverter converter = new CrossSectionConverter( section, profile );
    profile.doTransaction( converter );

    final DocumentConverter documentConverter = new DocumentConverter( m_documentBase );
    documentConverter.convertDocuments( section, newProfile );

    return newProfile;
  }
}