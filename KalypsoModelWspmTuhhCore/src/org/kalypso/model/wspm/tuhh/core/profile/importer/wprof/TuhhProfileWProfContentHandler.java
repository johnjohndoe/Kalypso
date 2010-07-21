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
package org.kalypso.model.wspm.tuhh.core.profile.importer.wprof;

import java.util.LinkedHashMap;
import java.util.Map;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IStatus;
import org.kalypso.commons.command.EmptyCommand;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.gmlschema.GMLSchemaException;
import org.kalypso.model.wspm.core.gml.WspmWaterBody;
import org.kalypso.model.wspm.tuhh.core.gml.TuhhWspmProject;
import org.kalypso.model.wspm.tuhh.core.wprof.IWProfContentHandler;
import org.kalypso.model.wspm.tuhh.core.wprof.IWProfPoint;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypso.transformation.transformer.GeoTransformerFactory;
import org.kalypso.transformation.transformer.IGeoTransformer;
import org.kalypsodeegree.model.feature.event.FeatureStructureChangeModellEvent;

/**
 * @author Gernot Belger
 */
public class TuhhProfileWProfContentHandler implements IWProfContentHandler
{
  private final Map<String, ProfileData> m_data = new LinkedHashMap<String, ProfileData>();

  private final PunktattributMapping m_punktattribute = new PunktattributMapping();

  private final TuhhWspmProject m_project;

  private final IGeoTransformer m_transformer;

  private final CommandableWorkspace m_workspace;

  private IProfileCreatorStrategy m_strategy = new ProfileCreatorStrategy();

  private final String m_wprofPath;

  public TuhhProfileWProfContentHandler( final CommandableWorkspace workspace, final TuhhWspmProject project, final String targetSrs, final String wprofPath )
  {
    m_workspace = workspace;
    m_project = project;
    m_wprofPath = wprofPath;
    m_transformer = GeoTransformerFactory.getGeoTransformer( targetSrs );
  }

  public void addMarkerMapping( final String markerID, final int pointAttribute )
  {
    m_punktattribute.addAttributeMapping( markerID, pointAttribute );
  }

  @Override
  public void finished( ) throws CoreException
  {
    addProfiles( m_strategy );

    fireChangeEvents();
  }

  public void addProfiles( final IProfileCreatorStrategy strategy ) throws CoreException
  {
    final ProfileData[] data = m_data.values().toArray( new ProfileData[m_data.size()] );
    for( final ProfileData currentData : data )
    {
      final IProfileCreator profileCreator = strategy.createProfileCreator( currentData );
      currentData.createProfile( m_project, profileCreator );
    }

    final IProfileSecondaryCreator[] secondaryCreators = strategy.createSecondaryCreators( data );
    for( final IProfileSecondaryCreator secondaryCreator : secondaryCreators )
      secondaryCreator.execute( m_project, data );
  }

  private void fireChangeEvents( )
  {
    final WspmWaterBody[] waterBodies = m_project.getWaterBodies();

    m_workspace.fireModellEvent( new FeatureStructureChangeModellEvent( m_workspace, m_project, waterBodies, FeatureStructureChangeModellEvent.STRUCTURE_CHANGE_ADD ) );
    try
    {
      m_workspace.postCommand( new EmptyCommand( "", false ) ); //$NON-NLS-1$
    }
    catch( final Exception e )
    {
      // does not happen
      e.printStackTrace();
    }
  }

  /**
   * @see org.kalypso.gernot.tools.wprof.IWProfContentHandler#newPoint(org.kalypso.gernot.tools.wprof.IWProfPoint)
   */
  @Override
  public void newPoint( final IWProfPoint wprofPoint ) throws CoreException
  {
    try
    {
      final ProfileData data = getProfileData( wprofPoint );
      data.addPoint( wprofPoint );
    }
    catch( final GMLSchemaException e )
    {
      e.printStackTrace();

      throw new CoreException( StatusUtilities.createStatus( IStatus.ERROR, "Failed to create profile", e ) ); //$NON-NLS-1$
    }
    catch( final Exception e )
    {
      e.printStackTrace();
      throw new CoreException( StatusUtilities.createStatus( IStatus.ERROR, "Failed to add new point", e ) ); //$NON-NLS-1$
    }
  }

  private ProfileData getProfileData( final IWProfPoint wprofPoint )
  {
    final String pNam = wprofPoint.getPNam();

    if( !m_data.containsKey( pNam ) )
      m_data.put( pNam, new ProfileData( m_transformer, m_punktattribute, m_wprofPath ) );

    return m_data.get( pNam );
  }

  public void setStrategy( final IProfileCreatorStrategy strategy )
  {
    m_strategy = strategy;
  }

}
