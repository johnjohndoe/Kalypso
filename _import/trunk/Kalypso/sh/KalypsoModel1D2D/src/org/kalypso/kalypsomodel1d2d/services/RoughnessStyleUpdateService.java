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
package org.kalypso.kalypsomodel1d2d.services;

import java.net.URL;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Path;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;
import org.kalypso.contribs.eclipse.core.resources.ResourceUtilities;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.kalypsosimulationmodel.core.roughness.IRoughnessClsCollection;
import org.kalypso.kalypsosimulationmodel.core.terrainmodel.IRoughnessPolygon;
import org.kalypso.kalypsosimulationmodel.utils.SLDHelper;
import org.kalypso.ui.KalypsoGisPlugin;
import org.kalypso.util.pool.PoolableObjectType;
import org.kalypso.util.pool.ResourcePool;
import org.kalypsodeegree.model.feature.GMLWorkspace;

/**
 * @author Dejan Antanaskovic
 * 
 */
public class RoughnessStyleUpdateService extends Job
{
  private static final IPath ROUGHNESS_SLD_PATH = new Path( ".metadata/roughness.sld" ); //$NON-NLS-1$     

  // if this STYLE_NAME is changed, it should be changed in all SLD layers in gmt files also
  private static final String STYLE_NAME = "Roughness style"; //$NON-NLS-1$     

  private static final String STYLE_TITLE = "Roughness style"; //$NON-NLS-1$     

  private final IFile m_roughnessDBFile;

  private final IFile m_sldFile;

  public RoughnessStyleUpdateService( final IFile file )
  {
    super( "Aktualisere Rauheiten-SLD Dienst" );
    m_roughnessDBFile = file;
    m_sldFile = m_roughnessDBFile.getProject().getFile( ROUGHNESS_SLD_PATH );
  }

  /**
   * @see org.eclipse.core.runtime.jobs.Job#run(org.eclipse.core.runtime.IProgressMonitor)
   */
  @Override
  protected IStatus run( final IProgressMonitor monitor )
  {
    try
    {
      final URL roughnessUrl = ResourceUtilities.createURL( m_roughnessDBFile );
      final PoolableObjectType poolKey = new PoolableObjectType( "gml", roughnessUrl.toExternalForm(), roughnessUrl );

      final ResourcePool pool = KalypsoGisPlugin.getDefault().getPool();
      GMLWorkspace roughnessWorkspace;

      // Here happens a NPE (roughnessWorkspace == null) after creating a new 1d2d model
      // reason: terrain model is still not loaded, so we will wait a bit...
      synchronized( this )
      {
        do
        {
          Thread.sleep( 100 );
          roughnessWorkspace = (GMLWorkspace) pool.getObject( poolKey );
        }
        while( roughnessWorkspace == null );
      }
      final IRoughnessClsCollection collection = (IRoughnessClsCollection) roughnessWorkspace.getRootFeature().getAdapter( IRoughnessClsCollection.class );
      SLDHelper.exportSLD( m_sldFile, collection, IRoughnessPolygon.PROP_GEOMETRY, IRoughnessPolygon.PROP_ROUGHNESS_STYLE, STYLE_NAME, STYLE_TITLE, monitor );
      return Status.OK_STATUS;
    }
    catch( final Throwable t )
    {
      return StatusUtilities.statusFromThrowable( t );
    }
  }

  public IFile getSldFile( )
  {
    return m_sldFile;
  }
}
