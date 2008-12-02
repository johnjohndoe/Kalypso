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
package org.kalypso.risk.model.services;

import java.net.URL;
import java.util.HashMap;
import java.util.List;

import org.eclipse.core.expressions.IEvaluationContext;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.handlers.IHandlerService;
import org.kalypso.contribs.eclipse.core.resources.ResourceUtilities;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.kalypsosimulationmodel.utils.SLDHelper;
import org.kalypso.risk.Messages;
import org.kalypso.risk.model.schema.binding.ILanduseClass;
import org.kalypso.risk.model.schema.binding.ILandusePolygon;
import org.kalypso.risk.model.schema.binding.IRasterizationControlModel;
import org.kalypso.risk.model.schema.binding.IRiskZoneDefinition;
import org.kalypso.risk.plugin.RasterizedLanduseThemeInfo;
import org.kalypso.risk.plugin.RiskZonesThemeInfo;
import org.kalypso.ui.KalypsoGisPlugin;
import org.kalypso.util.pool.PoolableObjectType;
import org.kalypso.util.pool.ResourcePool;
import org.kalypsodeegree.model.feature.GMLWorkspace;

import de.renew.workflow.contexts.ICaseHandlingSourceProvider;

/**
 * @author Dejan Antanaskovic
 */
public class LanduseStyleUpdateService extends Job
{
  private final IFile m_dbFile;

  private final IFile m_landuseVectorSymbolyzerSldFile;

  private final IFile m_landuseRasterSymbolyzerSldFile;

  private final IFile m_riskZonesSymbolyzerSldFile;

  public LanduseStyleUpdateService( final IFile file )
  {
    super( Messages.getString("LanduseStyleUpdateService.0") ); //$NON-NLS-1$
    final IWorkbench workbench = PlatformUI.getWorkbench();
    final IHandlerService handlerService = (IHandlerService) workbench.getService( IHandlerService.class );
    final IEvaluationContext context = handlerService.getCurrentState();
    final IFolder scenarioFolder = (IFolder) context.getVariable( ICaseHandlingSourceProvider.ACTIVE_CASE_FOLDER_NAME );
    m_dbFile = file;
    m_landuseVectorSymbolyzerSldFile = scenarioFolder.getFile( "/styles/LanduseVector.sld" ); //$NON-NLS-1$
    m_landuseRasterSymbolyzerSldFile = scenarioFolder.getFile( "/styles/LanduseCoverage.sld" ); //$NON-NLS-1$
    m_riskZonesSymbolyzerSldFile = scenarioFolder.getFile( "/styles/RiskZonesCoverage.sld" ); //$NON-NLS-1$
  }

  /**
   * @see org.eclipse.core.runtime.jobs.Job#run(org.eclipse.core.runtime.IProgressMonitor)
   */
  @Override
  protected IStatus run( final IProgressMonitor monitor )
  {
    try
    {
      final URL databaseUrl = ResourceUtilities.createURL( m_dbFile );
      final PoolableObjectType poolKey = new PoolableObjectType( "gml", databaseUrl.toExternalForm(), databaseUrl ); //$NON-NLS-1$

      final ResourcePool pool = KalypsoGisPlugin.getDefault().getPool();
      GMLWorkspace workspace;

      synchronized( this )
      {
        do
        {
          Thread.sleep( 100 );
          workspace = (GMLWorkspace) pool.getObject( poolKey );
        }
        while( workspace == null );
      }
      final IRasterizationControlModel model = (IRasterizationControlModel) workspace.getRootFeature().getAdapter( IRasterizationControlModel.class );
      final List<ILanduseClass> landuseClassesList = model.getLanduseClassesList();
      if( landuseClassesList != null && landuseClassesList.size() > 0 )
      {
        SLDHelper.exportPolygonSymbolyzerSLD( m_landuseVectorSymbolyzerSldFile, model.getLanduseClassesList(), ILandusePolygon.PROPERTY_GEOMETRY, ILandusePolygon.PROPERTY_SLDSTYLE, null, null, monitor );
        SLDHelper.exportRasterSymbolyzerSLD( m_landuseRasterSymbolyzerSldFile, model.getLanduseClassesList(), null, null, monitor );
        final HashMap<Double, String> values = new HashMap<Double, String>();
        for( final ILanduseClass landuseClass : landuseClassesList )
          values.put( new Double( landuseClass.getOrdinalNumber() ), landuseClass.getName() );
        RasterizedLanduseThemeInfo.updateClassesDefinition( values );
      }
      final List<IRiskZoneDefinition> riskZonesList = model.getRiskZoneDefinitionsList();
      if( riskZonesList != null && riskZonesList.size() > 0 )
      {
        SLDHelper.exportRasterSymbolyzerSLD( m_riskZonesSymbolyzerSldFile, riskZonesList, null, null, monitor );
        final HashMap<Double, String> values = new HashMap<Double, String>();
        for( final IRiskZoneDefinition riskZoneDefinition : riskZonesList )
          values.put( new Double( riskZoneDefinition.getOrdinalNumber() ), riskZoneDefinition.getName() );
        RiskZonesThemeInfo.updateZonesDefinition( values );
      }
      return Status.OK_STATUS;
    }
    catch( final Throwable t )
    {
      return StatusUtilities.statusFromThrowable( t );
    }
  }

  public IFile getPolygonSymbolzerSldFile( )
  {
    return m_landuseVectorSymbolyzerSldFile;
  }

  public IFile getRasterSymbolzerSldFile( )
  {
    return m_landuseRasterSymbolyzerSldFile;
  }
}
