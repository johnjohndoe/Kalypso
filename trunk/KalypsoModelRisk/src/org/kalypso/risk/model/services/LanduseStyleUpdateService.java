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

import java.awt.Color;
import java.net.URL;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.swt.graphics.RGB;
import org.kalypso.afgui.scenarios.ScenarioHelper;
import org.kalypso.contribs.eclipse.core.resources.ResourceUtilities;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.core.KalypsoCorePlugin;
import org.kalypso.core.util.pool.PoolableObjectType;
import org.kalypso.core.util.pool.ResourcePool;
import org.kalypso.kalypsosimulationmodel.utils.SLDHelper;
import org.kalypso.risk.i18n.Messages;
import org.kalypso.risk.model.schema.binding.ILanduseClass;
import org.kalypso.risk.model.schema.binding.ILandusePolygon;
import org.kalypso.risk.model.schema.binding.IRasterizationControlModel;
import org.kalypso.risk.model.schema.binding.IRiskZoneDefinition;
import org.kalypso.risk.plugin.RasterizedLanduseThemeInfo;
import org.kalypsodeegree.graphics.sld.ColorMapEntry;
import org.kalypsodeegree.graphics.sld.Layer;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree_impl.graphics.sld.ColorMapEntry_Impl;

/**
 * @author Dejan Antanaskovic
 */
public class LanduseStyleUpdateService extends Job
{
  private final IFile m_dbFile;

  private final IFile m_landuseVectorSymbolizerSldFile;

  private final IFile m_riskZonesSymbolizerSldFile;

  public LanduseStyleUpdateService( final IFile file )
  {
    super( Messages.getString( "org.kalypso.risk.model.services.LanduseStyleUpdateService.0" ) ); //$NON-NLS-1$

    final IFolder scenarioFolder = ScenarioHelper.getScenarioFolder();

    m_dbFile = file;
    m_landuseVectorSymbolizerSldFile = scenarioFolder.getFile( "/styles/LanduseVector.sld" ); //$NON-NLS-1$
    m_riskZonesSymbolizerSldFile = scenarioFolder.getFile( "/styles/RiskZonesCoverage.sld" ); //$NON-NLS-1$
  }

  @Override
  protected IStatus run( final IProgressMonitor monitor )
  {
    try
    {
      final URL databaseUrl = ResourceUtilities.createURL( m_dbFile );
      final PoolableObjectType poolKey = new PoolableObjectType( "gml", databaseUrl.toExternalForm(), databaseUrl ); //$NON-NLS-1$

      final ResourcePool pool = KalypsoCorePlugin.getDefault().getPool();
      final GMLWorkspace workspace = (GMLWorkspace)pool.getObject( poolKey );
      if( workspace == null )
        return Status.OK_STATUS;

      final IRasterizationControlModel model = (IRasterizationControlModel)workspace.getRootFeature().getAdapter( IRasterizationControlModel.class );
      final List<ILanduseClass> landuseClassesList = model.getLanduseClassesList();
      if( landuseClassesList != null && landuseClassesList.size() > 0 )
      {
        final List<Layer> layers = new ArrayList<>();
        layers.add( SLDHelper.polygonStyleLayer( null, model.getLanduseClassesList(), ILandusePolygon.PROPERTY_GEOMETRY, ILandusePolygon.PROPERTY_SLDSTYLE, null, null, monitor ) );

        SLDHelper.exportPolygonSymbolyzerSLD( m_landuseVectorSymbolizerSldFile, layers.toArray( new Layer[0] ), monitor );
        final HashMap<Double, String> values = new HashMap<>();
        for( final ILanduseClass landuseClass : landuseClassesList )
          values.put( new Double( landuseClass.getOrdinalNumber() ), landuseClass.getName() );
        RasterizedLanduseThemeInfo.updateClassesDefinition( values );
      }
      final List<IRiskZoneDefinition> riskZonesList = model.getRiskZoneDefinitionsList();
      final List<Double> urbanZonesBoundaryList = new ArrayList<>();

      for( final IRiskZoneDefinition riskZoneDefinition : riskZonesList )
      {
        if( riskZoneDefinition.isUrbanLanduseType() )
          urbanZonesBoundaryList.add( riskZoneDefinition.getLowerBoundary() );
      }

      urbanZonesBoundaryList.add( Double.MAX_VALUE );
      Collections.sort( urbanZonesBoundaryList );

      if( riskZonesList.size() > 0 )
      {
        final List<ColorMapEntry> riskZonesAdaptedList = new ArrayList<>();
        for( final IRiskZoneDefinition zoneDef : riskZonesList )
        {
          final double quantity;
          if( zoneDef.isUrbanLanduseType() )
            quantity = urbanZonesBoundaryList.get( urbanZonesBoundaryList.indexOf( zoneDef.getLowerBoundary() ) + 1 );
          else
            quantity = zoneDef.getLowerBoundary() > 0.0 ? -zoneDef.getLowerBoundary() : 0.0;

          final RGB rgb = zoneDef.getColorStyle();
          final Color color = rgb == null ? Color.WHITE : new Color( rgb.red, rgb.green, rgb.blue );
          riskZonesAdaptedList.add( new ColorMapEntry_Impl( color, 1.0, quantity, zoneDef.getName() ) );
        }
        SLDHelper.exportRasterSymbolyzerSLD( m_riskZonesSymbolizerSldFile, riskZonesAdaptedList, null, null, monitor );

        final Map<Double, String> values = new HashMap<>();
        for( final IRiskZoneDefinition riskZoneDefinition : riskZonesList )
        {
          final Double key = new Double( riskZoneDefinition.getLowerBoundary() );
          if( riskZoneDefinition.isUrbanLanduseType() )
            values.put( key, riskZoneDefinition.getName() );
          else
            values.put( -key, riskZoneDefinition.getName() );
        }

        // final List<ColorMapEntry> valueDefinitionList = new ArrayList<ColorMapEntry>();
        // for( final IRiskZoneDefinition zoneDef : riskZonesList )
        // {
        // final double quantity = zoneDef.isUrbanLanduseType() ? zoneDef.getLowerBoundary() :
        // -zoneDef.getLowerBoundary();
        // final RGB rgb = zoneDef.getColorStyle();
        // final Color color = rgb == null ? Color.WHITE : new Color( rgb.red, rgb.green, rgb.blue );
        // valueDefinitionList.add( new ColorMapEntry_Impl( color, 1.0, quantity, "" ) );
        // }
        // SLDHelper.exportRasterSymbolyzerSLD( m_riskValuesSymbolizerSldFile, valueDefinitionList, null, null, monitor
        // );
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
    return m_landuseVectorSymbolizerSldFile;
  }
}