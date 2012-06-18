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
package org.kalypso.model.hydrology.operation.hydrotope;

import java.lang.reflect.InvocationTargetException;
import java.util.Collection;
import java.util.List;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.SubMonitor;
import org.kalypso.contribs.eclipse.core.runtime.IStatusCollector;
import org.kalypso.contribs.eclipse.core.runtime.StatusCollector;
import org.kalypso.contribs.eclipse.jface.operation.ICoreRunnableWithProgress;
import org.kalypso.model.hydrology.binding.GeologyCollection;
import org.kalypso.model.hydrology.binding.HydrotopeCollection;
import org.kalypso.model.hydrology.binding.LanduseCollection;
import org.kalypso.model.hydrology.binding.OverlayCollection;
import org.kalypso.model.hydrology.binding.SoilTypeCollection;
import org.kalypso.model.hydrology.binding.model.NaModell;
import org.kalypso.model.hydrology.internal.ModelNA;
import org.kalypso.model.hydrology.internal.i18n.Messages;
import org.kalypsodeegree.model.geometry.GM_MultiSurface;

import com.vividsolutions.jts.geom.Polygon;

/**
 * Creates and writes hydrotops into a 'hydrotop.gml' file from 'modell.gml' (catchments), 'pedologie.gml',
 * 'geologie.gml' and 'landuse.gml'<br/>
 * FIXME: break this code into smaller chunks. Else, no one will every understand how it works....
 * 
 * @author Dejan Antanaskovic
 */
public class HydrotopeCreationOperation implements ICoreRunnableWithProgress
{
  private final LanduseCollection m_landuse;

  private final SoilTypeCollection m_pedology;

  private final GeologyCollection m_geology;

  private final OverlayCollection m_overlay;

  private final NaModell m_naModel;

  private final HydrotopeCollection m_hydrotopes;

  private GM_MultiSurface m_workingArea = null;

  public HydrotopeCreationOperation( final NaModell namodel, final LanduseCollection landuse, final SoilTypeCollection pedology, final GeologyCollection geology, final OverlayCollection overlay, final HydrotopeCollection hydrotopes, final GM_MultiSurface workingArea )
  {
    m_naModel = namodel;
    m_landuse = landuse;
    m_pedology = pedology;
    m_geology = geology;
    m_overlay = overlay;
    m_hydrotopes = hydrotopes;
    m_workingArea = workingArea;
  }

  @Override
  public IStatus execute( final IProgressMonitor monitor ) throws InvocationTargetException
  {
    final String taskName = Messages.getString( "org.kalypso.convert.namodel.hydrotope.HydrotopeCreationOperation.0" ); //$NON-NLS-1$
    final SubMonitor progress = SubMonitor.convert( monitor, taskName, 130 ); //$NON-NLS-1$3

    final IStatusCollector log = new StatusCollector( ModelNA.PLUGIN_ID );

    /* Build the indices */
    final String stepIndex = formatStep( 1, 6, Messages.getString("HydrotopeCreationOperation.0") ); //$NON-NLS-1$
    progress.setTaskName( stepIndex );
    final HydrotopeInputIndexer geometryIndexer = configureIndexer( stepIndex );
    final IStatus initStatus = geometryIndexer.execute( progress.newChild( 5 ) );
    log.add( initStatus );

    /* Input validation */
    final String stepValidate = formatStep( 2, 6, Messages.getString("HydrotopeCreationOperation.1") ); //$NON-NLS-1$
    progress.setTaskName( stepValidate );
    final IHydrotopeInput[] indices = geometryIndexer.getIndices();
    final HydrotopeCreationInputValidation geometryValidator = new HydrotopeCreationInputValidation( indices, stepValidate );
    final IStatus validationStatus = geometryValidator.execute( progress.newChild( 15 ) );
    log.add( validationStatus );
    if( validationStatus.matches( IStatus.ERROR ) )
    {
      log.add( IStatus.ERROR, Messages.getString("HydrotopeCreationOperation.2") ); //$NON-NLS-1$
      return log.asMultiStatusOrOK( taskName );
    }

    /* Geometry intersection */
    final String stepIntersect = formatStep( 3, 6, Messages.getString( "org.kalypso.convert.namodel.hydrotope.HydrotopeCreationOperation.1" ) ); //$NON-NLS-1$
    progress.setTaskName( stepIntersect );
    final FeatureListGeometryIntersector geometryIntersector = new FeatureListGeometryIntersector( indices, stepIntersect );
    final IStatus intersectorStatus = geometryIntersector.execute( progress.newChild( 50 ) );
    log.add( intersectorStatus );

    /* Dissolve */
    final String stepDissolve = formatStep( 4, 6, Messages.getString("HydrotopeCreationOperation.3") ); //$NON-NLS-1$
    progress.setTaskName( stepDissolve );
    final List<Polygon> intersectionList = geometryIntersector.getResult();
    final HydrotopeDissolver dissolver = new HydrotopeDissolver( intersectionList, stepDissolve );
    final IStatus dissolveStatus = dissolver.execute( progress.newChild( 20 ) );
    log.add( dissolveStatus );

    /* Build hydrotopes */
    final String stepBuildHydrotopes = formatStep( 5, 6, Messages.getString( "org.kalypso.convert.namodel.hydrotope.HydrotopeCreationOperation.2" ) ); //$NON-NLS-1$
    progress.setTaskName( stepBuildHydrotopes );
    final Collection<HydrotopeUserData> hydrotopeBeans = dissolver.getResult();
    final HydrotopeBuilder hydrotopeBuilder = new HydrotopeBuilder( hydrotopeBeans, m_hydrotopes, stepBuildHydrotopes );
    final IStatus buildStatus = hydrotopeBuilder.execute( progress.newChild( 5 ) );
    log.add( buildStatus );

    /* Validate hydrotopes */
    final String stepValidateHydrotopes = formatStep( 6, 6, Messages.getString("HydrotopeCreationOperation.4") ); //$NON-NLS-1$
    progress.setTaskName( stepValidateHydrotopes );
    final HydrotopeValidator hydrotopeValidator = new HydrotopeValidator( m_hydrotopes, m_naModel, stepValidateHydrotopes );
    final IStatus validationHydrotopesStatus = hydrotopeValidator.execute( progress.newChild( 5 ) );
    log.add( validationHydrotopesStatus );

    return log.asMultiStatus( taskName );
  }

  private String formatStep( final int step, final int stepCount, final String message )
  {
    return Messages.getString( "org.kalypso.convert.namodel.hydrotope.HydrotopeCreationOperation.5", step, stepCount, message ); //$NON-NLS-1$
  }

  private HydrotopeInputIndexer configureIndexer( final String logLabel )
  {
    final HydrotopeInputIndexer indexer = new HydrotopeInputIndexer( logLabel );

    if( m_workingArea == null )
    {
      indexer.addInput( new CatchmentHydrotopeInput( m_naModel ) );
      indexer.addInput( new LanduseHydrotopeInput( m_landuse ) );
      indexer.addInput( new PedologyHydrotopeInput( m_pedology ) );
      indexer.addInput( new GeologyHydrotopeInput( m_geology ) );
      indexer.addInput( new OverlayHydrotopeInput( m_overlay, m_naModel ) );
    }
    else
    {
      // FIXME: this does not work; if the planning area is present, hydrotope creation fails.
// final GM_Envelope envelope = m_workingArea.getEnvelope();
//
// final List<Feature> pedologyQuery = m_pedologyList.query( envelope, null );
// final List<Feature> geologyQuery = m_geologyList.query( envelope, null );
// final List<Feature> catchmentsQuery = m_catchmentsList.query( envelope, null );
// final List<Feature> landuseQuery = m_landuseList.query( envelope, null );
//
// geometryIntersector.addFeatureList( pedologyQuery );
// geometryIntersector.addFeatureList( geologyQuery );
// geometryIntersector.addFeatureList( catchmentsQuery );
// geometryIntersector.addFeatureList( landuseQuery );
      throw new UnsupportedOperationException();
    }

    return indexer;
  }
}