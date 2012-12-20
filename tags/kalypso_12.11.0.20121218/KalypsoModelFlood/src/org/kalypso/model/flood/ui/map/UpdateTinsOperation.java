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
package org.kalypso.model.flood.ui.map;

import java.lang.reflect.InvocationTargetException;
import java.net.URL;
import java.util.Date;
import java.util.Properties;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.SubMonitor;
import org.eclipse.core.runtime.SubProgressMonitor;
import org.kalypso.commons.command.EmptyCommand;
import org.kalypso.contribs.eclipse.jface.operation.ICoreRunnableWithProgress;
import org.kalypso.contribs.java.util.PropertiesUtilities;
import org.kalypso.gml.processes.tin.GmlTriangulatedSurfaceConverter;
import org.kalypso.gml.processes.tin.HmoTriangulatedSurfaceConverter;
import org.kalypso.gml.processes.tin.ShapeTriangulatedSurfaceConverter;
import org.kalypso.model.flood.binding.IFloodModel;
import org.kalypso.model.flood.binding.ITinReference;
import org.kalypso.model.flood.binding.ITinReference.SOURCETYPE;
import org.kalypso.model.flood.i18n.Messages;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.feature.event.FeaturesChangedModellEvent;
import org.kalypsodeegree.model.feature.event.ModellEvent;
import org.kalypsodeegree.model.geometry.GM_Exception;
import org.kalypsodeegree.model.geometry.GM_Triangle;
import org.kalypsodeegree.model.geometry.GM_TriangulatedSurface;
import org.kalypsodeegree.model.geometry.MinMaxSurfacePatchVisitor;
import org.kalypsodeegree_impl.model.feature.gmlxpath.GMLXPath;

import de.renew.workflow.connector.cases.IScenarioDataProvider;

/**
 * Updates the data of some tin-references. I.e. re-reads the original tin and copies the data into the reference.<br/>
 * TODO: It does not transform anything. The read data will be in the source coordinate system.
 *
 * @author Gernot Belger
 *
 */
public class UpdateTinsOperation implements ICoreRunnableWithProgress
{
  private final ITinReference[] m_tinReferences;

  private final IScenarioDataProvider m_provider;

  public UpdateTinsOperation( final ITinReference[] tinReferences, final IScenarioDataProvider provider )
  {
    m_tinReferences = tinReferences;
    m_provider = provider;
  }

  /**
   * @see org.kalypso.contribs.eclipse.jface.operation.ICoreRunnableWithProgress#execute(org.eclipse.core.runtime.IProgressMonitor)
   */
  @Override
  public IStatus execute( final IProgressMonitor monitor ) throws CoreException, InvocationTargetException
  {
    final SubMonitor progress = SubMonitor.convert( monitor, Messages.getString( "org.kalypso.model.flood.ui.map.UpdateTinsOperation.0" ), m_tinReferences.length ); //$NON-NLS-1$
    try
    {
      for( final ITinReference ref : m_tinReferences )
        updateTinReference( ref, progress.newChild( 1, SubMonitor.SUPPRESS_NONE ) );
    }
    catch( final CoreException ce )
    {
      throw ce;
    }
    catch( final Exception e )
    {
      throw new InvocationTargetException( e );
    }
    finally
    {
      progress.done();
    }

    return Status.OK_STATUS;
  }

  private IStatus updateTinReference( final ITinReference ref, final IProgressMonitor monitor ) throws Exception
  {
    /* Monitor. */
    monitor.beginTask( ref.getName(), 100 );
    monitor.subTask( Messages.getString( "org.kalypso.model.flood.ui.map.UpdateTinsOperation.1" ) ); //$NON-NLS-1$

    /* Read source data. */
    final Date sourceDate = new Date();
    final MinMaxSurfacePatchVisitor<GM_Triangle> minmaxVisitor = new MinMaxSurfacePatchVisitor<>();

    /* Get the source type. */
    final SOURCETYPE sourceType = ref.getSourceType();
    switch( sourceType )
    {
      case gml:
      {
        convertFromGml( ref, monitor, sourceDate, minmaxVisitor );
        break;
      }
      case hmo:
      {
        convertFromHmo( ref, monitor, sourceDate, minmaxVisitor );
        break;
      }
      case shape:
      {
        convertFromShape( ref, monitor, sourceDate, minmaxVisitor );
        break;
      }
    }

    /* Fire modell event as feature was changed. */
    final Feature refFeature = ref;
    final GMLWorkspace workspace = refFeature.getWorkspace();
    final ModellEvent event = new FeaturesChangedModellEvent( workspace, new Feature[] { refFeature } );
    workspace.fireModellEvent( event );

    /* Post command in order to make the pool dirty. */
    m_provider.postCommand( IFloodModel.class.getName(), new EmptyCommand( "Get dirty!", false ) ); //$NON-NLS-1$

    /* Monitor. */
    monitor.done();

    return Status.OK_STATUS;
  }

  private void convertFromGml( final ITinReference ref, final IProgressMonitor monitor, final Date sourceDate, final MinMaxSurfacePatchVisitor<GM_Triangle> minmaxVisitor ) throws CoreException, GM_Exception
  {
    /* Monitor. */
    monitor.subTask( Messages.getString( "org.kalypso.model.flood.ui.map.UpdateTinsOperation.5" ) ); //$NON-NLS-1$

    /* Reset tin to null before cloning the source in order to free some memory. */
    ref.setTin( null );

    /* Get some values of the reference. */
    final URL sourceLocation = ref.getSourceLocation();
    final GMLXPath sourcePath = ref.getSourceFeaturePath();

    /* Convert to triangulated surface. */
    final GmlTriangulatedSurfaceConverter converter = new GmlTriangulatedSurfaceConverter( sourcePath );
    final GM_TriangulatedSurface gmSurface = converter.convert( sourceLocation, new SubProgressMonitor( monitor, 50 ) );

    /* Monitor. */
    monitor.subTask( Messages.getString( "org.kalypso.model.flood.ui.map.UpdateTinsOperation.6" ) ); //$NON-NLS-1$

    /* Calculate some values. */
    final String desc = Messages.getString( "org.kalypso.model.flood.ui.map.UpdateTinsOperation.7", sourceLocation.toExternalForm(), sourcePath, sourceDate ); //$NON-NLS-1$
    gmSurface.acceptSurfacePatches( gmSurface.getEnvelope(), minmaxVisitor, new SubProgressMonitor( monitor, 50 ) );

    /* Update target data. */
    ref.setDescription( desc );
    ref.setMin( minmaxVisitor.getMin() );
    ref.setMax( minmaxVisitor.getMax() );
    ref.setUpdateDate( sourceDate );
    ref.setTin( gmSurface );
  }

  private void convertFromHmo( final ITinReference ref, final IProgressMonitor monitor, final Date sourceDate, final MinMaxSurfacePatchVisitor<GM_Triangle> minmaxVisitor ) throws CoreException, GM_Exception
  {
    /* Monitor. */
    monitor.subTask( Messages.getString( "org.kalypso.model.flood.ui.map.UpdateTinsOperation.5" ) ); //$NON-NLS-1$

    /* Get some values of the reference. */
    final URL sourceLocation = ref.getSourceLocation();

    /* Convert to triangulated surface. */
    final Properties properties = PropertiesUtilities.collectProperties( sourceLocation.getQuery(), "&", "=", null ); //$NON-NLS-1$ //$NON-NLS-2$
    final String sourceSrs = properties.getProperty( "srs" ); //$NON-NLS-1$
    final HmoTriangulatedSurfaceConverter converter = new HmoTriangulatedSurfaceConverter( sourceSrs );
    final GM_TriangulatedSurface gmSurface = converter.convert( sourceLocation, new SubProgressMonitor( monitor, 50 ) );

    /* Monitor. */
    monitor.subTask( Messages.getString( "org.kalypso.model.flood.ui.map.UpdateTinsOperation.13" ) ); //$NON-NLS-1$

    /* Calculate some values. */
    final String desc = Messages.getString( "org.kalypso.model.flood.ui.map.UpdateTinsOperation.14", sourceLocation.toExternalForm(), sourceDate ); //$NON-NLS-1$
    gmSurface.acceptSurfacePatches( gmSurface.getEnvelope(), minmaxVisitor, new SubProgressMonitor( monitor, 50 ) );

    /* Update target data. */
    ref.setDescription( desc );
    ref.setMin( minmaxVisitor.getMin() );
    ref.setMax( minmaxVisitor.getMax() );
    // TODO: check for right time zone?
    ref.setUpdateDate( sourceDate );
    ref.setTin( gmSurface );
  }

  private void convertFromShape( final ITinReference ref, final IProgressMonitor monitor, final Date sourceDate, final MinMaxSurfacePatchVisitor<GM_Triangle> minmaxVisitor ) throws CoreException, GM_Exception
  {
    /* Monitor. */
    monitor.subTask( Messages.getString( "org.kalypso.model.flood.ui.map.UpdateTinsOperation.5" ) ); //$NON-NLS-1$

    /* Get some values of the reference. */
    final URL sourceLocation = ref.getSourceLocation();

    /* Convert to triangulated surface. */
    final Properties properties = PropertiesUtilities.collectProperties( sourceLocation.getQuery(), "&", "=", null ); //$NON-NLS-1$ //$NON-NLS-2$
    final String sourceSrs = properties.getProperty( "srs" ); //$NON-NLS-1$
    final ShapeTriangulatedSurfaceConverter converter = new ShapeTriangulatedSurfaceConverter( sourceSrs );
    final GM_TriangulatedSurface gmSurface = converter.convert( sourceLocation, new SubProgressMonitor( monitor, 50 ) );

    /* Monitor. */
    monitor.subTask( Messages.getString( "org.kalypso.model.flood.ui.map.UpdateTinsOperation.18" ) ); //$NON-NLS-1$

    /* Calculate some values. */
    final String desc = Messages.getString( "org.kalypso.model.flood.ui.map.UpdateTinsOperation.19", sourceLocation.toExternalForm(), sourceDate ); //$NON-NLS-1$
    gmSurface.acceptSurfacePatches( gmSurface.getEnvelope(), minmaxVisitor, new SubProgressMonitor( monitor, 50 ) );

    /* Update target data. */
    ref.setDescription( desc );
    ref.setMin( minmaxVisitor.getMin() );
    ref.setMax( minmaxVisitor.getMax() );
    // TODO: check for right time zone?
    ref.setUpdateDate( sourceDate );
    ref.setTin( gmSurface );
  }
}