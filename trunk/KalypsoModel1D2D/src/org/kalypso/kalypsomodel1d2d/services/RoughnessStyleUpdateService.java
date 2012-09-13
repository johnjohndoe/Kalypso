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
import java.util.ArrayList;
import java.util.List;

import javax.xml.namespace.QName;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Path;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.ui.PlatformUI;
import org.kalypso.contribs.eclipse.core.resources.ResourceUtilities;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.core.KalypsoCorePlugin;
import org.kalypso.core.util.pool.PoolableObjectType;
import org.kalypso.core.util.pool.ResourcePool;
import org.kalypso.kalypsomodel1d2d.i18n.Messages;
import org.kalypso.kalypsosimulationmodel.core.roughness.IRoughnessClsCollection;
import org.kalypso.kalypsosimulationmodel.core.terrainmodel.IRoughnessPolygon;
import org.kalypso.kalypsosimulationmodel.utils.SLDHelper;
import org.kalypsodeegree.graphics.sld.Layer;
import org.kalypsodeegree.model.feature.GMLWorkspace;

/**
 * FIXME: the same sld file is used for rendering the rougness polygones as well as the 2d-elements.<br>
 * In order so that can work, the geometry name of both must be the same, but this should not be the case.<br>
 * Introducing a second virtual property on the 2d-elements is no solution, as then the 2d-element geometry is always
 * calculated twice, such having heavy impact on the overall performance!
 *
 * @author Dejan Antanaskovic
 */
public class RoughnessStyleUpdateService extends Job
{
  private static final IPath ROUGHNESS_SLD_PATH = new Path( ".metadata/roughness.sld" ); //$NON-NLS-1$

  private static final String POLYGON_LAYER_NAME = "Flow Resistance Class";  //$NON-NLS-1$

  // if this STYLE_NAME is changed, it should be changed in all SLD layers in gmt files also

  private static final String POLYGON_STYLE_NAME = "Roughness style"; //$NON-NLS-1$

  private static final String POLYGON_STYLE_TITLE = "Flow Resistance Class"; //$NON-NLS-1$

  private final IFile m_roughnessDBFile;

  private final IFile m_sldFile;

  public RoughnessStyleUpdateService( final IFile file )
  {
    super( Messages.getString( "org.kalypso.kalypsomodel1d2d.services.RoughnessStyleUpdateService.0" ) ); //$NON-NLS-1$
    m_roughnessDBFile = file;
    m_sldFile = m_roughnessDBFile.getProject().getFile( ROUGHNESS_SLD_PATH );
  }

  @Override
  protected IStatus run( final IProgressMonitor monitor )
  {
    if( PlatformUI.getWorkbench().isClosing() )
      return Status.OK_STATUS;
    if( PlatformUI.getWorkbench().isClosing() )
      return Status.OK_STATUS;

    try
    {
      final URL roughnessUrl = ResourceUtilities.createURL( m_roughnessDBFile );
      final PoolableObjectType poolKey = new PoolableObjectType( "gml", roughnessUrl.toExternalForm(), roughnessUrl ); //$NON-NLS-1$

      final ResourcePool pool = KalypsoCorePlugin.getDefault().getPool();
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
      // TODO: change name of property here
      // either write the sld twice, for polygons and 2d-element
      // or don't specify the geometry, using the default one.
      // Now trying to omit geometry: check if it works
      final QName geomPropertyName = null; // IRoughnessPolygon.PROP_GEOM OR IElement2D.QNAME_PROP_GEOMETRY

      final IRoughnessClsCollection collection = (IRoughnessClsCollection) roughnessWorkspace.getRootFeature().getAdapter( IRoughnessClsCollection.class );

      final List<Layer> layers = new ArrayList<>();
      layers.add( SLDHelper.polygonStyleLayer( POLYGON_LAYER_NAME, collection.getRoughnessClasses(), geomPropertyName, IRoughnessPolygon.PROP_ROUGHNESS_STYLE, POLYGON_STYLE_NAME, POLYGON_STYLE_TITLE, monitor ) );

      // TODO: make optional via preferences?
      // layers.add( SLDHelper.textlabelStyleLayer( LABEL_LAYER_NAME, geomPropertyName,
      // IRoughnessPolygon.PROP_ROUGHNESS_STYLE, LABEL_STYLE_NAME, LABEL_STYLE_TITLE ) );

      SLDHelper.exportPolygonSymbolyzerSLD( m_sldFile, layers.toArray(new Layer[0]), monitor );
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
