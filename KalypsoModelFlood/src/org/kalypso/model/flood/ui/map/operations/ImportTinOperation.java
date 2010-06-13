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
package org.kalypso.model.flood.ui.map.operations;

import java.lang.reflect.InvocationTargetException;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.SubMonitor;
import org.eclipse.jface.dialogs.ErrorDialog;
import org.eclipse.swt.widgets.Shell;
import org.kalypso.afgui.scenarios.SzenarioDataProvider;
import org.kalypso.contribs.eclipse.ui.progress.ProgressUtilities;
import org.kalypso.core.KalypsoCorePlugin;
import org.kalypso.core.gml.provider.IGmlSource;
import org.kalypso.core.gml.provider.IGmlSourceRunnableWithProgress;
import org.kalypso.model.flood.binding.ITinReference;
import org.kalypso.model.flood.binding.ITinReference.SOURCETYPE;
import org.kalypso.model.flood.i18n.Messages;
import org.kalypso.model.flood.ui.map.UpdateTinsOperation;
import org.kalypso.ogc.gml.map.IMapPanel;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.feature.binding.IFeatureWrapperCollection;
import org.kalypsodeegree.model.feature.event.FeatureStructureChangeModellEvent;
import org.kalypsodeegree.model.feature.event.ModellEvent;
import org.kalypsodeegree.model.geometry.GM_Envelope;
import org.kalypsodeegree_impl.model.feature.FeatureHelper;
import org.kalypsodeegree_impl.tools.GeometryUtilities;

/**
 * Handles the import of a tins.
 * 
 * @author Gernot Belger
 */
public class ImportTinOperation implements IGmlSourceRunnableWithProgress
{
  private IGmlSource[] m_sources;

  private final IFeatureWrapperCollection<ITinReference> m_tins;

  private final SzenarioDataProvider m_provider;

  private final IMapPanel m_mapPanel;

  /**
   * @param mapPanel
   *            After importing, the exctent of this mapPanel will be set to the bounding box of the imported tins. May
   *            be <code>null</code>.
   */
  public ImportTinOperation( final SzenarioDataProvider provider, final IFeatureWrapperCollection<ITinReference> tins, final IMapPanel mapPanel )
  {
    m_provider = provider;
    m_tins = tins;
    m_mapPanel = mapPanel;
  }

  /**
   * @see org.kalypso.core.gml.provider.IGmlSourceRunnableWithProgress#setGmlSource(org.kalypso.core.gml.provider.IGmlSource[])
   */
  @Override
  public void setGmlSource( final IGmlSource[] sources )
  {
    m_sources = sources;
  }

  /**
   * @see org.kalypso.contribs.eclipse.jface.operation.ICoreRunnableWithProgress#execute(org.eclipse.core.runtime.IProgressMonitor)
   */
  @Override
  public IStatus execute( final IProgressMonitor monitor ) throws CoreException, InvocationTargetException
  {
    final SubMonitor progress = SubMonitor.convert( monitor, Messages.getString("org.kalypso.model.flood.ui.map.operations.ImportTinOperation.0"), 100 ); //$NON-NLS-1$

    /* Add sources as new tin references */
    progress.subTask( Messages.getString("org.kalypso.model.flood.ui.map.operations.ImportTinOperation.1") ); //$NON-NLS-1$
    final ITinReference[] tinRefs = new ITinReference[m_sources.length];
    final Feature[] changedFeatures = new Feature[m_sources.length];

    for( int i = 0; i < m_sources.length; i++ )
    {
      final IGmlSource source = m_sources[i];

      final ITinReference tinRef = m_tins.addNew( -1, ITinReference.QNAME, ITinReference.class );
      tinRef.setName( source.getName() );
      tinRef.setDescription( source.getDescription() );
      tinRef.setSourceLocation( source.getLocation() );
      tinRef.setSourceFeaturePath( source.getPath() );
      tinRef.setSourceType( typeForSource( source ) );

      tinRefs[i] = tinRef;
      changedFeatures[i] = tinRef.getFeature();
    }
    ProgressUtilities.worked( progress, 20 );

    /* post command for events stuff... */
    final Feature parentFeature = m_tins.getFeature();
    final GMLWorkspace workspace = parentFeature.getWorkspace();
    final ModellEvent modelEvent = new FeatureStructureChangeModellEvent( workspace, parentFeature, changedFeatures, FeatureStructureChangeModellEvent.STRUCTURE_CHANGE_ADD );
    workspace.fireModellEvent( modelEvent );
    /* Save data model */
    // progress.subTask( "speichere Datenmodell" );
    // m_provider.saveModel( IFloodModel.class, progress.newChild( 20 ) );
    /* update tins */
    progress.subTask( Messages.getString("org.kalypso.model.flood.ui.map.operations.ImportTinOperation.2") ); //$NON-NLS-1$
    final UpdateTinsOperation updateOp = new UpdateTinsOperation( tinRefs, m_provider );
    updateOp.execute( progress.newChild( 60 ) );

    /* Jump to imported tins */
    final GM_Envelope envelope = FeatureHelper.getEnvelope( changedFeatures );
    final GM_Envelope scaledBox = envelope == null ? null : GeometryUtilities.scaleEnvelope( envelope, 1.05 );
    if( m_mapPanel != null && scaledBox != null )
      m_mapPanel.setBoundingBox( scaledBox );

    return Status.OK_STATUS;
  }

  private SOURCETYPE typeForSource( IGmlSource source )
  {
    final String file = source.getLocation().getPath().toLowerCase();
    if( file.endsWith( ".hmo" ) ) //$NON-NLS-1$
      return SOURCETYPE.hmo;

    if( file.endsWith( ".gml" ) ) //$NON-NLS-1$
      return SOURCETYPE.gml;

    if( file.endsWith( ".shp" ) ) //$NON-NLS-1$
      return SOURCETYPE.shape;

    // In doubt, probably its an gml
    return SOURCETYPE.gml;
  }

  /**
   * @see org.kalypso.core.gml.provider.IGmlSourceRunnableWithProgress#handleResult(org.eclipse.swt.widgets.Shell,
   *      org.eclipse.core.runtime.IStatus)
   */
  @Override
  public boolean handleResult( final Shell shell, final IStatus resultStatus )
  {
    if( !resultStatus.isOK() )
      KalypsoCorePlugin.getDefault().getLog().log( resultStatus );
    ErrorDialog.openError( shell, Messages.getString("org.kalypso.model.flood.ui.map.operations.ImportTinOperation.6"), Messages.getString("org.kalypso.model.flood.ui.map.operations.ImportTinOperation.7"), resultStatus ); //$NON-NLS-1$ //$NON-NLS-2$
    return resultStatus.isOK();
  }
}
