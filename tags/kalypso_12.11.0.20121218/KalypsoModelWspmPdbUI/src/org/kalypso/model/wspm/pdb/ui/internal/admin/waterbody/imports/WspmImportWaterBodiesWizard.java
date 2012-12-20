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
package org.kalypso.model.wspm.pdb.ui.internal.admin.waterbody.imports;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.eclipse.core.databinding.observable.set.WritableSet;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.ui.IWorkbenchPart;
import org.kalypso.contribs.eclipse.jface.operation.ICoreRunnableWithProgress;
import org.kalypso.contribs.eclipse.jface.operation.RunnableContextHelper;
import org.kalypso.core.status.StatusDialog;
import org.kalypso.model.wspm.core.gml.WspmProject;
import org.kalypso.model.wspm.core.gml.WspmWaterBody;
import org.kalypso.model.wspm.pdb.db.mapping.WaterBody;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypso.ogc.gml.selection.IFeatureSelection;
import org.kalypso.ui.editor.gmleditor.part.FeatureAssociationTypeElement;
import org.kalypsodeegree.KalypsoDeegreePlugin;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.feature.IFeatureBindingCollection;
import org.kalypsodeegree_impl.model.geometry.JTSAdapter;

/**
 * A wizard for importing water bodies into wspm.
 *
 * @author Holger Albert
 */
public class WspmImportWaterBodiesWizard extends AbstractImportWaterBodiesWizard
{
  /**
   * The features of the existing water bodies.
   */
  private final Map<String, WspmWaterBody> m_features;

  private CommandableWorkspace m_workspace;

  private WspmProject m_wspmProject;

  public WspmImportWaterBodiesWizard( )
  {
    m_features = new HashMap<>();
    m_workspace = null;
    m_wspmProject = null;
  }

  @Override
  protected WaterBody[] initData( final IWorkbenchPart part, final IStructuredSelection selection )
  {
    final String kalypsoSRS = KalypsoDeegreePlugin.getDefault().getCoordinateSystem();
    getData().setTargetSRID( JTSAdapter.toSrid( kalypsoSRS ) );

    m_workspace = findWorkspace( selection );
    if( m_workspace == null )
      throw new IllegalStateException( "No commandable workspace was found..." ); //$NON-NLS-1$

    m_wspmProject = findWspmProject( selection );
    if( m_wspmProject == null )
      throw new IllegalStateException( "No WSPM project was found..." ); //$NON-NLS-1$

    int cnt = 0;
    final List<WaterBody> waterBodies = new ArrayList<>();
    final IFeatureBindingCollection<WspmWaterBody> wspmWaterBodies = m_wspmProject.getWaterBodies();
    for( final WspmWaterBody wspmWaterBody : wspmWaterBodies )
    {
      final String gkz = wspmWaterBody.getRefNr();
      final String name = wspmWaterBody.getName();

      final WaterBody waterBody = new WaterBody( cnt++, gkz, name, null );

      waterBodies.add( waterBody );
      m_features.put( gkz, wspmWaterBody );
    }

    return waterBodies.toArray( new WaterBody[] {} );
  }

  @Override
  public boolean performFinish( )
  {
    /* Get the import water bodies data. */
    final ImportWaterBodiesData data = getData();

    /* Get the water bodies. */
    final WritableSet selectedWaterBodies = data.getSelectedWaterBodies();
    final WaterBody[] waterBodies = (WaterBody[])selectedWaterBodies.toArray( new WaterBody[selectedWaterBodies.size()] );

    /* Create the operation. */
    final ICoreRunnableWithProgress operation = new WspmImportWaterBodiesOperation( m_workspace, waterBodies, data, m_wspmProject, m_features );

    /* Execute the operation. */
    final IStatus status = RunnableContextHelper.execute( getContainer(), true, false, operation );
    if( !status.isOK() )
    {
      final StatusDialog dialog = new StatusDialog( getShell(), status, getWindowTitle() );
      dialog.open();
    }

    return status.isOK();
  }

  private CommandableWorkspace findWorkspace( final IStructuredSelection selection )
  {
    if( !(selection instanceof IFeatureSelection) )
      return null;

    final IFeatureSelection featureSelection = (IFeatureSelection)selection;

    final Object[] elements = selection.toArray();
    for( final Object element : elements )
    {
      if( element instanceof FeatureAssociationTypeElement )
      {
        final FeatureAssociationTypeElement featureAssociationTypeElement = (FeatureAssociationTypeElement)element;
        final Feature feature = featureAssociationTypeElement.getOwner();
        final CommandableWorkspace workspace = featureSelection.getWorkspace( feature );
        if( workspace != null )
          return workspace;

      }

      if( element instanceof Feature )
      {
        final Feature feature = (Feature)element;
        final CommandableWorkspace workspace = featureSelection.getWorkspace( feature );
        if( workspace != null )
          return workspace;
      }
    }

    return null;
  }

  /**
   * This function inspects all elements in the selection and returns the WSPM project of first element which is a
   * feature of a WSPM project.
   *
   * @param selection
   *          The selection to inspect.
   * @return A WSPM project or null.
   */
  private WspmProject findWspmProject( final IStructuredSelection selection )
  {
    final Object[] elements = selection.toArray();
    for( final Object element : elements )
    {
      if( element instanceof FeatureAssociationTypeElement )
      {
        final FeatureAssociationTypeElement featureAssociationTypeElement = (FeatureAssociationTypeElement)element;
        final Feature feature = featureAssociationTypeElement.getOwner();
        final GMLWorkspace workspace = feature.getWorkspace();
        final Feature rootFeature = workspace.getRootFeature();
        if( rootFeature instanceof WspmProject )
          return (WspmProject)rootFeature;
      }

      if( element instanceof Feature )
      {
        final Feature feature = (Feature)element;
        final GMLWorkspace workspace = feature.getWorkspace();
        final Feature rootFeature = workspace.getRootFeature();
        if( rootFeature instanceof WspmProject )
          return (WspmProject)rootFeature;
      }
    }

    return null;
  }
}