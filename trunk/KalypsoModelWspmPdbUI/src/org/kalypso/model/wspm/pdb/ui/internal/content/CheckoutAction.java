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
package org.kalypso.model.wspm.pdb.ui.internal.content;

import java.net.URL;

import javax.xml.namespace.QName;

import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.jface.action.Action;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.IViewPart;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.PlatformUI;
import org.kalypso.contribs.eclipse.jface.operation.ICoreRunnableWithProgress;
import org.kalypso.contribs.eclipse.ui.progress.ProgressUtilities;
import org.kalypso.core.status.StatusDialog2;
import org.kalypso.model.wspm.pdb.ui.internal.WspmPdbUiImages;
import org.kalypso.model.wspm.pdb.ui.internal.wspm.PdbMapViewPart;
import org.kalypso.model.wspm.pdb.ui.internal.wspm.PdbWspmUtils;
import org.kalypso.model.wspm.pdb.ui.internal.wspm.WspmViewPart;
import org.kalypso.model.wspm.tuhh.core.gml.TuhhWspmProject;
import org.kalypso.ogc.gml.serialize.GmlSerializerFeatureProviderFactory;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree_impl.model.feature.FeatureFactory;

/**
 * @author Gernot Belger
 */
public class CheckoutAction extends Action
{
  private final ConnectionContentControl m_control;

  public CheckoutAction( final ConnectionContentControl control )
  {
    m_control = control;

    setText( "Checkout" );
    setToolTipText( "Load the selected profiles into the local workspace." );
    setImageDescriptor( WspmPdbUiImages.getImageDescriptor( WspmPdbUiImages.IMAGE.IMPORT ) );
  }

  @Override
  public void runWithEvent( final Event event )
  {
    final Shell shell = event.widget.getDisplay().getActiveShell();

    final IWorkbenchWindow window = PlatformUI.getWorkbench().getActiveWorkbenchWindow();
    final IWorkbenchPage page = window.getActivePage();

    final IStructuredSelection selection = m_control.getSelection();

    // FIXME: save changes in project (ask user!)
    // REMARK: project will be saved second time by the checkout operation

    final TuhhWspmProject project = findProject( page );

    final ICoreRunnableWithProgress operation = new CheckoutOperation( project, selection );
    final IStatus status = ProgressUtilities.busyCursorWhile( operation );
    if( !status.isOK() )
      new StatusDialog2( shell, status, getText() ).open();

    // reload to force reload of tree
    reloadWspmView( page );
    updateMap( page );
  }

  private TuhhWspmProject findProject( final IWorkbenchPage page )
  {
    final IViewPart view = page.findView( WspmViewPart.ID );
    if( view instanceof WspmViewPart )
    {
      final WspmViewPart wspmView = (WspmViewPart) view;
      final TuhhWspmProject project = wspmView.getProject();
      /* If project already exists, just return it */
      if( project != null )
        return project;
    }

    try
    {
      /* Project does not yet exist, create on from scratch in memory */
      final IPath modelLocation = PdbWspmUtils.getModelLocation();
      final URL modelURL = modelLocation.toFile().toURI().toURL();

      final QName rootQName = TuhhWspmProject.QNAME;
      final GMLWorkspace workspace = FeatureFactory.createGMLWorkspace( rootQName, modelURL, new GmlSerializerFeatureProviderFactory() );
      return (TuhhWspmProject) workspace.getRootFeature();
    }
    catch( final Exception e )
    {
      // should never happen
      e.printStackTrace();
      return null;
    }
  }

  private void reloadWspmView( final IWorkbenchPage page )
  {
    final IViewPart view = page.findView( WspmViewPart.ID );
    if( view instanceof WspmViewPart )
    {
      final WspmViewPart wspmView = (WspmViewPart) view;
      wspmView.reload();
    }
  }

  private void updateMap( final IWorkbenchPage page )
  {
    final IViewPart view = page.findView( PdbMapViewPart.ID );
    if( view instanceof PdbMapViewPart )
    {
      final PdbMapViewPart wspmView = (PdbMapViewPart) view;
// wspmView.updateMap();
    }
  }
}