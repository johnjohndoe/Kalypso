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
package org.kalypso.model.wspm.sobek.core.ui.lastfall;

import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.jface.viewers.TreePath;
import org.eclipse.jface.viewers.TreeSelection;
import org.eclipse.jface.viewers.TreeViewer;
import org.eclipse.jface.window.Window;
import org.eclipse.jface.wizard.WizardDialog;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.ToolBar;
import org.eclipse.swt.widgets.ToolItem;
import org.eclipse.ui.IWorkbenchWizard;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.forms.widgets.FormToolkit;
import org.kalypso.model.wspm.sobek.core.interfaces.IBoundaryNode;
import org.kalypso.model.wspm.sobek.core.interfaces.IBoundaryNodeLastfallCondition;
import org.kalypso.model.wspm.sobek.core.interfaces.ILastfall;
import org.kalypso.model.wspm.sobek.core.interfaces.ISobekModelMember;
import org.kalypso.model.wspm.sobek.core.model.LastfallUtils;
import org.kalypso.model.wspm.sobek.core.wizard.SobekWizardEditBoundaryCondition;
import org.kalypso.model.wspm.sobek.core.wizard.SobekWizardEditLastfall;
import org.kalypso.model.wspm.sobek.core.wizard.SobekWizardEditTimeSeriesObservation;
import org.kalypso.ogc.gml.FeatureUtils;

/**
 * @author kuch
 */
public class LastFallExplorer
{
  static private final Font fTextBold = new Font( Display.getDefault(), "Tahoma", 8, SWT.BOLD );

  protected final ISobekModelMember m_modelBuilder;

  public LastFallExplorer( final ISobekModelMember modelBuilder )
  {
    m_modelBuilder = modelBuilder;
  }

  public void draw( final FormToolkit toolkit, final Composite body )
  {
    if( m_modelBuilder == null )
      return;

    /* header */
    final Composite header = toolkit.createComposite( body );
    final GridLayout layout = new GridLayout( 2, false );
    layout.marginWidth = layout.horizontalSpacing = 0;
    header.setLayout( layout );
    header.setLayoutData( new GridData( GridData.FILL, GridData.FILL, true, false ) );

    final TreeViewer viewer = new TreeViewer( body );

    getToolbar( toolkit, header, viewer );

    /* tree */
    viewer.getTree().setLayoutData( new GridData( GridData.FILL, GridData.FILL, true, true ) );
    viewer.setLabelProvider( new LastfallTreeLabelProvider() );
    viewer.setContentProvider( new LastfallTreeContentProvider() );

    final ILastfall[] lastfalls = m_modelBuilder.getLastfallMembers();
    viewer.setInput( lastfalls );

    viewer.expandAll();
  }

  private void getToolbar( final FormToolkit toolkit, final Composite header, final TreeViewer viewer )
  {

    /* label */
    final Label lLastfalls = toolkit.createLabel( header, "Calculation Cases:" );
    lLastfalls.setFont( LastFallExplorer.fTextBold );

    final ToolBar toolBar = new ToolBar( header, SWT.FLAT );
    toolBar.setLayoutData( new GridData( GridData.END, GridData.FILL, true, false ) );
    toolkit.adapt( toolBar );

    /* create calculation case */
    final ToolItem createCC = new ToolItem( toolBar, SWT.NONE );
    final Image iAdd = new Image( header.getDisplay(), getClass().getResourceAsStream( "icons/add.gif" ) );
    createCC.setImage( iAdd );
    createCC.setToolTipText( "Create a new Calculation Case" );

    createCC.addSelectionListener( new SelectionAdapter()
    {
      /**
       * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
       */
      @Override
      public void widgetSelected( final SelectionEvent e )
      {
        final ILastfall lastfall = LastfallUtils.createEmptyLastfallFeature( m_modelBuilder );

        final IWorkbenchWizard wizard = new SobekWizardEditLastfall( lastfall );
        wizard.init( PlatformUI.getWorkbench(), null );

        final WizardDialog dialog = new WizardDialog( null, wizard );
        dialog.open();

        final int returnCode = dialog.getReturnCode();
        if( returnCode != Window.OK )
          try
          {
            FeatureUtils.deleteFeature( lastfall.getModelMember().getWorkspace(), lastfall.getFeature() );
          }
          catch( final Exception e1 )
          {
            e1.printStackTrace();
          }
      }
    } );

    new ToolItem( toolBar, SWT.NONE );// spacer

    /* edit */
    final ToolItem edit = new ToolItem( toolBar, SWT.NONE );
    final Image iEdit = new Image( header.getDisplay(), getClass().getResourceAsStream( "icons/edit.gif" ) );
    edit.setImage( iEdit );
    edit.setEnabled( false );

    edit.addSelectionListener( new SelectionAdapter()
    {
      /**
       * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
       */
      @Override
      public void widgetSelected( final SelectionEvent e )
      {
        final TreeSelection selection = (TreeSelection) viewer.getSelection();
        final Object element = selection.getFirstElement();

        if( element instanceof ILastfall )
        {
          final ILastfall lastfall = (ILastfall) element;

          final IWorkbenchWizard wizard = new SobekWizardEditLastfall( lastfall );
          wizard.init( PlatformUI.getWorkbench(), null );

          final WizardDialog dialog = new WizardDialog( null, wizard );
          dialog.open();
        }
        else if( element instanceof IBoundaryNode )
        {
          final TreePath[] path = selection.getPathsFor( element );
          if( path.length != 1 )
            throw new IllegalStateException( "Tree path is incorrect" );

          if( path[0].getSegmentCount() != 2 )
            throw new IllegalStateException( "Segment count of tree path is incorrect" );

          final ILastfall lastfall = (ILastfall) path[0].getFirstSegment();
          final IBoundaryNode node = (IBoundaryNode) element;

          final SobekWizardEditBoundaryCondition wizard = new SobekWizardEditBoundaryCondition( lastfall, node );
          wizard.init( PlatformUI.getWorkbench(), null );

          final WizardDialog dialog = new WizardDialog( null, wizard );
          dialog.open();
        }
      }
    } );

    /* edit time series observation */
    final ToolItem editObs = new ToolItem( toolBar, SWT.NONE );
    final Image iEditObs = new Image( header.getDisplay(), getClass().getResourceAsStream( "icons/edit_ts_obs.gif" ) );
    editObs.setImage( iEditObs );
    editObs.setEnabled( false );

    editObs.addSelectionListener( new SelectionAdapter()
    {
      /**
       * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
       */
      @Override
      public void widgetSelected( final SelectionEvent e )
      {
        final TreeSelection selection = (TreeSelection) viewer.getSelection();
        final TreePath[] path = selection.getPathsFor( selection.getFirstElement() );

        if( path.length != 1 || path[0].getSegmentCount() != 2 || !(path[0].getSegment( 0 ) instanceof ILastfall) || !(path[0].getSegment( 1 ) instanceof IBoundaryNode) )
          return;

        final ILastfall lastfall = (ILastfall) path[0].getSegment( 0 );
        final IBoundaryNode node = (IBoundaryNode) path[0].getSegment( 1 );

        try
        {
          final IBoundaryNodeLastfallCondition condition = node.getLastfallCondition( lastfall );

          if( condition.hasTimeSeriesObservation() )
          {
            final IWorkbenchWizard wizard = new SobekWizardEditTimeSeriesObservation( condition );
            wizard.init( PlatformUI.getWorkbench(), null );

            final WizardDialog dialog = new WizardDialog( null, wizard );
            dialog.open();
          }

        }
        catch( final Exception e1 )
        {
          e1.printStackTrace();
        }
      }
    } );

    /* delete */
    final ToolItem delete = new ToolItem( toolBar, SWT.NONE );
    final Image iDelete = new Image( header.getDisplay(), getClass().getResourceAsStream( "icons/delete.gif" ) );
    delete.setImage( iDelete );
    delete.setEnabled( false );

    delete.addSelectionListener( new SelectionAdapter()
    {
      /**
       * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
       */
      @Override
      public void widgetSelected( final SelectionEvent e )
      {
        final TreeSelection selection = (TreeSelection) viewer.getSelection();
        final Object element = selection.getFirstElement();

        if( element instanceof ILastfall )
        {
          final ILastfall lastfall = (ILastfall) element;
          if( MessageDialog.openConfirm( toolBar.getShell(), "Delete Lastfall", "Delete Lastfall: " + lastfall.getName() + "?" ) )
            try
            {
              FeatureUtils.deleteFeature( lastfall.getModelMember().getWorkspace(), lastfall.getFeature() );
            }
            catch( final Exception e1 )
            {
              e1.printStackTrace();
            }
        }
      }
    } );

    /* selection change listener for en/disabling toolitems */
    viewer.addSelectionChangedListener( new ISelectionChangedListener()
    {
      public void selectionChanged( final SelectionChangedEvent event )
      {
        final TreeSelection selection = (TreeSelection) viewer.getSelection();
        final Object element = selection.getFirstElement();

        if( element instanceof IBoundaryNode )
        {
          edit.setEnabled( true );
          delete.setEnabled( false );

          final TreePath[] path = selection.getPathsFor( element );
          if( hasTimeSeriesObservation( path ) )
            editObs.setEnabled( true );
          else
            editObs.setEnabled( false );

        }
        else if( element instanceof ILastfall )
        {
          edit.setEnabled( true );
          delete.setEnabled( true );
          editObs.setEnabled( false );
        }
        else
        {
          edit.setEnabled( false );
          delete.setEnabled( false );
          editObs.setEnabled( false );
        }
      }

      /**
       * @param path
       *            path[0] ILastfall, path[1] IBoundaryNode
       */
      private boolean hasTimeSeriesObservation( final TreePath[] path )
      {
        if( path.length != 1 || path[0].getSegmentCount() != 2 || !(path[0].getSegment( 0 ) instanceof ILastfall) || !(path[0].getSegment( 1 ) instanceof IBoundaryNode) )
          return false;

        final ILastfall lastfall = (ILastfall) path[0].getSegment( 0 );
        final IBoundaryNode node = (IBoundaryNode) path[0].getSegment( 1 );

        try
        {
          final IBoundaryNodeLastfallCondition condition = node.getLastfallCondition( lastfall );

          if( condition.hasTimeSeriesObservation() )
            return true;

        }
        catch( final Exception e )
        {
          return false;
        }

        return false;
      }
    } );

  }
}
