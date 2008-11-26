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
package org.kalypso.ui.editor.gmleditor.ui;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.jface.viewers.AbstractTreeViewer;
import org.eclipse.jface.viewers.IDoubleClickListener;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.jface.viewers.ViewerFilter;
import org.eclipse.jface.viewers.ViewerSorter;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.ui.forms.widgets.FormToolkit;
import org.eclipse.ui.progress.UIJob;
import org.kalypso.contribs.eclipse.core.resources.ResourceUtilities;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.core.KalypsoCorePlugin;
import org.kalypso.ogc.gml.GisTemplateHelper;
import org.kalypso.ogc.gml.selection.AbstractFeatureSelection;
import org.kalypso.template.gistreeview.Gistreeview;
import org.kalypso.ui.KalypsoGisPlugin;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.event.ModellEventListener;

/**
 * @author Dirk Kuch
 */
public class GftTreeViewer
{

  protected GmlTreeView m_treeViewer = null;

  private Gistreeview m_gistreeTemplate = null;

  protected Object[] m_selection;

  Set<Object> m_listenersToAdd = new HashSet<Object>();

  private ViewerSorter m_treeSorter = null;

  private final IFile m_gmvFile;

  public GftTreeViewer( final IFile gmvFile )
  {
    m_gmvFile = gmvFile;
  }

  public void addDoubleClickListener( final IDoubleClickListener listener )
  {
    if( m_treeViewer == null )
      m_listenersToAdd.add( listener );
    else
      m_treeViewer.getTreeViewer().addDoubleClickListener( listener );

  }

  public void addSelectionListener( final ISelectionChangedListener listener )
  {
    if( m_treeViewer == null )
      m_listenersToAdd.add( listener );
    else
      m_treeViewer.addSelectionChangedListener( listener );
  }

  public void addModelEventListener( final ModellEventListener listener )
  {
    m_treeViewer.addModellListener( listener );
  }

  public void addViewFilter( final ViewerFilter filter )
  {
    m_treeViewer.getTreeViewer().addFilter( filter );
  }

  public void destroy( )
  {
    if( m_treeViewer != null )
    {
      m_treeViewer.getTreeViewer().getTree().dispose();

      m_treeViewer.dispose();
      m_treeViewer = null;
    }

    m_gistreeTemplate = null;

  }

  public void draw( final FormToolkit toolkit, final Composite body, final GridData layout, final boolean border )
  {
    /* set layout of tree viewer */
    int flags = 0;
    if( border )
      flags = SWT.H_SCROLL | SWT.V_SCROLL | SWT.BORDER;
    else
      flags = SWT.H_SCROLL | SWT.V_SCROLL;

    draw( toolkit, body, layout, flags );
  }

  public void draw( final FormToolkit toolkit, final Composite body, final GridData layout, final int flags )
  {
    final Runnable objectLoadedCallback = new Runnable()
    {
      public void run( )
      {
        treeLoaded();
      }
    };

    final Composite treeBody = toolkit.createComposite( body );
    treeBody.setLayout( new FillLayout() );
    treeBody.setLayoutData( layout );

    m_treeViewer = new GmlTreeView( treeBody, flags, KalypsoCorePlugin.getDefault().getSelectionManager(), objectLoadedCallback );
    m_treeViewer.getTreeViewer().setAutoExpandLevel( AbstractTreeViewer.ALL_LEVELS );

    try
    {
      m_gistreeTemplate = GisTemplateHelper.loadGisTreeView( m_gmvFile );
      m_treeViewer.setInput( m_gistreeTemplate, ResourceUtilities.createURL( m_gmvFile ) );
    }
    catch( final Exception e )
    {
      KalypsoGisPlugin.getDefault().getLog().log( StatusUtilities.statusFromThrowable( e ) );
    }

    m_treeViewer.addSelectionChangedListener( new ISelectionChangedListener()
    {
      public void selectionChanged( final SelectionChangedEvent event )
      {
        final ISelection selection = m_treeViewer.getSelection();
        if( !(selection instanceof AbstractFeatureSelection) )
          return;

        final List<Object> selects = new ArrayList<Object>();

        final AbstractFeatureSelection featureSelection = (AbstractFeatureSelection) selection;
        for( final Object obj : featureSelection.toArray() )
          if( obj instanceof FeatureAssociationTypeElement )
            selects.add( obj );
          else if( obj instanceof Feature )
            selects.add( obj );
          else if( obj instanceof LinkedFeatureElement2 )
          {
            final LinkedFeatureElement2 lnk = (LinkedFeatureElement2) obj;
            final Feature decoratedFeature = lnk.getDecoratedFeature();
            selects.add( decoratedFeature );
          }

        m_selection = selects.toArray();
      }
    } );

    if( m_listenersToAdd.size() > 0 )
    {
      for( final Object listener : m_listenersToAdd )
        if( listener instanceof ISelectionChangedListener )
          m_treeViewer.addSelectionChangedListener( (ISelectionChangedListener) listener );
        else if( listener instanceof IDoubleClickListener )
          m_treeViewer.getTreeViewer().addDoubleClickListener( (IDoubleClickListener) listener );

      m_listenersToAdd.clear();
    }

    if( m_treeSorter != null )
    {
      m_treeViewer.getTreeViewer().setSorter( m_treeSorter );
      m_treeSorter = null;
    }
  }

  public Object[] getSelectedFeatures( )
  {
    if( m_selection == null )
      return new Object[] {};

    return m_selection;
  }

  public void refresh( )
  {
    m_treeViewer.getTreeViewer().refresh();
    m_treeViewer.getTreeViewer().getTree().update();
    m_treeViewer.getTreeViewer().getTree().layout();
  }

  public void setViewerSorter( final ViewerSorter sorter )
  {
    if( m_treeViewer != null )
      m_treeViewer.getTreeViewer().setSorter( sorter );
    else
      m_treeSorter = sorter;
  }

  protected void treeLoaded( )
  {
    if( m_treeViewer != null )
      m_treeViewer.getTreeViewer().getTree().getDisplay().asyncExec( new Runnable()
      {
        public void run( )
        {
          if( !m_treeViewer.getTreeViewer().getTree().isDisposed() )
          {
            m_treeViewer.getTreeViewer().expandAll();

            m_treeViewer.getTreeViewer().getTree().update();
            m_treeViewer.getTreeViewer().getTree().layout();
          }
        }
      } );
  }

  public void dispose( )
  {
    m_treeViewer.dispose();
    m_gistreeTemplate = null;

  }

  public void update( )
  {
    new UIJob( "" ) //$NON-NLS-1$
    {

      @Override
      public IStatus runInUIThread( final IProgressMonitor arg0 )
      {
        m_treeViewer.getTreeViewer().refresh();

        return org.eclipse.core.runtime.Status.OK_STATUS;
      }
    }.schedule();

  }

  public boolean isDisposed( )
  {
    return m_treeViewer.isDisposed();
  }
}
