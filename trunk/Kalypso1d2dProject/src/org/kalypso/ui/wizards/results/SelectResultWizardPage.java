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
package org.kalypso.ui.wizards.results;

import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.jface.viewers.CheckStateChangedEvent;
import org.eclipse.jface.viewers.CheckboxTreeViewer;
import org.eclipse.jface.viewers.ICheckStateListener;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.ITreeContentProvider;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.jface.viewers.ViewerComparator;
import org.eclipse.jface.viewers.ViewerFilter;
import org.eclipse.jface.wizard.IWizardPage;
import org.eclipse.jface.wizard.WizardPage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.ui.model.WorkbenchContentProvider;
import org.eclipse.ui.model.WorkbenchLabelProvider;
import org.kalypso.kalypsomodel1d2d.KalypsoModel1D2DPlugin;
import org.kalypso.kalypsomodel1d2d.ui.geolog.GeoLog;
import org.kalypso.kalypsomodel1d2d.ui.geolog.IGeoLog;
import org.kalypso.kalypsosimulationmodel.core.resultmeta.IResultMeta;
import org.kalypso.ui.wizards.i18n.Messages;

/**
 * Wizard page for displaying the result database in a checkbox-treeview Components are a {@link CheckboxTreeViewer} and
 * an {@link ResultMetaInfoViewer}. <br>
 * 
 * optional: The result info viewer can be given a {@link IThemeConstructionFactory} for displaying special button /
 * combo components for sld handling displayed inside the info viewer.
 * 
 * 
 * @author Thomas Jung
 */
public class SelectResultWizardPage extends WizardPage implements IWizardPage
{
  private final IThemeConstructionFactory m_factory;

  private final ViewerFilter m_filter;

  private IResultMeta m_resultRoot;

  private CheckboxTreeViewer m_treeViewer;

  private Object[] m_checkedElements = null;

  private final ViewerComparator m_comparator;

  protected IGeoLog m_geoLog;

  public SelectResultWizardPage( final String pageName, final String title, final ImageDescriptor titleImage, final ViewerFilter filter, final ViewerComparator comparator, final IThemeConstructionFactory factory, final IGeoLog geoLog )
  {
    super( pageName, title, titleImage );
    m_comparator = comparator;

    m_factory = factory;
    m_filter = filter;
    if( geoLog == null )
    {
      try
      {
        m_geoLog = new GeoLog( KalypsoModel1D2DPlugin.getDefault().getLog() );
      }
      catch( Exception e )
      {
        e.printStackTrace();
      }
    }
    else
    {
      m_geoLog = geoLog;
    }

    setDescription( Messages.getString( "org.kalypso.ui.wizards.results.SelectResultWizardPage.0" ) ); //$NON-NLS-1$
  }

  public void setResultMeta( final IResultMeta resultRoot )
  {
    m_resultRoot = resultRoot;

    if( m_treeViewer != null )
      m_treeViewer.setInput( resultRoot );
  }

  public IResultMeta getResultRoot( )
  {
    return m_resultRoot;
  }

  /**
   * @see org.eclipse.jface.dialogs.IDialogPage#createControl(org.eclipse.swt.widgets.Composite)
   */
  @Override
  public void createControl( final Composite parent )
  {
    /* set a fixed size to the Wizard */
    // HACK!!
    final Object layoutData = parent.getLayoutData();
    if( layoutData instanceof GridData )
    {
      final GridData pLayout = (GridData) layoutData;
      pLayout.widthHint = 700;
      pLayout.heightHint = 350;
      parent.layout();
    }
    final Composite panel = new Composite( parent, SWT.NONE );
    panel.setLayout( new FillLayout( SWT.HORIZONTAL ) );

    m_treeViewer = new CheckboxTreeViewer( panel, SWT.BORDER );
    m_treeViewer.setContentProvider( new WorkbenchContentProvider() );
    m_treeViewer.setLabelProvider( new WorkbenchLabelProvider() );
    m_treeViewer.addFilter( m_filter );
    m_treeViewer.setComparator( m_comparator );
    m_treeViewer.setInput( m_resultRoot );

    /* The next two lines are needed so that checking children of checked elements always works. */
    m_treeViewer.expandAll();
    m_treeViewer.collapseAll();

    /* Info View for one result */
    final ResultMetaInfoViewer resultViewer = new ResultMetaInfoViewer( panel, SWT.NONE, m_factory );

    m_treeViewer.addSelectionChangedListener( new ISelectionChangedListener()
    {
      @Override
      public void selectionChanged( final SelectionChangedEvent event )
      {
        handleSelectionChanged( (IStructuredSelection) event.getSelection(), resultViewer );
      }
    } );

    m_treeViewer.addCheckStateListener( new ICheckStateListener()
    {
      @Override
      public void checkStateChanged( final CheckStateChangedEvent event )
      {
        final IResultMeta resultMeta = (IResultMeta) event.getElement();
        final boolean isChecked = event.getChecked();
        handleCheckStateChanged( resultMeta, isChecked );
      }
    } );

    /* Check elements if any defined */
    if( m_checkedElements != null )
    {
      final ITreeContentProvider contentProvider = (ITreeContentProvider) m_treeViewer.getContentProvider();
      for( final Object elementToCheck : m_checkedElements )
      {
        final Object parentToExpand = contentProvider.getParent( elementToCheck );
        if( parentToExpand != null )
          m_treeViewer.expandToLevel( parentToExpand, 1 );
      }
      m_treeViewer.setCheckedElements( m_checkedElements );
    }

    setControl( panel );
  }

  protected void handleSelectionChanged( final IStructuredSelection selection, final ResultMetaInfoViewer resultViewer )
  {
    resultViewer.setInput( selection.getFirstElement() );
  }

  public IResultMeta[] getSelectedResults( )
  {
    final Object[] checkedElements = m_treeViewer.getCheckedElements();
    final IResultMeta[] resultArray = new IResultMeta[checkedElements.length];
    for( int i = 0; i < checkedElements.length; i++ )
      resultArray[i] = (IResultMeta) checkedElements[i];
    return resultArray;
  }

  protected void handleCheckStateChanged( final IResultMeta resultMeta, final boolean isChecked )
  {
    m_treeViewer.setSubtreeChecked( resultMeta, isChecked );
    getContainer().updateButtons();
  }

  public IThemeConstructionFactory getThemeFactory( )
  {
    return m_factory;
  }

  /**
   * The elements which should initally be checked.
   * <p>
   * This method must be called before createControl is invoked.
   * </p>
   */
  public void setInitialCheckedElements( final Object[] checkedElements )
  {
    m_checkedElements = checkedElements;
  }

  protected CheckboxTreeViewer getTreeViewer( )
  {
    return m_treeViewer;
  }
}
