/** This file is part of Kalypso
 *
 *  Copyright (c) 2012 by
 *
 *  Björnsen Beratende Ingenieure GmbH, Koblenz, Germany (Bjoernsen Consulting Engineers), http://www.bjoernsen.de
 *  Technische Universität Hamburg-Harburg, Institut für Wasserbau, Hamburg, Germany
 *  (Technical University Hamburg-Harburg, Institute of River and Coastal Engineering), http://www.tu-harburg.de/wb/
 *
 *  Kalypso is free software: you can redistribute it and/or modify it under the terms  
 *  of the GNU Lesser General Public License (LGPL) as published by the Free Software 
 *  Foundation, either version 3 of the License, or (at your option) any later version.
 *
 *  Kalypso is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied 
 *  warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public License for more details.
 *
 *  You should have received a copy of the GNU Lesser General Public
 *  License along with Kalypso.  If not, see <http://www.gnu.org/licenses/>.
 */
package org.kalypso.kalypso1d2d.pjt.wizards;

import org.eclipse.core.databinding.beans.BeansObservables;
import org.eclipse.core.databinding.observable.list.IObservableList;
import org.eclipse.core.databinding.observable.value.IObservableValue;
import org.eclipse.core.databinding.property.value.IValueProperty;
import org.eclipse.core.resources.IFolder;
import org.eclipse.jface.action.ToolBarManager;
import org.eclipse.jface.databinding.viewers.IViewerObservableValue;
import org.eclipse.jface.databinding.viewers.ViewerSupport;
import org.eclipse.jface.databinding.viewers.ViewersObservables;
import org.eclipse.jface.layout.GridLayoutFactory;
import org.eclipse.jface.viewers.ArrayContentProvider;
import org.eclipse.jface.viewers.DoubleClickEvent;
import org.eclipse.jface.viewers.IDoubleClickListener;
import org.eclipse.jface.viewers.TableViewer;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.ToolBar;
import org.kalypso.commons.databinding.IDataBinding;
import org.kalypso.commons.eclipse.core.runtime.PluginImageProvider;
import org.kalypso.contribs.eclipse.swt.widgets.ControlUtils;
import org.kalypso.kalypso1d2d.internal.i18n.Messages;
import org.kalypso.kalypsomodel1d2d.KalypsoModel1D2DPlugin;
import org.kalypso.kalypsomodel1d2d.KalypsoModel1D2DUIImages;
import org.kalypso.ui.wizards.results.SelectResultData.ShowType;

/**
 * @author Gernot Belger
 */
class RestartSorterComposite extends Composite
{
  private final RestartSelectData m_data;

  private final ToolBarManager m_toolbarManager = new ToolBarManager( SWT.VERTICAL | SWT.FLAT );

  private TableViewer m_viewer;

  public RestartSorterComposite( final IDataBinding binding, final Composite parent, final RestartSelectData data )
  {
    super( parent, SWT.NONE );

    m_data = data;

    GridLayoutFactory.swtDefaults().numColumns( 2 ).applyTo( this );

    final PluginImageProvider imageProvider = KalypsoModel1D2DPlugin.getImageProvider();

    final RestartSorterMoveAction moveUp = new RestartSorterMoveAction( data, -1 );
    moveUp.setImageDescriptor( imageProvider.getImageDescriptor( KalypsoModel1D2DUIImages.IMGKEY.RESTART_SORT_UP ) );

    final RestartSorterMoveAction moveDown = new RestartSorterMoveAction( data, +1 );
    moveDown.setImageDescriptor( imageProvider.getImageDescriptor( KalypsoModel1D2DUIImages.IMGKEY.RESTART_SORT_DOWN ) );

    m_toolbarManager.add( moveUp );
    m_toolbarManager.add( moveDown );

    createControls( binding, this );

    ControlUtils.addDisposeListener( parent );
  }

  @Override
  public void dispose( )
  {
    super.dispose();

    m_toolbarManager.dispose();
  }

  private void createControls( final IDataBinding binding, final Composite parent )
  {
    /* table */
    createTree( binding, parent ).setLayoutData( new GridData( SWT.FILL, SWT.FILL, true, true ) );

    /* toolbar */
    final ToolBar toolbar = m_toolbarManager.createControl( parent );
    toolbar.setLayoutData( new GridData( SWT.FILL, SWT.FILL, false, true ) );

    final Label description = new Label( parent, SWT.WRAP );
    description.setLayoutData( new GridData( SWT.FILL, SWT.FILL, true, false, 2, 1 ) );
    description.setText( Messages.getString( "org.kalypso.kalypso1d2d.pjt.wizards.RestartSelectWizardPage2.0" ) ); //$NON-NLS-1$
  }

  private Control createTree( final IDataBinding binding, final Composite parent )
  {
    m_viewer = new TableViewer( parent, SWT.BORDER | SWT.SINGLE | SWT.FULL_SELECTION );

    m_viewer.setContentProvider( new ArrayContentProvider() );

    /* bind */
    final IViewerObservableValue tableSelection = ViewersObservables.observeSinglePostSelection( m_viewer );
    final IObservableValue modelSelection = BeansObservables.observeValue( m_data, RestartSelectData.PROPERTY_SELECTED_RESTART );
    binding.bindValue( tableSelection, modelSelection );

    final IObservableList restartResults = m_data.getRestartResultSet();
    final IFolder currentScenario = m_data.getCurrentScenario();
    final IValueProperty labelProperty = new RestartSorterLabelProvider( currentScenario );
    ViewerSupport.bind( m_viewer, restartResults, labelProperty );

    /* remove action */
    m_viewer.addDoubleClickListener( new IDoubleClickListener()
    {
      @Override
      public void doubleClick( final DoubleClickEvent event )
      {
        treeDoubleClicked();
      }
    } );

    return m_viewer.getControl();
  }

  protected void treeDoubleClicked( )
  {
    final RestartElement currentResult = m_data.getSelectedRestart();
    if( currentResult == null )
      return;

    /* make sure, element can be selected (i.e. tree shows the right elements ) */
    final ShowType showType = m_data.findShowType( currentResult );
    if( showType.compareTo( m_data.getShowAllType() ) > 0 )
      m_data.setShowAllType( showType );

    /* now we can always select it */
    m_data.setTreeSelection( currentResult.getStepResult() );
  }
}