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

import java.util.ArrayList;
import java.util.Collection;

import org.eclipse.core.runtime.Assert;
import org.eclipse.jface.action.IAction;
import org.eclipse.jface.layout.GridLayoutFactory;
import org.eclipse.jface.viewers.CheckStateChangedEvent;
import org.eclipse.jface.viewers.CheckboxTreeViewer;
import org.eclipse.jface.viewers.ICheckStateListener;
import org.eclipse.jface.viewers.ViewerFilter;
import org.eclipse.jface.wizard.WizardPage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.kalypso.commons.databinding.jface.wizard.DatabindingWizardPage;
import org.kalypso.contribs.eclipse.jface.viewers.tree.CollapseAllTreeItemsAction;
import org.kalypso.contribs.eclipse.jface.viewers.tree.ExpandAllTreeItemsAction;
import org.kalypso.contribs.eclipse.jface.viewers.tree.ITreeViewerProvider;
import org.kalypso.kalypsosimulationmodel.core.resultmeta.IResultMeta;

/**
 * Wizard page for displaying the result database in a checkbox-treeview Components are a {@link CheckboxTreeViewer} and
 * an {@link ResultMetaInfoViewer}. <br>
 * optional: The result info viewer can be given a {@link IThemeConstructionFactory} for displaying special button /
 * combo components for sld handling displayed inside the info viewer.
 * 
 * @author Thomas Jung
 */
public class SelectResultWizardPage extends WizardPage implements ITreeViewerProvider
{
  private final SelectResultData m_data;

  // TODO: most use cases of result viewer only need the information about a result node, not creation of a theme. We should separate these concerns.
  private IResultControlFactory m_factory;

  private DatabindingWizardPage m_binding;

  private final SelectResultTreeComposite m_treeComposite;

  public SelectResultWizardPage( final String pageName, final String title, final SelectResultData data )
  {
    super( pageName );

    m_data = data;
    m_treeComposite = new SelectResultTreeComposite( m_data );

    setTitle( title );

    addAction( new CollapseAllTreeItemsAction( this ) );
    addAction( new ExpandAllTreeItemsAction( this ) );
    // separator
    addAction( null );
  }

  @Override
  public void dispose( )
  {
    if( m_binding != null )
      m_binding.dispose();

    m_treeComposite.dispose();

    super.dispose();
  }

  public void setFactory( final IResultControlFactory factory )
  {
    /* controls must not have been created */
    Assert.isTrue( m_binding == null );

    m_factory = factory;
  }

  public void setFilter( final ViewerFilter filter )
  {
    m_treeComposite.setFilter( filter );
  }

  /**
   * Adds an action to the toolbar. Must be called before {@link #createControl(Composite)} is called.
   */
  public final void addAction( final IAction action )
  {
    m_treeComposite.addAction( action );
  }

  @Override
  public void createControl( final Composite parent )
  {
    m_binding = new DatabindingWizardPage( this, null );

    final Composite panel = new Composite( parent, SWT.NONE );
    setControl( panel );

    GridLayoutFactory.swtDefaults().numColumns( 2 ).equalWidth( true ).applyTo( panel );

    /* tree panel */
    final Control treeControl = m_treeComposite.createControls( m_binding, panel, SWT.CHECK | SWT.BORDER );
    treeControl.setLayoutData( new GridData( SWT.FILL, SWT.FILL, true, true ) );
    final CheckboxTreeViewer treeViewer = (CheckboxTreeViewer)m_treeComposite.getTreeViewer();
    treeViewer.getControl().setFocus();

    /* Info View for one result */
    final ResultMetaInfoViewer resultViewer = new ResultMetaInfoViewer( m_factory, m_data );
    final Control resultControl = resultViewer.createControl( m_binding, panel );
    resultControl.setLayoutData( new GridData( SWT.FILL, SWT.FILL, true, true ) );

    // REMARK: necessary, else the minimum width of the tree control is not applied
    parent.layout();

    // FIXME: use checkstate provider and delegate behavior to various strategies that also validate the current check state
    treeViewer.addCheckStateListener( new ICheckStateListener()
    {
      @Override
      public void checkStateChanged( final CheckStateChangedEvent event )
      {
        final Object element = event.getElement();
        final boolean isChecked = event.getChecked();
        handleCheckStateChanged( treeViewer, element, isChecked );
      }
    } );
  }

  public IResultMeta[] getSelectedResults( )
  {
    final Object[] checkedElements = getTreeViewer().getCheckedElements();

    final Collection<IResultMeta> results = new ArrayList<>( checkedElements.length );
    for( final Object checkedElement : checkedElements )
    {
      if( checkedElement instanceof IResultMeta )
        results.add( (IResultMeta)checkedElement );
    }

    return results.toArray( new IResultMeta[results.size()] );
  }

  protected void handleCheckStateChanged( final CheckboxTreeViewer viewer, final Object element, final boolean isChecked )
  {
    viewer.setSubtreeChecked( element, isChecked );
    getContainer().updateButtons();
  }

  // TODO: not nice... currently used to refresh tree
  @Override
  public CheckboxTreeViewer getTreeViewer( )
  {
    return (CheckboxTreeViewer)m_treeComposite.getTreeViewer();
  }
}