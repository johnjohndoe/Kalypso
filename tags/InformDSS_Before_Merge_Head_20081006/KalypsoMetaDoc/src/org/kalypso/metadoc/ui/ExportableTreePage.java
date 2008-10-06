/*--------------- Kalypso-Header ------------------------------------------

 This file is part of kalypso.
 Copyright (C) 2004, 2005 by:

 Technical University Hamburg-Harburg (TUHH)
 Institute of River and coastal engineering
 Denickestr. 22
 21073 Hamburg, Germany
 http://www.tuhh.de/wb

 and

 Bjoernsen Consulting Engineers (BCE)
 Maria Trost 3
 56070 Koblenz, Germany
 http://www.bjoernsen.de

 This library is free software; you can redistribute it and/or
 modify it under the terms of the GNU Lesser General Public
 License as published by the Free Software Foundation; either
 version 2.1 of the License, or (at your option) any later version.

 This library is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 Lesser General Public License for more details.

 You should have received a copy of the GNU Lesser General Public
 License along with this library; if not, write to the Free Software
 Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

 Contact:

 E-Mail:
 belger@bjoernsen.de
 schlienger@bjoernsen.de
 v.doemming@tuhh.de

 --------------------------------------------------------------------------*/

package org.kalypso.metadoc.ui;

import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.jface.viewers.CheckStateChangedEvent;
import org.eclipse.jface.viewers.CheckboxTreeViewer;
import org.eclipse.jface.viewers.ICheckStateListener;
import org.eclipse.jface.viewers.ITreeContentProvider;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Display;
import org.kalypso.contribs.eclipse.jface.wizard.TreeSelectionPage;

/**
 * @author Gernot Belger
 */
public class ExportableTreePage extends TreeSelectionPage
{
  public ExportableTreePage( String pageName )
  {
    super( pageName, new ExportableContentProvider(), new LabelProvider() );
  }

  public ExportableTreePage( String pageName, String title, ImageDescriptor titleImage )
  {
    super( pageName, title, titleImage, new ExportableContentProvider(), new LabelProvider() );
  }

  /**
   * @see org.kalypso.simulation.ui.wizards.exporter.featureWithTemplate.TreeSelectionPage#createControl(org.eclipse.swt.widgets.Composite)
   */
  @Override
  public void createControl( Composite parent )
  {
    super.createControl( parent );

    final Display display = getControl().getDisplay();

    final Font font = getFont();
    Font grayedFont = font;

    final CheckboxTreeViewer viewer = getViewer();

    // Change color of grayed elements
    final Color grayedForeground = display.getSystemColor( SWT.COLOR_GRAY );
    final Color grayedBackground = viewer.getControl().getBackground();
    ExportableLabelProvider labelProvider = new ExportableLabelProvider( grayedFont, grayedForeground, grayedBackground );

    viewer.setLabelProvider( labelProvider );

    // handle check state changes
    viewer.addCheckStateListener( new ICheckStateListener()
    {
      public void checkStateChanged( CheckStateChangedEvent event )
      {
        final Object element = event.getElement();
        final boolean checked = event.getChecked();

        viewer.setSubtreeChecked( element, checked );

        updateState( viewer );
      }
    } );
  }

  /**
   * @see org.kalypso.simulation.ui.wizards.exporter.featureWithTemplate.TreeSelectionPage#updateState(org.eclipse.jface.viewers.CheckboxTreeViewer)
   */
  @Override
  protected void updateState( CheckboxTreeViewer viewer )
  {
    // Uncheck all grayed elements
    ITreeContentProvider treeContentProvider = (ITreeContentProvider)viewer.getContentProvider();
    Object[] elements = treeContentProvider.getElements( viewer.getInput() );
    uncheckGrayed( viewer, treeContentProvider, elements );

    super.updateState( viewer );
  }

  /**
   * @param treeContentProvider
   * @param viewer
   * @param elements
   */
  private void uncheckGrayed( CheckboxTreeViewer viewer, ITreeContentProvider treeContentProvider, Object[] elements )
  {
    for( int i = 0; i < elements.length; i++ )
    {
      Object object = elements[i];
      if( viewer.getGrayed( object ) )
        viewer.setChecked( object, false );

      Object[] children = treeContentProvider.getChildren( object );
      uncheckGrayed( viewer, treeContentProvider, children );
    }
  }

  /**
   * Overwritten: page is only complete, if any 'real' exportable objects are selected.
   * 
   * @see org.eclipse.jface.wizard.WizardPage#isPageComplete()
   */
  @Override
  public boolean isPageComplete()
  {
    final boolean isPageComplete = super.isPageComplete();
    if( !isPageComplete )
      return false;

    final Object[] checkedElements = getCheckedElements();
    for( int i = 0; i < checkedElements.length; i++ )
    {
      ExportableTreeItem item = (ExportableTreeItem)checkedElements[i];
      if( item.getExportableObject() != null )
        return true;
    }

    return false;
  }
}
