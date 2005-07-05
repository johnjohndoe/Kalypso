/*--------------- Kalypso-Header --------------------------------------------------------------------

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
 
 ---------------------------------------------------------------------------------------------------*/
package org.kalypso.contribs.eclipse.jface.wizard;

import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.jface.viewers.ArrayContentProvider;
import org.eclipse.jface.viewers.CheckboxTableViewer;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.jface.viewers.StructuredSelection;
import org.eclipse.jface.wizard.WizardPage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;

/**
 * @author belger
 */
public class ArrayChooserPage extends WizardPage
{
  private final Object m_chooseables;

  private CheckboxTableViewer m_viewer;

  private Object[] m_selected;

  private Object[] m_checked;

  /**
   * @param chooseables
   *          Used as input for {@link ArrayContentProvider}
   * @param pageName
   * @param title
   * @param titleImage
   */
  public ArrayChooserPage( final Object chooseables, final String pageName, final String title,
      final ImageDescriptor titleImage )
  {
    this( chooseables, null, null, pageName, title, titleImage );
  }

  public ArrayChooserPage( final Object chooseables, final Object[] selected, final Object[] checked,
      final String pageName, final String title, final ImageDescriptor titleImage )
  {
    super( pageName, title, titleImage );

    m_chooseables = chooseables;
    m_selected = selected;
    m_checked = checked;
  }

  /**
   * @see org.eclipse.jface.dialogs.IDialogPage#dispose()
   */
  public void dispose()
  {
    super.dispose();
  }

  /**
   * @see org.eclipse.jface.dialogs.IDialogPage#createControl(org.eclipse.swt.widgets.Composite)
   */
  public void createControl( final Composite parent )
  {
    final Composite panel = new Composite( parent, SWT.NONE );
    panel.setLayout( new GridLayout() );

    m_viewer = CheckboxTableViewer.newCheckList( panel, SWT.BORDER );
    m_viewer.getTable().setLayoutData( new GridData( GridData.FILL_BOTH ) );
    m_viewer.setLabelProvider( new LabelProvider() );
    m_viewer.setContentProvider( new ArrayContentProvider() );
    m_viewer.setInput( m_chooseables );

    if( m_selected != null )
      m_viewer.setSelection( new StructuredSelection( m_selected ) );
    if( m_checked != null )
      m_viewer.setCheckedElements( m_checked );

    final Composite buttonpanel = new Composite( panel, SWT.RIGHT );
    buttonpanel.setLayout( new GridLayout( 2, true ) );
    final GridData buttonpaneldata = new GridData( GridData.HORIZONTAL_ALIGN_END | GridData.GRAB_HORIZONTAL );
    buttonpaneldata.grabExcessHorizontalSpace = true;
    buttonpanel.setData( buttonpaneldata );

    createSelectButton( buttonpanel, m_viewer, true );
    createSelectButton( buttonpanel, m_viewer, false );

    setControl( panel );
  }

  public Object[] getChoosen()
  {
    return m_viewer.getCheckedElements();
  }

  private static void createSelectButton( final Composite parent, final CheckboxTableViewer viewer, final boolean select )
  {
    final Button button = new Button( parent, SWT.PUSH );

    button.setLayoutData( new GridData( GridData.FILL_BOTH ) );
    button.setText( select ? "&Alle wählen" : "Alle a&bwählen" );

    button.addSelectionListener( new SelectionAdapter()
    {
      /**
       * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
       */
      public void widgetSelected( SelectionEvent e )
      {
        viewer.setAllChecked( select );
      }
    } );
  }

}