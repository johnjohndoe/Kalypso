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
package org.kalypso.eclipse.jface.wizard;

import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.jface.viewers.ArrayContentProvider;
import org.eclipse.jface.viewers.CheckboxTableViewer;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.jface.wizard.WizardPage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Composite;

/**
 * @author belger
 */
public class ArrayChooserPage extends WizardPage
{
  private final Object m_chooseables;
  private CheckboxTableViewer m_viewer;

  /**
   * @param chooseables
   *          Used as input for {@link ArrayContentProvider}
   */
  public ArrayChooserPage( final Object chooseables, final String pageName, final String title,
      final ImageDescriptor titleImage )
  {
    super( pageName, title, titleImage );

    m_chooseables = chooseables;
  }

  /**
   * @see org.eclipse.jface.dialogs.IDialogPage#createControl(org.eclipse.swt.widgets.Composite)
   */
  public void createControl( final Composite parent )
  {
    m_viewer = CheckboxTableViewer.newCheckList( parent, SWT.BORDER );
    m_viewer.getTable().setLayoutData( new GridData( GridData.FILL_BOTH ) );
    m_viewer.setLabelProvider( new LabelProvider() );
    m_viewer.setContentProvider( new ArrayContentProvider() );
    m_viewer.setInput( m_chooseables );
    
    setControl( m_viewer.getTable() );
  }
  
  public Object[] getChoosen()
  {
    return m_viewer.getCheckedElements();
  }
}